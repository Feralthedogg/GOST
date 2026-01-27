use std::collections::HashMap;

use crate::frontend::ast::{Block, Expr, ExprId, ExprKind, Stmt, TypeAst};
use crate::mir::{MirStmt, Terminator};
use crate::sema::FunctionSig;
use crate::sema::types::{builtin_from_name, BuiltinType, Type, TypeClass, TypeDefKind, TypeDefs};

use super::{
    is_float_type, llvm_storage_type, llvm_type, llvm_type_for_tuple_elem, zero_value,
};

#[derive(Clone)]
pub(crate) struct Value {
    ty: Type,
    ir: String,
}

pub(crate) struct BlockInsts {
    pub(crate) name: String,
    pub(crate) instrs: Vec<String>,
    pub(crate) terminated: bool,
}

#[derive(Clone)]
struct DeferredCall {
    name: String,
    type_args: Vec<Type>,
    args: Vec<(Type, String)>,
}

#[derive(Clone)]
struct MirLocalInfo {
    name: Option<String>,
    ty: Type,
    ptr: String,
}

pub(crate) struct FnEmitter<'a> {
    fn_name: String,
    fn_sigs: &'a HashMap<String, FunctionSig>,
    types: &'a TypeDefs,
    ret_type: Type,
    pub(crate) blocks: Vec<BlockInsts>,
    current: usize,
    temp_counter: usize,
    block_counter: usize,
    locals: HashMap<String, (Type, String)>,
    scopes: Vec<Vec<String>>,
    pub(crate) string_literals: Vec<String>,
    string_count: usize,
    deferred: Vec<Vec<DeferredCall>>,
    loop_stack: Vec<(String, String)>,
    pub(crate) extra_functions: Vec<String>,
    go_counter: usize,
    drop_counter: usize,
    linear_states: HashMap<String, bool>,
    mir_mode: bool,
    mir_local_ptrs: Vec<Option<MirLocalInfo>>,
    mir_expr_types: HashMap<ExprId, Type>,
}

impl<'a> FnEmitter<'a> {
    pub(crate) fn new(
        fn_name: impl Into<String>,
        fn_sigs: &'a HashMap<String, FunctionSig>,
        types: &'a TypeDefs,
        ret_type: Type,
    ) -> Self {
        let entry = BlockInsts {
            name: "entry".to_string(),
            instrs: Vec::new(),
            terminated: false,
        };
        Self {
            fn_name: fn_name.into(),
            fn_sigs,
            types,
            ret_type,
            blocks: vec![entry],
            current: 0,
            temp_counter: 0,
            block_counter: 0,
            locals: HashMap::new(),
            scopes: vec![Vec::new()],
            string_literals: Vec::new(),
            string_count: 0,
            deferred: vec![Vec::new()],
            loop_stack: Vec::new(),
            extra_functions: Vec::new(),
            go_counter: 0,
            drop_counter: 0,
            linear_states: HashMap::new(),
            mir_mode: false,
            mir_local_ptrs: Vec::new(),
            mir_expr_types: HashMap::new(),
        }
    }

    fn emit(&mut self, instr: impl Into<String>) {
        let instr = instr.into();
        if instr.contains(" = alloca ") {
            self.emit_in_entry(instr);
            return;
        }
        let block = &mut self.blocks[self.current];
        if !block.terminated {
            block.instrs.push(instr);
        }
    }

    pub(crate) fn emit_raw(&mut self, instr: impl Into<String>) {
        self.emit(instr);
    }

    fn emit_in_entry(&mut self, instr: String) {
        let entry = &mut self.blocks[0];
        if entry.terminated {
            let len = entry.instrs.len();
            if len == 0 {
                entry.instrs.push(instr);
            } else {
                entry.instrs.insert(len - 1, instr);
            }
        } else {
            entry.instrs.push(instr);
        }
    }

    fn terminate(&mut self, instr: impl Into<String>) {
        let block = &mut self.blocks[self.current];
        if !block.terminated {
            block.instrs.push(instr.into());
            block.terminated = true;
        }
    }

    pub(crate) fn current_block_terminated(&self) -> bool {
        self.blocks[self.current].terminated
    }

    fn new_temp(&mut self) -> String {
        let name = format!("%t{}", self.temp_counter);
        self.temp_counter += 1;
        name
    }

    fn new_block(&mut self, prefix: &str) -> String {
        let name = format!("{}{}", prefix, self.block_counter);
        self.block_counter += 1;
        name
    }

    pub(crate) fn add_block(&mut self, name: String) -> usize {
        self.blocks.push(BlockInsts {
            name,
            instrs: Vec::new(),
            terminated: false,
        });
        self.blocks.len() - 1
    }

    pub(crate) fn switch_to(&mut self, idx: usize) {
        self.current = idx;
    }

    pub(crate) fn emit_prologue(&mut self, params: &[Type], names: &[String]) -> Result<(), String> {
        for (idx, ty) in params.iter().enumerate() {
            let alloca = self.new_temp();
            self.emit(format!("{} = alloca {}", alloca, llvm_type(ty)?));
            self.emit(format!("store {} %arg{}, {}* {}", llvm_type(ty)?, idx, llvm_type(ty)?, alloca));
            let name = names.get(idx).cloned().unwrap_or_else(|| format!("arg{}", idx));
            self.locals.insert(name, (ty.clone(), alloca));
            if self.is_linear_type(ty) {
                let name = names.get(idx).cloned().unwrap_or_else(|| format!("arg{}", idx));
                self.linear_states.insert(name, true);
            }
        }
        Ok(())
    }

    pub(crate) fn set_mir_mode(&mut self, enabled: bool) {
        self.mir_mode = enabled;
    }

    pub(crate) fn set_mir_locals(&mut self, locals: &[crate::mir::Local]) -> Result<(), String> {
        self.mir_local_ptrs.clear();
        for local in locals {
            let name = local.name.clone();
            let (ty, ptr) = if let Some(name) = &name {
                if let Some((ty, ptr)) = self.locals.get(name).cloned() {
                    (ty, ptr)
                } else {
                    let storage_ty = llvm_storage_type(&local.ty)?;
                    let alloca = self.new_temp();
                    self.emit(format!("{} = alloca {}", alloca, storage_ty));
                    (local.ty.clone(), alloca)
                }
            } else {
                let storage_ty = llvm_storage_type(&local.ty)?;
                let alloca = self.new_temp();
                self.emit(format!("{} = alloca {}", alloca, storage_ty));
                (local.ty.clone(), alloca)
            };
            if let Some(name) = &name {
                if !self.locals.contains_key(name) {
                    self.locals.insert(name.clone(), (ty.clone(), ptr.clone()));
                }
                if self.is_linear_type(&ty) {
                    self.linear_states.entry(name.clone()).or_insert(true);
                }
            }
            self.mir_local_ptrs.push(Some(MirLocalInfo { name, ty, ptr }));
        }
        Ok(())
    }

    pub(crate) fn set_mir_expr_types(&mut self, expr_types: &HashMap<ExprId, Type>) {
        self.mir_expr_types = expr_types.clone();
    }

    fn mir_expr_type_override(&self, expr: &Expr) -> Option<Type> {
        self.mir_expr_types.get(&expr.id).cloned()
    }

    pub(crate) fn emit_block(&mut self, block: &Block) -> Result<Option<Value>, String> {
        self.enter_scope();
        for stmt in &block.stmts {
            self.emit_stmt(stmt)?;
            if self.current_block_terminated() {
                self.exit_scope()?;
                return Ok(None);
            }
        }
        if let Some(expr) = &block.tail {
            let value = self.emit_expr(expr)?;
            self.exit_scope()?;
            Ok(Some(value))
        } else {
            self.exit_scope()?;
            Ok(Some(Value {
                ty: Type::Builtin(BuiltinType::Unit),
                ir: String::new(),
            }))
        }
    }

    fn emit_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Let { name, init, .. } => {
                let value = self.emit_expr(init)?;
                if let ExprKind::Call { callee, .. } = &init.kind {
                    if let ExprKind::Ident(callee_name) = &callee.kind {
                        if callee_name != "shared_new" {
                            self.emit_shared_inc_value(&value)?;
                        }
                    } else {
                        self.emit_shared_inc_value(&value)?;
                    }
                } else {
                    self.emit_shared_inc_value(&value)?;
                }
                let alloca = self.new_temp();
                self.emit(format!("{} = alloca {}", alloca, llvm_type(&value.ty)?));
                if value.ty != Type::Builtin(BuiltinType::Unit) {
                    self.emit(format!(
                        "store {} {}, {}* {}",
                        llvm_type(&value.ty)?,
                        value.ir,
                        llvm_type(&value.ty)?,
                        alloca
                    ));
                }
                let value_ty = value.ty.clone();
                self.locals.insert(name.clone(), (value_ty.clone(), alloca));
                if let Some(scope) = self.scopes.last_mut() {
                    scope.push(name.clone());
                }
                if self.is_linear_type(&value_ty) {
                    self.linear_states.insert(name.clone(), true);
                }
            }
            Stmt::Assign { target, value, .. } => {
                let (target_ty, target_ptr) = self.emit_place_ptr(target)?;
                let target_name = match &target.kind {
                    ExprKind::Ident(name) => Some(name.as_str()),
                    _ => None,
                };
                if self.needs_drop(&target_ty) {
                    let should_drop = if let Some(name) = target_name {
                        if self.is_linear_type(&target_ty) {
                            self.linear_states.get(name).copied().unwrap_or(true)
                        } else {
                            true
                        }
                    } else {
                        true
                    };
                    if should_drop {
                        self.emit_drop_for_ptr(&target_ty, &target_ptr)?;
                    }
                }
                let val = self.emit_expr(value)?;
                if let ExprKind::Call { callee, .. } = &value.kind {
                    if let ExprKind::Ident(callee_name) = &callee.kind {
                        if callee_name != "shared_new" {
                            self.emit_shared_inc_value(&val)?;
                        }
                    } else {
                        self.emit_shared_inc_value(&val)?;
                    }
                } else {
                    self.emit_shared_inc_value(&val)?;
                }
                if target_ty != Type::Builtin(BuiltinType::Unit) {
                    self.emit(format!(
                        "store {} {}, {}* {}",
                        llvm_type(&target_ty)?,
                        val.ir,
                        llvm_storage_type(&target_ty)?,
                        target_ptr
                    ));
                }
                if let Some(name) = target_name {
                    if self.is_linear_type(&target_ty) {
                        self.mark_assigned(name);
                    }
                }
            }
            Stmt::Expr { expr, .. } => {
                let _ = self.emit_expr(expr)?;
            }
            Stmt::Return { expr, .. } => {
                if let Some(expr) = expr {
                    let val = self.emit_expr(expr)?;
                    self.emit_return_value(Some(val))?;
                } else {
                    self.emit_return_value(None)?;
                }
            }
            Stmt::ForIn { name, iter, body, .. } => {
                self.emit_for_in_stmt(name, iter, body)?;
            }
            Stmt::Select { arms, .. } => {
                self.emit_select_stmt(arms)?;
            }
            Stmt::Go { expr, .. } => {
                self.emit_go_stmt(expr)?;
            }
            Stmt::Defer { expr, .. } => {
                let (name, type_args, args) = match &expr.kind {
                    ExprKind::Call { callee, type_args, args } => {
                        let name = match &callee.kind {
                            ExprKind::Ident(name) => name.clone(),
                            _ => return Err("defer expects a direct call".to_string()),
                        };
                        let mut resolved = Vec::new();
                        for arg in type_args {
                            resolved.push(self.resolve_type_ast(arg)?);
                        }
                        (name, resolved, args.clone())
                    }
                    _ => return Err("defer expects a call expression".to_string()),
                };
                let mut stored_args = Vec::new();
                for arg in args {
                    let val = self.emit_expr(&arg)?;
                    let storage_ty = llvm_storage_type(&val.ty)?;
                    let alloca = self.new_temp();
                    self.emit(format!("{} = alloca {}", alloca, storage_ty));
                    if val.ty != Type::Builtin(BuiltinType::Unit) {
                        self.emit(format!(
                            "store {} {}, {}* {}",
                            llvm_type(&val.ty)?,
                            val.ir,
                            storage_ty,
                            alloca
                        ));
                    } else {
                        self.emit(format!("store i8 0, i8* {}", alloca));
                    }
                    stored_args.push((val.ty, alloca));
                }
                let entry = DeferredCall {
                    name,
                    type_args,
                    args: stored_args,
                };
                if let Some(list) = self.deferred.last_mut() {
                    list.push(entry);
                } else {
                    self.deferred.push(vec![entry]);
                }
            }
            Stmt::Break { .. } => {
                let (break_label, _) = self
                    .loop_stack
                    .last()
                    .ok_or_else(|| "break outside loop".to_string())?
                    .clone();
                self.terminate(format!("br label %{}", break_label));
            }
            Stmt::Continue { .. } => {
                let (_, continue_label) = self
                    .loop_stack
                    .last()
                    .ok_or_else(|| "continue outside loop".to_string())?
                    .clone();
                self.terminate(format!("br label %{}", continue_label));
            }
        }
        Ok(())
    }

    pub(crate) fn emit_return_value(&mut self, value: Option<Value>) -> Result<(), String> {
        if let Some(value) = value {
            if value.ty == Type::Builtin(BuiltinType::Unit) {
                self.terminate("ret void");
            } else {
                self.terminate(format!("ret {} {}", llvm_type(&value.ty)?, value.ir));
            }
        } else {
            self.terminate("ret void");
        }
        Ok(())
    }

    pub(crate) fn emit_error_return(&mut self, err_ir: String) -> Result<(), String> {
        let err_val = Value {
            ty: Type::Builtin(BuiltinType::Error),
            ir: err_ir,
        };
        let ret_type = self.ret_type.clone();
        match &ret_type {
            Type::Builtin(BuiltinType::Error) => self.emit_return_value(Some(err_val)),
            Type::Tuple(items) if items.len() == 2 => {
                let ok_ty = &items[0];
                let err_ty = &items[1];
                if *err_ty != Type::Builtin(BuiltinType::Error) {
                    return Err("`?` expects function return type error or (T, error)".to_string());
                }
                let tuple_ty = Type::Tuple(items.clone());
                let llvm_tuple = llvm_type(&tuple_ty)?;
                let zero = if *ok_ty == Type::Builtin(BuiltinType::Unit) {
                    "0".to_string()
                } else {
                    zero_value(ok_ty)?
                };
                let tmp0 = self.new_temp();
                self.emit(format!(
                    "{} = insertvalue {} undef, {} {}, 0",
                    tmp0,
                    llvm_tuple,
                    llvm_type_for_tuple_elem(ok_ty)?,
                    zero
                ));
                let tmp1 = self.new_temp();
                self.emit(format!(
                    "{} = insertvalue {} {}, {} {}, 1",
                    tmp1,
                    llvm_tuple,
                    tmp0,
                    llvm_type(err_ty)?,
                    err_val.ir
                ));
                self.emit_return_value(Some(Value { ty: tuple_ty, ir: tmp1 }))
            }
            _ => Err("`?` expects function return type error or (T, error)".to_string()),
        }
    }

    pub(crate) fn emit_error_return_value(&mut self, value: Value) -> Result<(), String> {
        self.emit_error_return(value.ir)
    }

    pub(crate) fn emit_expr(&mut self, expr: &Expr) -> Result<Value, String> {
        match &expr.kind {
            ExprKind::Bool(value) => Ok(Value {
                ty: Type::Builtin(BuiltinType::Bool),
                ir: if *value { "1".to_string() } else { "0".to_string() },
            }),
            ExprKind::Int(value) => Ok(Value {
                ty: self
                    .mir_expr_type_override(expr)
                    .unwrap_or(Type::Builtin(BuiltinType::I32)),
                ir: value.clone(),
            }),
            ExprKind::Float(value) => Ok(Value {
                ty: Type::Builtin(BuiltinType::F64),
                ir: value.clone(),
            }),
            ExprKind::Char(value) => Ok(Value {
                ty: Type::Builtin(BuiltinType::Char),
                ir: (*value as u32).to_string(),
            }),
            ExprKind::String(value) => self.emit_string_literal(value),
            ExprKind::Nil => Ok(Value {
                ty: Type::Builtin(BuiltinType::Error),
                ir: "0".to_string(),
            }),
            ExprKind::Ident(name) => {
                if let Some((ty, ptr)) = self.locals.get(name).cloned() {
                    if ty == Type::Builtin(BuiltinType::Unit) {
                        return Ok(Value { ty, ir: String::new() });
                    }
                    let tmp = self.new_temp();
                    self.emit(format!(
                        "{} = load {}, {}* {}",
                        tmp,
                        llvm_type(&ty)?,
                        llvm_type(&ty)?,
                        ptr
                    ));
                    if self.is_linear_type(&ty) {
                        self.mark_moved(name);
                    }
                    Ok(Value { ty, ir: tmp })
                } else {
                    let block_name = self
                        .blocks
                        .get(self.current)
                        .map(|b| b.name.clone())
                        .unwrap_or_else(|| "<unknown>".to_string());
                    Err(format!(
                        "unknown local {} in {}@{}",
                        name, self.fn_name, block_name
                    ))
                }
            }
            ExprKind::Tuple(items) => {
                let mut values = Vec::new();
                let mut tys = Vec::new();
                for item in items {
                    let val = self.emit_expr(item)?;
                    self.emit_shared_inc_value(&val)?;
                    tys.push(val.ty.clone());
                    values.push(val);
                }
                let tuple_ty = Type::Tuple(tys);
                if values.is_empty() {
                    return Ok(Value {
                        ty: tuple_ty,
                        ir: "zeroinitializer".to_string(),
                    });
                }
                let llvm_tuple = llvm_type(&tuple_ty)?;
                let mut current = "undef".to_string();
                for (idx, val) in values.iter().enumerate() {
                    let tmp = self.new_temp();
                    let elem_ir = if val.ty == Type::Builtin(BuiltinType::Unit) {
                        "0".to_string()
                    } else {
                        val.ir.clone()
                    };
                    self.emit(format!(
                        "{} = insertvalue {} {}, {} {}, {}",
                        tmp,
                        llvm_tuple,
                        current,
                        llvm_type_for_tuple_elem(&val.ty)?,
                        elem_ir,
                        idx
                    ));
                    current = tmp;
                }
                Ok(Value {
                    ty: tuple_ty,
                    ir: current,
                })
            }
            ExprKind::Block(block) => {
                let value = self.emit_block(block)?;
                Ok(value.unwrap_or(Value {
                    ty: Type::Builtin(BuiltinType::Unit),
                    ir: String::new(),
                }))
            }
            ExprKind::If { cond, then_block, else_block } => {
                let cond_val = self.emit_expr(cond)?;
                let then_name = self.new_block("then");
                let else_name = self.new_block("else");
                let cont_name = self.new_block("ifend");
                let then_idx = self.add_block(then_name.clone());
                let else_idx = self.add_block(else_name.clone());
                let cont_idx = self.add_block(cont_name.clone());
                self.terminate(format!(
                    "br i1 {}, label %{}, label %{}",
                    cond_val.ir, then_name, else_name
                ));
                self.switch_to(then_idx);
                let then_val = self.emit_block(then_block)?;
                if !self.current_block_terminated() {
                    self.terminate(format!("br label %{}", cont_name));
                }
                let then_block_name = self.blocks[then_idx].name.clone();
                self.switch_to(else_idx);
                let else_val = if let Some(block) = else_block {
                    self.emit_block(block)?
                } else {
                    Some(Value {
                        ty: Type::Builtin(BuiltinType::Unit),
                        ir: String::new(),
                    })
                };
                if !self.current_block_terminated() {
                    self.terminate(format!("br label %{}", cont_name));
                }
                let else_block_name = self.blocks[else_idx].name.clone();
                self.switch_to(cont_idx);
                if let (Some(then_val), Some(else_val)) = (then_val, else_val) {
                    if then_val.ty == Type::Builtin(BuiltinType::Unit) {
                        return Ok(Value {
                            ty: Type::Builtin(BuiltinType::Unit),
                            ir: String::new(),
                        });
                    }
                    let phi = self.new_temp();
                    self.emit(format!(
                        "{} = phi {} [ {}, %{} ], [ {}, %{} ]",
                        phi,
                        llvm_type(&then_val.ty)?,
                        then_val.ir,
                        then_block_name,
                        else_val.ir,
                        else_block_name
                    ));
                    Ok(Value {
                        ty: then_val.ty,
                        ir: phi,
                    })
                } else {
                    Ok(Value {
                        ty: Type::Builtin(BuiltinType::Unit),
                        ir: String::new(),
                    })
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                let scrut = self.emit_expr(scrutinee)?;
                let enum_info = match &scrut.ty {
                    Type::Named(name) => match self.types.get(name) {
                        Some(TypeDefKind::Enum(_)) => {
                            let tag = self.new_temp();
                            self.emit(format!(
                                "{} = extractvalue %{} {}, 0",
                                tag, name, scrut.ir
                            ));
                            let payload = self.new_temp();
                            self.emit(format!(
                                "{} = extractvalue %{} {}, 1",
                                payload, name, scrut.ir
                            ));
                            Some((name.clone(), tag, payload))
                        }
                        _ => None,
                    },
                    _ => None,
                };
                let end_name = self.new_block("match_end");
                let end_idx = self.add_block(end_name.clone());
                let mut incoming: Vec<(String, String)> = Vec::new();
                let mut result_ty: Option<Type> = None;
                for (idx, arm) in arms.iter().enumerate() {
                    let is_last = idx + 1 == arms.len();
                    let arm_name = self.new_block("match_arm");
                    let arm_idx = self.add_block(arm_name.clone());
                    let next_name = if !is_last {
                        Some(self.new_block("match_next"))
                    } else {
                        None
                    };
                    let cond_ir = match &arm.pattern {
                        crate::frontend::ast::Pattern::Wildcard | crate::frontend::ast::Pattern::Ident(_) => "1".to_string(),
                        crate::frontend::ast::Pattern::Bool(value) => {
                            let lit = if *value { "1" } else { "0" };
                            let tmp = self.new_temp();
                            self.emit(format!(
                                "{} = icmp eq {} {}, {}",
                                tmp,
                                llvm_type(&scrut.ty)?,
                                scrut.ir,
                                lit
                            ));
                            tmp
                        }
                        crate::frontend::ast::Pattern::Int(value) => {
                            let tmp = self.new_temp();
                            self.emit(format!(
                                "{} = icmp eq {} {}, {}",
                                tmp,
                                llvm_type(&scrut.ty)?,
                                scrut.ir,
                                value
                            ));
                            tmp
                        }
                        crate::frontend::ast::Pattern::Variant {
                            enum_name,
                            variant,
                            ..
                        } => {
                            let (scrut_enum, tag_ir, _) = enum_info
                                .as_ref()
                                .ok_or_else(|| "enum pattern expects enum".to_string())?;
                            if scrut_enum != enum_name {
                                return Err("enum pattern does not match scrutinee type".to_string());
                            }
                            let (variant_idx, _) =
                                self.resolve_enum_variant(enum_name, variant)?;
                            let tmp = self.new_temp();
                            self.emit(format!(
                                "{} = icmp eq i32 {}, {}",
                                tmp, tag_ir, variant_idx
                            ));
                            tmp
                        }
                    };
                    if let Some(next_name) = &next_name {
                        self.terminate(format!(
                            "br i1 {}, label %{}, label %{}",
                            cond_ir, arm_name, next_name
                        ));
                    } else {
                        if cond_ir == "1" {
                            self.terminate(format!("br label %{}", arm_name));
                        } else {
                            self.terminate(format!(
                                "br i1 {}, label %{}, label %{}",
                                cond_ir, arm_name, end_name
                            ));
                        }
                    }
                    self.switch_to(arm_idx);
                    self.enter_scope();
                    match &arm.pattern {
                        crate::frontend::ast::Pattern::Ident(name) => {
                            let alloca = self.new_temp();
                            self.emit(format!("{} = alloca {}", alloca, llvm_type(&scrut.ty)?));
                            if scrut.ty != Type::Builtin(BuiltinType::Unit) {
                                self.emit_shared_inc_value(&scrut)?;
                                self.emit(format!(
                                    "store {} {}, {}* {}",
                                    llvm_type(&scrut.ty)?,
                                    scrut.ir,
                                    llvm_type(&scrut.ty)?,
                                    alloca
                                ));
                            }
                            self.locals.insert(name.clone(), (scrut.ty.clone(), alloca));
                            if let Some(scope) = self.scopes.last_mut() {
                                scope.push(name.clone());
                            }
                        }
                        crate::frontend::ast::Pattern::Variant {
                            enum_name,
                            variant,
                            binds,
                        } => {
                            let (scrut_enum, _, payload_ir) = enum_info
                                .as_ref()
                                .ok_or_else(|| "enum pattern expects enum".to_string())?;
                            if scrut_enum != enum_name {
                                return Err("enum pattern does not match scrutinee type".to_string());
                            }
                            let (variant_idx, field_tys) =
                                self.resolve_enum_variant(enum_name, variant)?;
                            if !field_tys.is_empty() {
                                let payload_ty = self.enum_payload_type_name(enum_name, variant_idx);
                                let payload_ptr = self.new_temp();
                                self.emit(format!(
                                    "{} = bitcast i8* {} to {}*",
                                    payload_ptr, payload_ir, payload_ty
                                ));
                                for (idx, field_ty) in field_tys.iter().enumerate() {
                                    let bind = binds.get(idx).cloned().unwrap_or_else(|| "_".to_string());
                                    if bind == "_" {
                                        continue;
                                    }
                                    let field_ptr = self.new_temp();
                                    self.emit(format!(
                                        "{} = getelementptr {}, {}* {}, i32 0, i32 {}",
                                        field_ptr, payload_ty, payload_ty, payload_ptr, idx
                                    ));
                                    let storage_ty = llvm_type_for_tuple_elem(field_ty)?;
                                    let tmp = self.new_temp();
                                    self.emit(format!(
                                        "{} = load {}, {}* {}",
                                        tmp, storage_ty, storage_ty, field_ptr
                                    ));
                                    let val = Value { ty: field_ty.clone(), ir: tmp.clone() };
                                    self.emit_shared_inc_value(&val)?;
                                    let alloca = self.new_temp();
                                    self.emit(format!("{} = alloca {}", alloca, storage_ty));
                                    self.emit(format!(
                                        "store {} {}, {}* {}",
                                        storage_ty, tmp, storage_ty, alloca
                                    ));
                                    self.locals.insert(bind.clone(), (field_ty.clone(), alloca));
                                    if let Some(scope) = self.scopes.last_mut() {
                                        scope.push(bind);
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                    let arm_val = match &arm.body {
                        crate::frontend::ast::BlockOrExpr::Block(block) => self.emit_block(block)?,
                        crate::frontend::ast::BlockOrExpr::Expr(expr) => Some(self.emit_expr(expr)?),
                    };
                    self.exit_scope()?;
                    if let Some(value) = arm_val {
                        if !self.current_block_terminated() {
                            self.terminate(format!("br label %{}", end_name));
                        }
                        if result_ty.is_none() {
                            result_ty = Some(value.ty.clone());
                        }
                        if value.ty != Type::Builtin(BuiltinType::Unit) {
                            incoming.push((value.ir, arm_name.clone()));
                        }
                    }
                    if let Some(next_name) = next_name {
                        let next_idx = self.add_block(next_name.clone());
                        self.switch_to(next_idx);
                    }
                }
                self.switch_to(end_idx);
                if let Some(res_ty) = result_ty {
                    if res_ty == Type::Builtin(BuiltinType::Unit) {
                        Ok(Value {
                            ty: res_ty,
                            ir: String::new(),
                        })
                    } else {
                        let phi = self.new_temp();
                        let mut incoming_ir = Vec::new();
                        for (val, block) in incoming {
                            incoming_ir.push(format!("[ {}, %{} ]", val, block));
                        }
                        self.emit(format!(
                            "{} = phi {} {}",
                            phi,
                            llvm_type(&res_ty)?,
                            incoming_ir.join(", ")
                        ));
                        Ok(Value { ty: res_ty, ir: phi })
                    }
                } else {
                    Ok(Value {
                        ty: Type::Builtin(BuiltinType::Unit),
                        ir: String::new(),
                    })
                }
            }
            ExprKind::Call { callee, type_args, args } => {
                if let ExprKind::Field { base, name: variant } = &callee.kind {
                    if let ExprKind::Ident(enum_name) = &base.kind {
                        if !self.locals.contains_key(enum_name) {
                            if !type_args.is_empty() {
                                return Err(
                                    "type arguments not supported for enum constructors".to_string(),
                                );
                            }
                            return self.emit_enum_ctor(enum_name, variant, args);
                        }
                    }
                }
                let name = match &callee.kind {
                    ExprKind::Ident(name) => name.clone(),
                    _ => return Err("only direct calls are supported".to_string()),
                };
                let mut resolved_args = Vec::new();
                for arg in args {
                    resolved_args.push(self.emit_expr(arg)?);
                }
                let mut resolved_types = Vec::new();
                for arg in type_args {
                    resolved_types.push(self.resolve_type_ast(arg)?);
                }
                self.emit_call_by_name(&name, &resolved_types, resolved_args)
            }
            ExprKind::Borrow { is_mut, expr: inner } => {
                let (ty, ptr) = self.emit_place_ptr(inner)?;
                let ref_ty = if *is_mut {
                    Type::MutRef(Box::new(ty))
                } else {
                    Type::Ref(Box::new(ty))
                };
                Ok(Value { ty: ref_ty, ir: ptr })
            }
            ExprKind::Deref { expr: inner } => {
                let inner_val = self.emit_expr(inner)?;
                match inner_val.ty {
                    Type::Ref(inner) | Type::MutRef(inner) => {
                        let tmp = self.new_temp();
                        self.emit(format!(
                            "{} = load {}, {}* {}",
                            tmp,
                            llvm_type(&inner)?,
                            llvm_type(&inner)?,
                            inner_val.ir
                        ));
                        Ok(Value { ty: *inner, ir: tmp })
                    }
                    _ => Err("deref expects ref type".to_string()),
                }
            }
            ExprKind::Try { expr: inner } => {
                if self.mir_mode {
                    return Err("`?` must be lowered to MIR before codegen".to_string());
                }
                let value = self.emit_expr(inner)?;
                match &value.ty {
                    Type::Builtin(BuiltinType::Error) => {
                        let is_err = self.new_temp();
                        self.emit(format!(
                            "{} = icmp ne i32 {}, 0",
                            is_err, value.ir
                        ));
                        let ok_name = self.new_block("try_ok");
                        let err_name = self.new_block("try_err");
                        let cont_name = self.new_block("try_cont");
                        let ok_idx = self.add_block(ok_name.clone());
                        let err_idx = self.add_block(err_name.clone());
                        let cont_idx = self.add_block(cont_name.clone());
                        self.terminate(format!(
                            "br i1 {}, label %{}, label %{}",
                            is_err, err_name, ok_name
                        ));
                        self.switch_to(err_idx);
                        self.emit_error_return(value.ir)?;
                        self.switch_to(ok_idx);
                        self.terminate(format!("br label %{}", cont_name));
                        self.switch_to(cont_idx);
                        Ok(Value {
                            ty: Type::Builtin(BuiltinType::Unit),
                            ir: String::new(),
                        })
                    }
                    Type::Tuple(items) if items.len() == 2 => {
                        if items[1] != Type::Builtin(BuiltinType::Error) {
                            return Err("`?` expects (T, error)".to_string());
                        }
                        let err_tmp = self.new_temp();
                        self.emit(format!(
                            "{} = extractvalue {} {}, 1",
                            err_tmp,
                            llvm_type(&value.ty)?,
                            value.ir
                        ));
                        let is_err = self.new_temp();
                        self.emit(format!(
                            "{} = icmp ne i32 {}, 0",
                            is_err, err_tmp
                        ));
                        let ok_name = self.new_block("try_ok");
                        let err_name = self.new_block("try_err");
                        let cont_name = self.new_block("try_cont");
                        let ok_idx = self.add_block(ok_name.clone());
                        let err_idx = self.add_block(err_name.clone());
                        let cont_idx = self.add_block(cont_name.clone());
                        self.terminate(format!(
                            "br i1 {}, label %{}, label %{}",
                            is_err, err_name, ok_name
                        ));
                        self.switch_to(err_idx);
                        self.emit_error_return(err_tmp)?;
                        self.switch_to(ok_idx);
                        let ok_val = self.new_temp();
                        self.emit(format!(
                            "{} = extractvalue {} {}, 0",
                            ok_val,
                            llvm_type(&value.ty)?,
                            value.ir
                        ));
                        self.terminate(format!("br label %{}", cont_name));
                        self.switch_to(cont_idx);
                        if items[0] == Type::Builtin(BuiltinType::Unit) {
                            Ok(Value {
                                ty: items[0].clone(),
                                ir: String::new(),
                            })
                        } else {
                            Ok(Value {
                                ty: items[0].clone(),
                                ir: ok_val,
                            })
                        }
                    }
                    _ => Err("`?` expects error or (T, error)".to_string()),
                }
            }
            ExprKind::Send { chan, value } => {
                let chan_val = self.emit_expr(chan)?;
                let elem_ty = match &chan_val.ty {
                    Type::Chan(inner) => *inner.clone(),
                    _ => return Err("send expects chan[T]".to_string()),
                };
                self.emit_send_with_value(&chan_val, &elem_ty, value)?;
                Ok(Value {
                    ty: Type::Builtin(BuiltinType::Unit),
                    ir: String::new(),
                })
            }
            ExprKind::Recv { chan } => {
                let chan_val = self.emit_expr(chan)?;
                let elem_ty = match &chan_val.ty {
                    Type::Chan(inner) => *inner.clone(),
                    _ => return Err("recv expects chan[T]".to_string()),
                };
                let elem_ty_clone = elem_ty.clone();
                let storage_ty = llvm_storage_type(&elem_ty)?;
                let alloca = self.new_temp();
                self.emit(format!("{} = alloca {}", alloca, storage_ty));
                let elem_ptr = self.new_temp();
                self.emit(format!(
                    "{} = bitcast {}* {} to i8*",
                    elem_ptr, storage_ty, alloca
                ));
                let status = self.new_temp();
                self.emit(format!(
                    "{} = call i32 @__gost_chan_recv(%chan* {}, i8* {})",
                    status, chan_val.ir, elem_ptr
                ));
                let ok = self.new_temp();
                self.emit(format!(
                    "{} = icmp eq i32 {}, 0",
                    ok, status
                ));
                let ready_val = if elem_ty == Type::Builtin(BuiltinType::Unit) {
                    "0".to_string()
                } else {
                    let tmp = self.new_temp();
                    self.emit(format!(
                        "{} = load {}, {}* {}",
                        tmp,
                        llvm_type(&elem_ty)?,
                        storage_ty,
                        alloca
                    ));
                    tmp
                };
                let closed_val = if elem_ty == Type::Builtin(BuiltinType::Unit) {
                    "0".to_string()
                } else {
                    zero_value(&elem_ty)?
                };
                let selected_val = self.new_temp();
                self.emit(format!(
                    "{} = select i1 {}, {} {}, {} {}",
                    selected_val,
                    ok,
                    llvm_type(&elem_ty)?,
                    ready_val,
                    llvm_type(&elem_ty)?,
                    closed_val
                ));
                let tuple_ty = Type::Tuple(vec![elem_ty_clone, Type::Builtin(BuiltinType::Bool)]);
                let llvm_tuple = llvm_type(&tuple_ty)?;
                let tmp0 = self.new_temp();
                self.emit(format!(
                    "{} = insertvalue {} undef, {} {}, 0",
                    tmp0,
                    llvm_tuple,
                    llvm_type_for_tuple_elem(&elem_ty)?,
                    selected_val
                ));
                let tmp1 = self.new_temp();
                self.emit(format!(
                    "{} = insertvalue {} {}, i1 {}, 1",
                    tmp1,
                    llvm_tuple,
                    tmp0,
                    ok
                ));
                Ok(Value { ty: tuple_ty, ir: tmp1 })
            }
            ExprKind::Close { chan } => {
                let chan_val = self.emit_expr(chan)?;
                if !matches!(chan_val.ty, Type::Chan(_)) {
                    return Err("close expects chan[T]".to_string());
                }
                let tmp = self.new_temp();
                self.emit(format!(
                    "{} = call i32 @__gost_chan_close(%chan* {})",
                    tmp, chan_val.ir
                ));
                Ok(Value {
                    ty: Type::Builtin(BuiltinType::Error),
                    ir: tmp,
                })
            }
            ExprKind::After { ms } => {
                let ms_val = self.emit_expr(ms)?;
                let ms_ir = if ms_val.ty == Type::Builtin(BuiltinType::I64) {
                    ms_val.ir
                } else if ms_val.ty == Type::Builtin(BuiltinType::I32) {
                    let tmp = self.new_temp();
                    self.emit(format!("{} = sext i32 {} to i64", tmp, ms_val.ir));
                    tmp
                } else {
                    return Err("after expects i32 milliseconds".to_string());
                };
                let tmp = self.new_temp();
                self.emit(format!(
                    "{} = call %chan* @__gost_after_ms(i64 {})",
                    tmp, ms_ir
                ));
                Ok(Value {
                    ty: Type::Chan(Box::new(Type::Builtin(BuiltinType::Unit))),
                    ir: tmp,
                })
            }
            ExprKind::Field { .. } => {
                if let ExprKind::Field { base, name } = &expr.kind {
                    if let ExprKind::Ident(enum_name) = &base.kind {
                        if !self.locals.contains_key(enum_name) {
                            if let Ok((variant_idx, fields)) =
                                self.resolve_enum_variant(enum_name, name)
                            {
                                if !fields.is_empty() {
                                    return Err("enum variant requires arguments".to_string());
                                }
                                return self.emit_enum_value(enum_name, variant_idx, None);
                            }
                        }
                    }
                }
                let (field_ty, field_ptr) = self.emit_place_ptr(expr)?;
                if field_ty == Type::Builtin(BuiltinType::Unit) {
                    return Ok(Value {
                        ty: field_ty,
                        ir: String::new(),
                    });
                }
                let tmp = self.new_temp();
                let storage_ty = llvm_storage_type(&field_ty)?;
                self.emit(format!(
                    "{} = load {}, {}* {}",
                    tmp,
                    llvm_type(&field_ty)?,
                    storage_ty,
                    field_ptr
                ));
                let val = Value { ty: field_ty, ir: tmp };
                self.emit_shared_inc_value(&val)?;
                Ok(val)
            }
            ExprKind::Index { .. } => {
                let (elem_ty, elem_ptr) = self.emit_place_ptr(expr)?;
                if elem_ty == Type::Builtin(BuiltinType::Unit) {
                    return Ok(Value {
                        ty: elem_ty,
                        ir: String::new(),
                    });
                }
                let tmp = self.new_temp();
                let storage_ty = llvm_storage_type(&elem_ty)?;
                self.emit(format!(
                    "{} = load {}, {}* {}",
                    tmp,
                    llvm_type(&elem_ty)?,
                    storage_ty,
                    elem_ptr
                ));
                let val = Value { ty: elem_ty, ir: tmp };
                self.emit_shared_inc_value(&val)?;
                Ok(val)
            }
            ExprKind::Unary { op, expr: inner } => {
                let value = self.emit_expr(inner)?;
                match op {
                    crate::frontend::ast::UnaryOp::Neg => {
                        if is_float_type(&value.ty) {
                            let tmp = self.new_temp();
                            self.emit(format!("{} = fsub double -0.0, {}", tmp, value.ir));
                            Ok(Value { ty: value.ty, ir: tmp })
                        } else {
                            let tmp = self.new_temp();
                            self.emit(format!("{} = sub {} 0, {}", tmp, llvm_type(&value.ty)?, value.ir));
                            Ok(Value { ty: value.ty, ir: tmp })
                        }
                    }
                    crate::frontend::ast::UnaryOp::Not => {
                        let tmp = self.new_temp();
                        self.emit(format!("{} = xor i1 {}, true", tmp, value.ir));
                        Ok(Value {
                            ty: Type::Builtin(BuiltinType::Bool),
                            ir: tmp,
                        })
                    }
                }
            }
            ExprKind::Binary { op, left, right } => {
                let lhs = self.emit_expr(left)?;
                let rhs = self.emit_expr(right)?;
                if is_float_type(&lhs.ty) {
                    let tmp = self.new_temp();
                    let instr = match op {
                        crate::frontend::ast::BinaryOp::Add => "fadd",
                        crate::frontend::ast::BinaryOp::Sub => "fsub",
                        crate::frontend::ast::BinaryOp::Mul => "fmul",
                        crate::frontend::ast::BinaryOp::Div => "fdiv",
                        crate::frontend::ast::BinaryOp::Eq => "fcmp oeq",
                        crate::frontend::ast::BinaryOp::NotEq => "fcmp one",
                        crate::frontend::ast::BinaryOp::Lt => "fcmp olt",
                        crate::frontend::ast::BinaryOp::Lte => "fcmp ole",
                        crate::frontend::ast::BinaryOp::Gt => "fcmp ogt",
                        crate::frontend::ast::BinaryOp::Gte => "fcmp oge",
                        _ => return Err("unsupported float binary op".to_string()),
                    };
                    self.emit(format!("{} = {} double {}, {}", tmp, instr, lhs.ir, rhs.ir));
                    let ty = match op {
                        crate::frontend::ast::BinaryOp::Eq
                        | crate::frontend::ast::BinaryOp::NotEq
                        | crate::frontend::ast::BinaryOp::Lt
                        | crate::frontend::ast::BinaryOp::Lte
                        | crate::frontend::ast::BinaryOp::Gt
                        | crate::frontend::ast::BinaryOp::Gte => Type::Builtin(BuiltinType::Bool),
                        _ => lhs.ty,
                    };
                    Ok(Value { ty, ir: tmp })
                } else {
                    let tmp = self.new_temp();
                    let instr = match op {
                        crate::frontend::ast::BinaryOp::Add => "add",
                        crate::frontend::ast::BinaryOp::Sub => "sub",
                        crate::frontend::ast::BinaryOp::Mul => "mul",
                        crate::frontend::ast::BinaryOp::Div => "sdiv",
                        crate::frontend::ast::BinaryOp::Rem => "srem",
                        crate::frontend::ast::BinaryOp::Eq => "icmp eq",
                        crate::frontend::ast::BinaryOp::NotEq => "icmp ne",
                        crate::frontend::ast::BinaryOp::Lt => "icmp slt",
                        crate::frontend::ast::BinaryOp::Lte => "icmp sle",
                        crate::frontend::ast::BinaryOp::Gt => "icmp sgt",
                        crate::frontend::ast::BinaryOp::Gte => "icmp sge",
                        crate::frontend::ast::BinaryOp::And => "and",
                        crate::frontend::ast::BinaryOp::Or => "or",
                    };
                    self.emit(format!(
                        "{} = {} {} {}, {}",
                        tmp,
                        instr,
                        llvm_type(&lhs.ty)?,
                        lhs.ir,
                        rhs.ir
                    ));
                    let ty = match op {
                        crate::frontend::ast::BinaryOp::Eq
                        | crate::frontend::ast::BinaryOp::NotEq
                        | crate::frontend::ast::BinaryOp::Lt
                        | crate::frontend::ast::BinaryOp::Lte
                        | crate::frontend::ast::BinaryOp::Gt
                        | crate::frontend::ast::BinaryOp::Gte
                        | crate::frontend::ast::BinaryOp::And
                        | crate::frontend::ast::BinaryOp::Or => Type::Builtin(BuiltinType::Bool),
                        _ => lhs.ty,
                    };
                    Ok(Value { ty, ir: tmp })
                }
            }
        }
    }

    fn emit_call_by_name(
        &mut self,
        name: &str,
        type_args: &[Type],
        args: Vec<Value>,
    ) -> Result<Value, String> {
        if name == "__gost_println" {
            if !type_args.is_empty() {
                return Err("__gost_println does not take type arguments".to_string());
            }
            if args.len() != 1 {
                return Err("__gost_println expects 1 argument".to_string());
            }
            let arg = &args[0];
            if arg.ty != Type::Builtin(BuiltinType::String) {
                return Err("__gost_println expects string".to_string());
            }
            let ptr = self.new_temp();
            let len = self.new_temp();
            self.emit(format!("{} = extractvalue %string {}, 0", ptr, arg.ir));
            self.emit(format!("{} = extractvalue %string {}, 1", len, arg.ir));
            self.emit(format!("call void @__gost_println_str(i8* {}, i64 {})", ptr, len));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::Unit),
                ir: String::new(),
            });
        }
        if name == "make_chan" {
            if type_args.len() != 1 {
                return Err("make_chan expects one type argument".to_string());
            }
            if args.len() != 1 {
                return Err("make_chan expects one argument".to_string());
            }
            let elem_ty = type_args[0].clone();
            let size_val = self.emit_size_of(&elem_ty)?;
            let cap_val = &args[0];
            let cap_ir = if cap_val.ty == Type::Builtin(BuiltinType::I32) {
                cap_val.ir.clone()
            } else if cap_val.ty == Type::Builtin(BuiltinType::I64) {
                let tmp = self.new_temp();
                self.emit(format!("{} = trunc i64 {} to i32", tmp, cap_val.ir));
                tmp
            } else {
                return Err("make_chan expects i32 cap".to_string());
            };
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call %chan* @__gost_chan_new(i64 {}, i32 {})",
                tmp, size_val.ir, cap_ir
            ));
            return Ok(Value {
                ty: Type::Chan(Box::new(elem_ty)),
                ir: tmp,
            });
        }
        if name == "make_slice" {
            if type_args.len() != 1 {
                return Err("make_slice expects one type argument".to_string());
            }
            if args.len() != 2 {
                return Err("make_slice expects two arguments".to_string());
            }
            let elem_ty = type_args[0].clone();
            let len_ir = self.emit_index_i64(&args[0])?;
            let cap_ir = self.emit_index_i64(&args[1])?;
            let size_val = self.emit_size_of(&elem_ty)?;
            let drop_fn = self.emit_drop_fn_ptr(&elem_ty)?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call %slice_obj* @__gost_slice_new(i64 {}, i64 {}, i64 {}, void (i8*)* {})",
                tmp, size_val.ir, len_ir, cap_ir, drop_fn
            ));
            let slice_val = self.new_temp();
            self.emit(format!(
                "{} = insertvalue %slice undef, %slice_obj* {}, 0",
                slice_val, tmp
            ));
            return Ok(Value {
                ty: Type::Slice(Box::new(elem_ty)),
                ir: slice_val,
            });
        }
        if name == "slice_len" {
            if type_args.len() != 1 {
                return Err("slice_len expects one type argument".to_string());
            }
            if args.len() != 1 {
                return Err("slice_len expects one argument".to_string());
            }
            let slice_obj = self.emit_slice_obj_from_value(&args[0])?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i64 @__gost_slice_len(%slice_obj* {})",
                tmp, slice_obj
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I64),
                ir: tmp,
            });
        }
        if name == "slice_get_copy" {
            if type_args.len() != 1 {
                return Err("slice_get_copy expects one type argument".to_string());
            }
            if args.len() != 2 {
                return Err("slice_get_copy expects two arguments".to_string());
            }
            let elem_ty = type_args[0].clone();
            let slice_obj = self.emit_slice_obj_from_value(&args[0])?;
            let elem_ptr = self.emit_slice_elem_ptr(&slice_obj, &elem_ty, &args[1], true)?;
            let storage_ty = llvm_storage_type(&elem_ty)?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = load {}, {}* {}",
                tmp,
                llvm_type(&elem_ty)?,
                storage_ty,
                elem_ptr
            ));
            let val = Value { ty: elem_ty.clone(), ir: tmp.clone() };
            self.emit_shared_inc_value(&val)?;
            return Ok(Value { ty: elem_ty, ir: tmp });
        }
        if name == "slice_set" {
            if type_args.len() != 1 {
                return Err("slice_set expects one type argument".to_string());
            }
            if args.len() != 3 {
                return Err("slice_set expects three arguments".to_string());
            }
            let elem_ty = type_args[0].clone();
            let slice_obj = self.emit_slice_obj_from_value(&args[0])?;
            let elem_ptr = self.emit_slice_elem_ptr(&slice_obj, &elem_ty, &args[1], true)?;
            let storage_ty = llvm_storage_type(&elem_ty)?;
            if matches!(elem_ty, Type::Shared(_)) {
                self.emit_shared_inc_value(&args[2])?;
            }
            self.emit(format!(
                "store {} {}, {}* {}",
                llvm_type(&elem_ty)?,
                args[2].ir,
                storage_ty,
                elem_ptr
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::Unit),
                ir: String::new(),
            });
        }
        if name == "slice_ref" || name == "slice_mutref" {
            if type_args.len() != 1 {
                return Err("slice_ref expects one type argument".to_string());
            }
            if args.len() != 2 {
                return Err("slice_ref expects two arguments".to_string());
            }
            let elem_ty = type_args[0].clone();
            let slice_obj = self.emit_slice_obj_from_value(&args[0])?;
            let elem_ptr = self.emit_slice_elem_ptr(&slice_obj, &elem_ty, &args[1], true)?;
            let ref_ty = if name == "slice_mutref" {
                Type::MutRef(Box::new(elem_ty))
            } else {
                Type::Ref(Box::new(elem_ty))
            };
            return Ok(Value {
                ty: ref_ty,
                ir: elem_ptr,
            });
        }
        if name == "slice_push" {
            if type_args.len() != 1 {
                return Err("slice_push expects one type argument".to_string());
            }
            if args.len() != 2 {
                return Err("slice_push expects two arguments".to_string());
            }
            let elem_ty = type_args[0].clone();
            let slice_obj = self.emit_slice_obj_from_value(&args[0])?;
            let storage_ty = llvm_storage_type(&elem_ty)?;
            if matches!(elem_ty, Type::Shared(_)) {
                self.emit_shared_inc_value(&args[1])?;
            }
            let alloca = self.new_temp();
            self.emit(format!("{} = alloca {}", alloca, storage_ty));
            if elem_ty != Type::Builtin(BuiltinType::Unit) {
                self.emit(format!(
                    "store {} {}, {}* {}",
                    llvm_type(&elem_ty)?,
                    args[1].ir,
                    storage_ty,
                    alloca
                ));
            } else {
                self.emit(format!("store i8 0, i8* {}", alloca));
            }
            let cast_ptr = self.new_temp();
            self.emit(format!(
                "{} = bitcast {}* {} to i8*",
                cast_ptr, storage_ty, alloca
            ));
            self.emit(format!(
                "call void @__gost_slice_push(%slice_obj* {}, i8* {})",
                slice_obj, cast_ptr
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::Unit),
                ir: String::new(),
            });
        }
        if name == "slice_pop" {
            if type_args.len() != 1 {
                return Err("slice_pop expects one type argument".to_string());
            }
            if args.len() != 1 {
                return Err("slice_pop expects one argument".to_string());
            }
            let elem_ty = type_args[0].clone();
            let slice_obj = self.emit_slice_obj_from_value(&args[0])?;
            let storage_ty = llvm_storage_type(&elem_ty)?;
            let alloca = self.new_temp();
            self.emit(format!("{} = alloca {}", alloca, storage_ty));
            if elem_ty == Type::Builtin(BuiltinType::Unit) {
                self.emit(format!("store i8 0, i8* {}", alloca));
            }
            let cast_ptr = self.new_temp();
            self.emit(format!(
                "{} = bitcast {}* {} to i8*",
                cast_ptr, storage_ty, alloca
            ));
            let ret = self.new_temp();
            self.emit(format!(
                "{} = call i32 @__gost_slice_pop(%slice_obj* {}, i8* {})",
                ret, slice_obj, cast_ptr
            ));
            let ok = self.new_temp();
            self.emit(format!("{} = icmp eq i32 {}, 0", ok, ret));
            let val = if elem_ty == Type::Builtin(BuiltinType::Unit) {
                "0".to_string()
            } else {
                let tmp = self.new_temp();
                self.emit(format!(
                    "{} = load {}, {}* {}",
                    tmp,
                    llvm_type(&elem_ty)?,
                    storage_ty,
                    alloca
                ));
                tmp
            };
            let tuple_ty = Type::Tuple(vec![elem_ty.clone(), Type::Builtin(BuiltinType::Bool)]);
            let llvm_tuple = llvm_type(&tuple_ty)?;
            let tmp0 = self.new_temp();
            self.emit(format!(
                "{} = insertvalue {} undef, {} {}, 0",
                tmp0,
                llvm_tuple,
                llvm_type_for_tuple_elem(&elem_ty)?,
                val
            ));
            let tmp1 = self.new_temp();
            self.emit(format!(
                "{} = insertvalue {} {}, i1 {}, 1",
                tmp1, llvm_tuple, tmp0, ok
            ));
            return Ok(Value { ty: tuple_ty, ir: tmp1 });
        }
        if name == "shared_new" {
            if type_args.len() != 1 {
                return Err("shared_new expects one type argument".to_string());
            }
            if args.len() != 1 {
                return Err("shared_new expects one argument".to_string());
            }
            let elem_ty = type_args[0].clone();
            let storage_ty = llvm_storage_type(&elem_ty)?;
            let alloca = self.new_temp();
            self.emit(format!("{} = alloca {}", alloca, storage_ty));
            if elem_ty != Type::Builtin(BuiltinType::Unit) {
                self.emit(format!(
                    "store {} {}, {}* {}",
                    llvm_type(&elem_ty)?,
                    args[0].ir,
                    storage_ty,
                    alloca
                ));
            } else {
                self.emit(format!("store i8 0, i8* {}", alloca));
            }
            let cast_ptr = self.new_temp();
            self.emit(format!(
                "{} = bitcast {}* {} to i8*",
                cast_ptr, storage_ty, alloca
            ));
            let size_val = self.emit_size_of(&elem_ty)?;
            let drop_fn = self.emit_drop_fn_ptr(&elem_ty)?;
            let obj = self.new_temp();
            self.emit(format!(
                "{} = call %shared_obj* @__gost_shared_new(i64 {}, void (i8*)* {}, i8* {})",
                obj, size_val.ir, drop_fn, cast_ptr
            ));
            let shared_val = self.new_temp();
            self.emit(format!(
                "{} = insertvalue %shared undef, %shared_obj* {}, 0",
                shared_val, obj
            ));
            return Ok(Value {
                ty: Type::Shared(Box::new(elem_ty)),
                ir: shared_val,
            });
        }
        if name == "shared_get" || name == "shared_get_mut" {
            if type_args.len() != 1 {
                return Err("shared_get expects one type argument".to_string());
            }
            if args.len() != 1 {
                return Err("shared_get expects one argument".to_string());
            }
            let elem_ty = type_args[0].clone();
            let shared_obj = self.emit_shared_obj_from_value(&args[0])?;
            if name == "shared_get_mut" {
                let unique = self.new_temp();
                self.emit(format!(
                    "{} = call i32 @__gost_shared_is_unique(%shared_obj* {})",
                    unique, shared_obj
                ));
                let ok = self.new_temp();
                self.emit(format!("{} = icmp eq i32 {}, 1", ok, unique));
                let ok_name = self.new_block("shared_unique_ok");
                let err_name = self.new_block("shared_unique_err");
                let cont_name = self.new_block("shared_unique_cont");
                let ok_idx = self.add_block(ok_name.clone());
                let err_idx = self.add_block(err_name.clone());
                let cont_idx = self.add_block(cont_name.clone());
                self.terminate(format!(
                    "br i1 {}, label %{}, label %{}",
                    ok, ok_name, err_name
                ));
                self.switch_to(err_idx);
                self.emit("call void @__gost_panic(i8* null, i64 0)");
                self.terminate(format!("br label %{}", cont_name));
                self.switch_to(ok_idx);
                self.terminate(format!("br label %{}", cont_name));
                self.switch_to(cont_idx);
            }
            let ptr = self.new_temp();
            self.emit(format!(
                "{} = call i8* @__gost_shared_get_ptr(%shared_obj* {})",
                ptr, shared_obj
            ));
            let cast_ptr = self.new_temp();
            let storage_ty = llvm_storage_type(&elem_ty)?;
            self.emit(format!(
                "{} = bitcast i8* {} to {}*",
                cast_ptr, ptr, storage_ty
            ));
            let ref_ty = if name == "shared_get_mut" {
                Type::MutRef(Box::new(elem_ty))
            } else {
                Type::Ref(Box::new(elem_ty))
            };
            return Ok(Value { ty: ref_ty, ir: cast_ptr });
        }
        if name == "make_map" {
            if type_args.len() != 2 {
                return Err("make_map expects two type arguments".to_string());
            }
            if args.len() != 1 {
                return Err("make_map expects one argument".to_string());
            }
            let key_ty = type_args[0].clone();
            let val_ty = type_args[1].clone();
            let cap_ir = self.emit_index_i64(&args[0])?;
            let key_kind = match key_ty {
                Type::Builtin(BuiltinType::I64) => 1,
                Type::Builtin(BuiltinType::U64) => 2,
                Type::Builtin(BuiltinType::String) => 3,
                _ => return Err("map key type must be i64, u64, or string".to_string()),
            };
            let val_size = self.emit_size_of(&val_ty)?;
            let obj = self.new_temp();
            self.emit(format!(
                "{} = call %map_obj* @__gost_map_new(i32 {}, i64 {}, i64 {})",
                obj, key_kind, val_size.ir, cap_ir
            ));
            let map_val = self.new_temp();
            self.emit(format!(
                "{} = insertvalue %map undef, %map_obj* {}, 0",
                map_val, obj
            ));
            return Ok(Value {
                ty: Type::Map(Box::new(key_ty), Box::new(val_ty)),
                ir: map_val,
            });
        }
        if name == "map_len" {
            if type_args.len() != 2 {
                return Err("map_len expects two type arguments".to_string());
            }
            if args.len() != 1 {
                return Err("map_len expects one argument".to_string());
            }
            let map_obj = self.emit_map_obj_from_value(&args[0])?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i64 @__gost_map_len(%map_obj* {})",
                tmp, map_obj
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I64),
                ir: tmp,
            });
        }
        if name == "map_get" {
            if type_args.len() != 2 {
                return Err("map_get expects two type arguments".to_string());
            }
            if args.len() != 2 {
                return Err("map_get expects two arguments".to_string());
            }
            let key_ty = type_args[0].clone();
            let val_ty = type_args[1].clone();
            let map_obj = self.emit_map_obj_from_value(&args[0])?;
            let key_alloca = self.new_temp();
            let key_storage = llvm_storage_type(&key_ty)?;
            self.emit(format!("{} = alloca {}", key_alloca, key_storage));
            self.emit(format!(
                "store {} {}, {}* {}",
                llvm_type(&key_ty)?,
                args[1].ir,
                key_storage,
                key_alloca
            ));
            let key_ptr = self.new_temp();
            self.emit(format!(
                "{} = bitcast {}* {} to i8*",
                key_ptr, key_storage, key_alloca
            ));
            let val_alloca = self.new_temp();
            let val_storage = llvm_storage_type(&val_ty)?;
            self.emit(format!("{} = alloca {}", val_alloca, val_storage));
            let val_ptr = self.new_temp();
            self.emit(format!(
                "{} = bitcast {}* {} to i8*",
                val_ptr, val_storage, val_alloca
            ));
            let ret = self.new_temp();
            self.emit(format!(
                "{} = call i32 @__gost_map_get(%map_obj* {}, i8* {}, i8* {})",
                ret, map_obj, key_ptr, val_ptr
            ));
            let ok = self.new_temp();
            self.emit(format!("{} = icmp ne i32 {}, 0", ok, ret));
            let val_tmp = self.new_temp();
            self.emit(format!(
                "{} = load {}, {}* {}",
                val_tmp,
                llvm_type(&val_ty)?,
                val_storage,
                val_alloca
            ));
            let val = Value { ty: val_ty.clone(), ir: val_tmp.clone() };
            self.emit_shared_inc_value(&val)?;
            let tuple_ty = Type::Tuple(vec![val_ty.clone(), Type::Builtin(BuiltinType::Bool)]);
            let llvm_tuple = llvm_type(&tuple_ty)?;
            let tmp0 = self.new_temp();
            self.emit(format!(
                "{} = insertvalue {} undef, {} {}, 0",
                tmp0,
                llvm_tuple,
                llvm_type_for_tuple_elem(&val_ty)?,
                val_tmp
            ));
            let tmp1 = self.new_temp();
            self.emit(format!(
                "{} = insertvalue {} {}, i1 {}, 1",
                tmp1, llvm_tuple, tmp0, ok
            ));
            return Ok(Value { ty: tuple_ty, ir: tmp1 });
        }
        if name == "map_set" {
            if type_args.len() != 2 {
                return Err("map_set expects two type arguments".to_string());
            }
            if args.len() != 3 {
                return Err("map_set expects three arguments".to_string());
            }
            let key_ty = type_args[0].clone();
            let val_ty = type_args[1].clone();
            let map_obj = self.emit_map_obj_from_value(&args[0])?;
            let key_alloca = self.new_temp();
            let key_storage = llvm_storage_type(&key_ty)?;
            self.emit(format!("{} = alloca {}", key_alloca, key_storage));
            self.emit(format!(
                "store {} {}, {}* {}",
                llvm_type(&key_ty)?,
                args[1].ir,
                key_storage,
                key_alloca
            ));
            let val_alloca = self.new_temp();
            let val_storage = llvm_storage_type(&val_ty)?;
            self.emit(format!("{} = alloca {}", val_alloca, val_storage));
            if matches!(val_ty, Type::Shared(_)) {
                self.emit_shared_inc_value(&args[2])?;
            }
            self.emit(format!(
                "store {} {}, {}* {}",
                llvm_type(&val_ty)?,
                args[2].ir,
                val_storage,
                val_alloca
            ));
            let key_ptr = self.new_temp();
            self.emit(format!(
                "{} = bitcast {}* {} to i8*",
                key_ptr, key_storage, key_alloca
            ));
            let val_ptr = self.new_temp();
            self.emit(format!(
                "{} = bitcast {}* {} to i8*",
                val_ptr, val_storage, val_alloca
            ));
            self.emit(format!(
                "call void @__gost_map_set(%map_obj* {}, i8* {}, i8* {})",
                map_obj, key_ptr, val_ptr
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::Unit),
                ir: String::new(),
            });
        }
        if name == "map_del" {
            if type_args.len() != 2 {
                return Err("map_del expects two type arguments".to_string());
            }
            if args.len() != 2 {
                return Err("map_del expects two arguments".to_string());
            }
            let key_ty = type_args[0].clone();
            let map_obj = self.emit_map_obj_from_value(&args[0])?;
            let key_alloca = self.new_temp();
            let key_storage = llvm_storage_type(&key_ty)?;
            self.emit(format!("{} = alloca {}", key_alloca, key_storage));
            self.emit(format!(
                "store {} {}, {}* {}",
                llvm_type(&key_ty)?,
                args[1].ir,
                key_storage,
                key_alloca
            ));
            let key_ptr = self.new_temp();
            self.emit(format!(
                "{} = bitcast {}* {} to i8*",
                key_ptr, key_storage, key_alloca
            ));
            let ret = self.new_temp();
            self.emit(format!(
                "{} = call i32 @__gost_map_del(%map_obj* {}, i8* {})",
                ret, map_obj, key_ptr
            ));
            let ok = self.new_temp();
            self.emit(format!("{} = icmp ne i32 {}, 0", ok, ret));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::Bool),
                ir: ok,
            });
        }
        let fn_sig = match self.fn_sigs.get(name) {
            Some(sig) => sig.clone(),
            None => return Err(format!("unknown function {}", name)),
        };
        if !type_args.is_empty() {
            return Err("type arguments not supported for this call".to_string());
        }
        let mut args_ir = Vec::new();
        for (val, ty) in args.iter().zip(fn_sig.params.iter()) {
            if matches!(ty, Type::Shared(_)) {
                self.emit_shared_inc_value(val)?;
            }
            args_ir.push(format!("{} {}", llvm_type(ty)?, val.ir));
        }
        let ret_ty = llvm_type(&fn_sig.ret)?;
        if fn_sig.ret == Type::Builtin(BuiltinType::Unit) {
            self.emit(format!("call {} @{}({})", ret_ty, name, args_ir.join(", ")));
            Ok(Value {
                ty: Type::Builtin(BuiltinType::Unit),
                ir: String::new(),
            })
        } else {
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call {} @{}({})",
                tmp,
                ret_ty,
                name,
                args_ir.join(", ")
            ));
            Ok(Value {
                ty: fn_sig.ret,
                ir: tmp,
            })
        }
    }

    fn emit_go_spawn(
        &mut self,
        callee_name: &str,
        param_tys: &[Type],
        args: &[Value],
    ) -> Result<(), String> {
        let mut field_tys = Vec::new();
        for ty in param_tys {
            field_tys.push(llvm_storage_type(ty)?);
        }
        let ctx_ty = format!("{{ {} }}", field_tys.join(", "));
        let size_val = self.emit_size_of_llvm(&ctx_ty)?;
        let ctx_raw = self.new_temp();
        self.emit(format!(
            "{} = call i8* @__gost_alloc(i64 {}, i64 1)",
            ctx_raw, size_val.ir
        ));
        let ctx_ptr = self.new_temp();
        self.emit(format!(
            "{} = bitcast i8* {} to {}*",
            ctx_ptr, ctx_raw, ctx_ty
        ));
        for (idx, (arg, field_ty)) in args.iter().zip(field_tys.iter()).enumerate() {
            let slot = self.new_temp();
            self.emit(format!(
                "{} = getelementptr {}, {}* {}, i32 0, i32 {}",
                slot, ctx_ty, ctx_ty, ctx_ptr, idx
            ));
            if matches!(arg.ty, Type::Shared(_)) {
                self.emit_shared_inc_value(arg)?;
            }
            if arg.ty != Type::Builtin(BuiltinType::Unit) {
                self.emit(format!(
                    "store {} {}, {}* {}",
                    llvm_type(&arg.ty)?,
                    arg.ir,
                    field_ty,
                    slot
                ));
            } else {
                self.emit(format!("store i8 0, i8* {}", slot));
            }
        }
        let thunk_name = self.next_go_thunk_name();
        self.emit_go_thunk_def(&thunk_name, callee_name, param_tys, &ctx_ty)?;
        self.emit(format!(
            "call void @__gost_spawn_thread(void (i8*)* @{}, i8* {})",
            thunk_name, ctx_raw
        ));
        Ok(())
    }

    fn emit_for_in_stmt(
        &mut self,
        name: &str,
        iter: &Expr,
        body: &Block,
    ) -> Result<(), String> {
        let iter_val = self.emit_expr(iter)?;
        let (elem_ty, loop_var_ty) = match &iter_val.ty {
            Type::Slice(inner) => {
                let elem = *inner.clone();
                if !matches!(self.types.classify(&elem), Some(TypeClass::Copy)) {
                    return Err(
                        "for-in by value requires Copy element; use &xs or &mut xs".to_string(),
                    );
                }
                (elem.clone(), elem)
            }
            Type::Builtin(BuiltinType::Bytes) => {
                let elem = Type::Builtin(BuiltinType::U32);
                (elem.clone(), elem)
            }
            Type::Ref(inner) => match &**inner {
                Type::Slice(elem) => {
                    let elem = *elem.clone();
                    (elem.clone(), Type::Ref(Box::new(elem)))
                }
                Type::Builtin(BuiltinType::Bytes) => {
                    let elem = Type::Builtin(BuiltinType::U32);
                    (elem.clone(), Type::Ref(Box::new(elem)))
                }
                _ => return Err("for-in expects a slice".to_string()),
            },
            Type::MutRef(inner) => match &**inner {
                Type::Slice(elem) => {
                    let elem = *elem.clone();
                    (elem.clone(), Type::MutRef(Box::new(elem)))
                }
                Type::Builtin(BuiltinType::Bytes) => {
                    let elem = Type::Builtin(BuiltinType::U32);
                    (elem.clone(), Type::MutRef(Box::new(elem)))
                }
                _ => return Err("for-in expects a slice".to_string()),
            },
            _ => return Err("for-in expects a slice".to_string()),
        };
        let slice_obj = self.emit_slice_obj_from_value(&iter_val)?;
        let len = self.new_temp();
        self.emit(format!(
            "{} = call i64 @__gost_slice_len(%slice_obj* {})",
            len, slice_obj
        ));
        let idx_alloca = self.new_temp();
        self.emit(format!("{} = alloca i64", idx_alloca));
        self.emit(format!("store i64 0, i64* {}", idx_alloca));
        let cond_name = self.new_block("for_cond");
        let body_name = self.new_block("for_body");
        let step_name = self.new_block("for_step");
        let end_name = self.new_block("for_end");
        let cond_idx = self.add_block(cond_name.clone());
        let body_idx = self.add_block(body_name.clone());
        let step_idx = self.add_block(step_name.clone());
        let end_idx = self.add_block(end_name.clone());
        self.terminate(format!("br label %{}", cond_name));
        self.switch_to(cond_idx);
        let idx_val = self.new_temp();
        self.emit(format!("{} = load i64, i64* {}", idx_val, idx_alloca));
        let cmp = self.new_temp();
        self.emit(format!("{} = icmp ult i64 {}, {}", cmp, idx_val, len));
        self.terminate(format!(
            "br i1 {}, label %{}, label %{}",
            cmp, body_name, end_name
        ));
        self.switch_to(body_idx);
        self.enter_scope();
        let elem_ptr = self.emit_slice_elem_ptr(
            &slice_obj,
            &elem_ty,
            &Value {
                ty: Type::Builtin(BuiltinType::I64),
                ir: idx_val.clone(),
            },
            false,
        )?;
        let storage_ty = llvm_storage_type(&elem_ty)?;
        let alloca = self.new_temp();
        self.emit(format!(
            "{} = alloca {}",
            alloca,
            llvm_storage_type(&loop_var_ty)?
        ));
        if loop_var_ty == elem_ty {
            let load_tmp = self.new_temp();
            self.emit(format!(
                "{} = load {}, {}* {}",
                load_tmp,
                llvm_type(&elem_ty)?,
                storage_ty,
                elem_ptr
            ));
            self.emit(format!(
                "store {} {}, {}* {}",
                llvm_type(&elem_ty)?,
                load_tmp,
                llvm_storage_type(&loop_var_ty)?,
                alloca
            ));
        } else {
            self.emit(format!(
                "store {} {}, {}* {}",
                llvm_storage_type(&loop_var_ty)?,
                elem_ptr,
                llvm_storage_type(&loop_var_ty)?,
                alloca
            ));
        }
        self.locals.insert(name.to_string(), (loop_var_ty, alloca));
        if let Some(scope) = self.scopes.last_mut() {
            scope.push(name.to_string());
        }
        self.loop_stack
            .push((end_name.clone(), step_name.clone()));
        let _ = self.emit_block(body)?;
        self.loop_stack.pop();
        self.exit_scope()?;
        if !self.current_block_terminated() {
            self.terminate(format!("br label %{}", step_name));
        }
        self.switch_to(step_idx);
        let next_idx = self.new_temp();
        self.emit(format!("{} = add i64 {}, 1", next_idx, idx_val));
        self.emit(format!("store i64 {}, i64* {}", next_idx, idx_alloca));
        self.terminate(format!("br label %{}", cond_name));
        self.switch_to(end_idx);
        Ok(())
    }

    fn emit_select_stmt(&mut self, arms: &[crate::frontend::ast::SelectArm]) -> Result<(), String> {
        let mut arm_channels: Vec<Option<Value>> = Vec::new();
        let mut arm_elem_tys: Vec<Option<Type>> = Vec::new();
        for arm in arms {
            match &arm.kind {
                crate::frontend::ast::SelectArmKind::Send { chan, .. } => {
                    let chan_val = self.emit_expr(chan)?;
                    let elem_ty = match &chan_val.ty {
                        Type::Chan(inner) => *inner.clone(),
                        _ => return Err("select send expects chan[T]".to_string()),
                    };
                    arm_channels.push(Some(chan_val));
                    arm_elem_tys.push(Some(elem_ty));
                }
                crate::frontend::ast::SelectArmKind::Recv { chan, .. } => {
                    let chan_val = self.emit_expr(chan)?;
                    let elem_ty = match &chan_val.ty {
                        Type::Chan(inner) => *inner.clone(),
                        _ => return Err("select recv expects chan[T]".to_string()),
                    };
                    arm_channels.push(Some(chan_val));
                    arm_elem_tys.push(Some(elem_ty));
                }
                crate::frontend::ast::SelectArmKind::After { ms } => {
                    let ms_val = self.emit_expr(ms)?;
                    let ms_ir = if ms_val.ty == Type::Builtin(BuiltinType::I64) {
                        ms_val.ir
                    } else if ms_val.ty == Type::Builtin(BuiltinType::I32) {
                        let tmp = self.new_temp();
                        self.emit(format!("{} = sext i32 {} to i64", tmp, ms_val.ir));
                        tmp
                    } else {
                        return Err("after expects i32 milliseconds".to_string());
                    };
                    let chan_tmp = self.new_temp();
                    self.emit(format!(
                        "{} = call %chan* @__gost_after_ms(i64 {})",
                        chan_tmp, ms_ir
                    ));
                    arm_channels.push(Some(Value {
                        ty: Type::Chan(Box::new(Type::Builtin(BuiltinType::Unit))),
                        ir: chan_tmp,
                    }));
                    arm_elem_tys.push(Some(Type::Builtin(BuiltinType::Unit)));
                }
                crate::frontend::ast::SelectArmKind::Default => {
                    arm_channels.push(None);
                    arm_elem_tys.push(None);
                }
            }
        }
        let mut default_index = None;
        let mut non_default = Vec::new();
        for (idx, arm) in arms.iter().enumerate() {
            if matches!(arm.kind, crate::frontend::ast::SelectArmKind::Default) {
                default_index = Some(idx);
            } else {
                non_default.push(idx);
            }
        }
        let start_name = self.new_block("select_start");
        let end_name = self.new_block("select_end");
        let start_idx = self.add_block(start_name.clone());
        let end_idx = self.add_block(end_name.clone());
        self.terminate(format!("br label %{}", start_name));
        self.switch_to(start_idx);
        let default_name = default_index.map(|_| self.new_block("select_default"));
        let default_block = default_name
            .as_ref()
            .map(|name| self.add_block(name.clone()));
        let wait_name = if default_index.is_none() {
            Some(self.new_block("select_wait"))
        } else {
            None
        };
        let wait_idx = wait_name
            .as_ref()
            .map(|name| self.add_block(name.clone()));
        if non_default.is_empty() {
            if let Some(default_idx) = default_index {
                let default_name = default_name
                    .as_ref()
                    .ok_or_else(|| "select default missing block".to_string())?;
                let default_block = default_block
                    .ok_or_else(|| "select default missing block".to_string())?;
                self.terminate(format!("br label %{}", default_name));
                self.switch_to(default_block);
                let arm = &arms[default_idx];
                let _ = match &arm.body {
                    crate::frontend::ast::BlockOrExpr::Block(block) => self.emit_block(block),
                    crate::frontend::ast::BlockOrExpr::Expr(expr) => self.emit_expr(expr).map(|_| {
                        Some(Value {
                            ty: Type::Builtin(BuiltinType::Unit),
                            ir: String::new(),
                        })
                    }),
                }?;
                if !self.current_block_terminated() {
                    self.terminate(format!("br label %{}", end_name));
                }
                self.switch_to(end_idx);
                return Ok(());
            }
        }
        for (pos, arm_idx) in non_default.iter().enumerate() {
            let is_last = pos + 1 == non_default.len();
            let arm = &arms[*arm_idx];
            let arm_name = self.new_block("select_arm");
            let next_name = if !is_last {
                Some(self.new_block("select_next"))
            } else {
                None
            };
            let arm_block = self.add_block(arm_name.clone());
            let ready = match &arm.kind {
                crate::frontend::ast::SelectArmKind::Send { .. } => {
                    let chan_val = arm_channels[*arm_idx]
                        .as_ref()
                        .ok_or_else(|| "select send missing channel".to_string())?;
                    let can_send = self.new_temp();
                    self.emit(format!(
                        "{} = call i32 @__gost_chan_can_send(%chan* {})",
                        can_send, chan_val.ir
                    ));
                    let is_ready = self.new_temp();
                    self.emit(format!("{} = icmp ne i32 {}, 0", is_ready, can_send));
                    is_ready
                }
                crate::frontend::ast::SelectArmKind::Recv { .. }
                | crate::frontend::ast::SelectArmKind::After { .. } => {
                    let chan_val = arm_channels[*arm_idx]
                        .as_ref()
                        .ok_or_else(|| "select recv missing channel".to_string())?;
                    let can_recv = self.new_temp();
                    self.emit(format!(
                        "{} = call i32 @__gost_chan_can_recv(%chan* {})",
                        can_recv, chan_val.ir
                    ));
                    let is_ready = self.new_temp();
                    self.emit(format!("{} = icmp ne i32 {}, 0", is_ready, can_recv));
                    is_ready
                }
                crate::frontend::ast::SelectArmKind::Default => "0".to_string(),
            };
            if let Some(next_name) = &next_name {
                self.terminate(format!(
                    "br i1 {}, label %{}, label %{}",
                    ready, arm_name, next_name
                ));
            } else if default_index.is_some() {
                self.terminate(format!(
                    "br i1 {}, label %{}, label %{}",
                    ready,
                    arm_name,
                    default_name
                        .as_ref()
                        .ok_or_else(|| "select default missing block".to_string())?
                ));
            } else {
                self.terminate(format!(
                    "br i1 {}, label %{}, label %{}",
                    ready,
                    arm_name,
                    wait_name
                        .as_ref()
                        .ok_or_else(|| "select wait missing block".to_string())?
                ));
            }
            self.switch_to(arm_block);
            self.enter_scope();
            match &arm.kind {
                crate::frontend::ast::SelectArmKind::Send { value, .. } => {
                    let chan_val = arm_channels[*arm_idx]
                        .as_ref()
                        .ok_or_else(|| "select send missing channel".to_string())?;
                    let elem_ty = arm_elem_tys[*arm_idx]
                        .as_ref()
                        .ok_or_else(|| "select send missing element type".to_string())?;
                    self.emit_send_with_value(chan_val, elem_ty, value)?;
                }
                crate::frontend::ast::SelectArmKind::Recv { .. }
                | crate::frontend::ast::SelectArmKind::After { .. } => {
                    let chan_val = arm_channels[*arm_idx]
                        .as_ref()
                        .ok_or_else(|| "select recv missing channel".to_string())?;
                    let elem_ty = arm_elem_tys[*arm_idx]
                        .as_ref()
                        .ok_or_else(|| "select recv missing element type".to_string())?;
                    let storage_ty = llvm_storage_type(elem_ty)?;
                    let val_ptr = self.new_temp();
                    self.emit(format!("{} = alloca {}", val_ptr, storage_ty));
                    let elem_ptr = self.new_temp();
                    self.emit(format!(
                        "{} = bitcast {}* {} to i8*",
                        elem_ptr, storage_ty, val_ptr
                    ));
                    let status = self.new_temp();
                    self.emit(format!(
                        "{} = call i32 @__gost_chan_recv(%chan* {}, i8* {})",
                        status, chan_val.ir, elem_ptr
                    ));
                    if let crate::frontend::ast::SelectArmKind::Recv {
                        bind: Some((name, ok_name)),
                        ..
                    } = &arm.kind
                    {
                        let ok_tmp = self.new_temp();
                        self.emit(format!("{} = icmp eq i32 {}, 0", ok_tmp, status));
                        let val_alloca = self.new_temp();
                        self.emit(format!(
                            "{} = alloca {}",
                            val_alloca,
                            llvm_storage_type(elem_ty)?
                        ));
                        let val_ir = if *elem_ty == Type::Builtin(BuiltinType::Unit) {
                            "0".to_string()
                        } else {
                            let load_tmp = self.new_temp();
                            self.emit(format!(
                                "{} = load {}, {}* {}",
                                load_tmp,
                                llvm_type(elem_ty)?,
                                storage_ty,
                                val_ptr
                            ));
                            load_tmp
                        };
                        let closed_val = zero_value(elem_ty)?;
                        let select_tmp = self.new_temp();
                        self.emit(format!(
                            "{} = select i1 {}, {} {}, {} {}",
                            select_tmp,
                            ok_tmp,
                            llvm_type(elem_ty)?,
                            val_ir,
                            llvm_type(elem_ty)?,
                            closed_val
                        ));
                        self.emit(format!(
                            "store {} {}, {}* {}",
                            llvm_type(elem_ty)?,
                            select_tmp,
                            llvm_storage_type(elem_ty)?,
                            val_alloca
                        ));
                        self.locals.insert(name.clone(), (elem_ty.clone(), val_alloca));
                        if let Some(scope) = self.scopes.last_mut() {
                            scope.push(name.clone());
                        }
                        let ok_alloca = self.new_temp();
                        self.emit(format!("{} = alloca i1", ok_alloca));
                        self.emit(format!("store i1 {}, i1* {}", ok_tmp, ok_alloca));
                        self.locals
                            .insert(ok_name.clone(), (Type::Builtin(BuiltinType::Bool), ok_alloca));
                        if let Some(scope) = self.scopes.last_mut() {
                            scope.push(ok_name.clone());
                        }
                    }
                }
                crate::frontend::ast::SelectArmKind::Default => {}
            }
            let _ = match &arm.body {
                crate::frontend::ast::BlockOrExpr::Block(block) => self.emit_block(block),
                crate::frontend::ast::BlockOrExpr::Expr(expr) => self.emit_expr(expr).map(|_| {
                    Some(Value {
                        ty: Type::Builtin(BuiltinType::Unit),
                        ir: String::new(),
                    })
                }),
            }?;
            self.exit_scope()?;
            if !self.current_block_terminated() {
                self.terminate(format!("br label %{}", end_name));
            }
            if let Some(next_name) = next_name {
                let next_idx = self.add_block(next_name.clone());
                self.switch_to(next_idx);
            }
        }
        if let Some(default_idx) = default_index {
            let default_name = default_name
                .as_ref()
                .ok_or_else(|| "select default missing block".to_string())?;
            let default_block = default_block
                .ok_or_else(|| "select default missing block".to_string())?;
            self.terminate(format!("br label %{}", default_name));
            self.switch_to(default_block);
            let arm = &arms[default_idx];
            let _ = match &arm.body {
                crate::frontend::ast::BlockOrExpr::Block(block) => self.emit_block(block),
                crate::frontend::ast::BlockOrExpr::Expr(expr) => self.emit_expr(expr).map(|_| {
                    Some(Value {
                        ty: Type::Builtin(BuiltinType::Unit),
                        ir: String::new(),
                    })
                }),
            }?;
            if !self.current_block_terminated() {
                self.terminate(format!("br label %{}", end_name));
            }
        }
        if default_index.is_none() {
            let wait_idx = wait_idx.ok_or_else(|| "select wait missing block".to_string())?;
            self.switch_to(wait_idx);
            if !non_default.is_empty() {
                let arr_len = non_default.len();
                let arr_ty = format!("[{} x %chan*]", arr_len);
                let arr_ptr = self.new_temp();
                self.emit(format!("{} = alloca {}", arr_ptr, arr_ty));
                for (i, arm_idx) in non_default.iter().enumerate() {
                    let chan_val = arm_channels[*arm_idx]
                        .as_ref()
                        .ok_or_else(|| "select wait missing channel".to_string())?;
                    let slot = self.new_temp();
                    self.emit(format!(
                        "{} = getelementptr {}, {}* {}, i32 0, i32 {}",
                        slot, arr_ty, arr_ty, arr_ptr, i
                    ));
                    self.emit(format!("store %chan* {}, %chan** {}", chan_val.ir, slot));
                }
                let cast_ptr = self.new_temp();
                self.emit(format!(
                    "{} = bitcast {}* {} to %chan**",
                    cast_ptr, arr_ty, arr_ptr
                ));
                self.emit(format!(
                    "call i32 @__gost_select_wait(%chan** {}, i32 {})",
                    cast_ptr, arr_len
                ));
            } else {
                self.emit("call i32 @__gost_select_wait(%chan** null, i32 0)");
            }
            self.terminate(format!("br label %{}", start_name));
        }
        self.switch_to(end_idx);
        Ok(())
    }

    fn emit_go_stmt(&mut self, expr: &Expr) -> Result<(), String> {
        if let ExprKind::Call { callee, type_args, args } = &expr.kind {
            let name = match &callee.kind {
                ExprKind::Ident(name) => name.clone(),
                _ => return Err("go expects a direct call".to_string()),
            };
            if !type_args.is_empty() {
                return Err("go does not accept type arguments".to_string());
            }
            if name == "make_chan" {
                return Err("go does not support intrinsic calls".to_string());
            }
            let sig = self
                .fn_sigs
                .get(&name)
                .ok_or_else(|| format!("unknown function {}", name))?;
            let mut resolved_args = Vec::new();
            for arg in args {
                resolved_args.push(self.emit_expr(arg)?);
            }
            if sig.params.len() != resolved_args.len() {
                return Err("argument count mismatch".to_string());
            }
            self.emit_go_spawn(&name, &sig.params, &resolved_args)?;
            Ok(())
        } else {
            Err("go expects a call expression".to_string())
        }
    }

    pub(crate) fn emit_mir_stmt(&mut self, stmt: &MirStmt) -> Result<(), String> {
        match stmt {
            MirStmt::EnterScope { .. } => {
                self.enter_scope();
                Ok(())
            }
            MirStmt::ExitScope { .. } => self.exit_scope(),
            MirStmt::ForIn { name, iter, body } => {
                self.emit_for_in_stmt(name, iter, body)
            }
            MirStmt::Select { arms } => self.emit_select_stmt(arms),
            MirStmt::Go { expr } => self.emit_go_stmt(expr),
            MirStmt::Ast(stmt) => self.emit_stmt(stmt),
            MirStmt::Expr { expr } => {
                let _ = self.emit_expr(expr)?;
                Ok(())
            }
            MirStmt::DeferCall { call } => {
                let _ = self.emit_expr(call)?;
                Ok(())
            }
            MirStmt::Eval { expr, out } => {
                let val = self.emit_expr(expr)?;
                if out.is_empty() {
                    return Ok(());
                }
                if out.len() == 1 {
                    let info = self
                        .mir_local_ptrs
                        .get(out[0])
                        .and_then(|opt| opt.clone())
                        .ok_or_else(|| "missing mir local".to_string())?;
                    if let ExprKind::Call { callee, .. } = &expr.kind {
                        if let ExprKind::Ident(name) = &callee.kind {
                            if name != "shared_new" {
                                self.emit_shared_inc_value(&val)?;
                            }
                        } else {
                            self.emit_shared_inc_value(&val)?;
                        }
                    } else {
                        self.emit_shared_inc_value(&val)?;
                    }
                    if info.ty != Type::Builtin(BuiltinType::Unit) {
                        self.emit(format!(
                            "store {} {}, {}* {}",
                            llvm_type(&info.ty)?,
                            val.ir,
                            llvm_storage_type(&info.ty)?,
                            info.ptr
                        ));
                    } else {
                        self.emit(format!("store i8 0, i8* {}", info.ptr));
                    }
                    if let Some(name) = &info.name {
                        if self.is_linear_type(&info.ty) {
                            self.mark_assigned(name);
                        }
                    }
                    return Ok(());
                }
                let tuple_items = match &val.ty {
                    Type::Tuple(items) => items.clone(),
                    _ => return Err("mir eval expects tuple for multiple outputs".to_string()),
                };
                if tuple_items.len() != out.len() {
                    return Err("mir eval output count mismatch".to_string());
                }
                for (idx, local_id) in out.iter().enumerate() {
                    let info = self
                        .mir_local_ptrs
                        .get(*local_id)
                        .and_then(|opt| opt.clone())
                        .ok_or_else(|| "missing mir local".to_string())?;
                    let tmp = self.new_temp();
                    self.emit(format!(
                        "{} = extractvalue {} {}, {}",
                        tmp,
                        llvm_type(&val.ty)?,
                        val.ir,
                        idx
                    ));
                    if info.ty != Type::Builtin(BuiltinType::Unit) {
                        self.emit(format!(
                            "store {} {}, {}* {}",
                            llvm_type(&info.ty)?,
                            tmp,
                            llvm_storage_type(&info.ty)?,
                            info.ptr
                        ));
                    } else {
                        self.emit(format!("store i8 0, i8* {}", info.ptr));
                    }
                    if let Some(name) = &info.name {
                        if self.is_linear_type(&info.ty) {
                            self.mark_assigned(name);
                        }
                    }
                }
                Ok(())
            }
            MirStmt::Assign { target, value } => {
                let stmt = Stmt::Assign {
                    target: target.clone(),
                    value: value.clone(),
                    span: target.span.clone(),
                };
                self.emit_stmt(&stmt)
            }
            MirStmt::Let { .. } => Err("mir let is not supported in codegen yet".to_string()),
            MirStmt::Drop { local } => {
                let info = self
                    .mir_local_ptrs
                    .get(*local)
                    .and_then(|opt| opt.clone());
                if let Some(info) = info {
                    if let Some(name) = &info.name {
                        if self.is_linear_type(&info.ty)
                            && self.linear_states.get(name).copied() == Some(false)
                        {
                            return Ok(());
                        }
                    }
                    self.emit_drop_for_ptr(&info.ty, &info.ptr)?;
                }
                Ok(())
            }
            MirStmt::DropName { name, ty: _ } => {
                if let Some((local_ty, ptr)) = self.locals.get(name).cloned() {
                    if self.is_linear_type(&local_ty)
                        && self.linear_states.get(name).copied() == Some(false)
                    {
                        return Ok(());
                    }
                    self.emit_drop_for_ptr(&local_ty, &ptr)?;
                } else {
                    return Err(format!("unknown local {} for drop", name));
                }
                Ok(())
            }
        }
    }

    pub(crate) fn emit_mir_terminator(
        &mut self,
        term: &Terminator,
        block_names: &[String],
    ) -> Result<(), String> {
        match term {
            Terminator::Goto(target) => {
                self.terminate(format!("br label %{}", block_names[*target]));
                Ok(())
            }
            Terminator::If { cond, then_bb, else_bb } => {
                let cond_val = self.emit_expr(cond)?;
                self.terminate(format!(
                    "br i1 {}, label %{}, label %{}",
                    cond_val.ir, block_names[*then_bb], block_names[*else_bb]
                ));
                Ok(())
            }
            Terminator::Match { scrutinee, arms, default } => {
                let scrut_val = self.emit_expr(scrutinee)?;
                let default_bb = default.ok_or_else(|| "match default missing".to_string())?;
                if arms.is_empty() {
                    self.terminate(format!("br label %{}", block_names[default_bb]));
                    return Ok(());
                }
                let mut cases = Vec::new();
                for (pattern, target) in arms {
                    let lit = match pattern {
                        crate::frontend::ast::Pattern::Bool(value) => {
                            if *value { "1".to_string() } else { "0".to_string() }
                        }
                        crate::frontend::ast::Pattern::Int(value) => value.clone(),
                        _ => return Err("unsupported pattern in MIR match terminator".to_string()),
                    };
                    cases.push(format!(
                        "{} {}, label %{}",
                        llvm_type(&scrut_val.ty)?,
                        lit,
                        block_names[*target]
                    ));
                }
                self.terminate(format!(
                    "switch {} {}, label %{} [ {} ]",
                    llvm_type(&scrut_val.ty)?,
                    scrut_val.ir,
                    block_names[default_bb],
                    cases.join(" ")
                ));
                Ok(())
            }
            Terminator::Return { value } => {
                if let Some(expr) = value {
                    let val = self.emit_expr(expr)?;
                    self.emit_return_value(Some(val))
                } else {
                    if self.ret_type != Type::Builtin(BuiltinType::Unit) {
                        return Err("missing return value".to_string());
                    }
                    self.emit_return_value(None)
                }
            }
            Terminator::ReturnError { err } => {
                let val = self.emit_expr(err)?;
                self.emit_error_return(val.ir)
            }
        }
    }

    fn next_go_thunk_name(&mut self) -> String {
        let name = format!("__gost_go_thunk_{}", self.go_counter);
        self.go_counter += 1;
        name
    }

    fn emit_go_thunk_def(
        &mut self,
        thunk_name: &str,
        callee_name: &str,
        param_tys: &[Type],
        ctx_ty: &str,
    ) -> Result<(), String> {
        let mut lines = Vec::new();
        lines.push(format!("define void @{}(i8* %ctx) {{", thunk_name));
        lines.push("entry:".to_string());
        lines.push(format!(
            "  %ctx_cast = bitcast i8* %ctx to {}*",
            ctx_ty
        ));
        let mut arg_ir = Vec::new();
        for (idx, ty) in param_tys.iter().enumerate() {
            let storage_ty = llvm_storage_type(ty)?;
            let slot = format!("%slot{}", idx);
            lines.push(format!(
                "  {} = getelementptr {}, {}* %ctx_cast, i32 0, i32 {}",
                slot, ctx_ty, ctx_ty, idx
            ));
            if *ty == Type::Builtin(BuiltinType::Unit) {
                arg_ir.push(String::new());
                continue;
            }
            let tmp = format!("%arg{}", idx);
            lines.push(format!(
                "  {} = load {}, {}* {}",
                tmp, storage_ty, storage_ty, slot
            ));
            arg_ir.push(tmp);
        }
        let mut args_rendered = Vec::new();
        for (arg, ty) in arg_ir.iter().zip(param_tys.iter()) {
            if *ty == Type::Builtin(BuiltinType::Unit) {
                continue;
            }
            args_rendered.push(format!("{} {}", llvm_type(ty)?, arg));
        }
        let ret_ty = llvm_type(&self.fn_sigs.get(callee_name)
            .ok_or_else(|| format!("unknown function {}", callee_name))?
            .ret)?;
        if ret_ty == "void" {
            lines.push(format!(
                "  call {} @{}({})",
                ret_ty,
                callee_name,
                args_rendered.join(", ")
            ));
        } else {
            lines.push(format!(
                "  %_ = call {} @{}({})",
                ret_ty,
                callee_name,
                args_rendered.join(", ")
            ));
        }
        lines.push(format!(
            "  %size_ptr = getelementptr {}, {}* null, i32 1",
            ctx_ty, ctx_ty
        ));
        lines.push(format!(
            "  %size = ptrtoint {}* %size_ptr to i64",
            ctx_ty
        ));
        lines.push("  call void @__gost_free(i8* %ctx, i64 %size, i64 1)".to_string());
        lines.push("  ret void".to_string());
        lines.push("}".to_string());
        self.extra_functions.push(lines.join("\n"));
        Ok(())
    }

    fn emit_size_of(&mut self, ty: &Type) -> Result<Value, String> {
        let llvm_ty = llvm_type(ty)?;
        if llvm_ty == "void" {
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I64),
                ir: "0".to_string(),
            });
        }
        let ptr = self.new_temp();
        self.emit(format!(
            "{} = getelementptr {}, {}* null, i32 1",
            ptr, llvm_ty, llvm_ty
        ));
        let size = self.new_temp();
        self.emit(format!(
            "{} = ptrtoint {}* {} to i64",
            size, llvm_ty, ptr
        ));
        Ok(Value {
            ty: Type::Builtin(BuiltinType::I64),
            ir: size,
        })
    }

    fn emit_slice_obj_from_value(&mut self, value: &Value) -> Result<String, String> {
        match &value.ty {
            Type::Slice(_) | Type::Builtin(BuiltinType::Bytes) => {
                let obj = self.new_temp();
                self.emit(format!(
                    "{} = extractvalue %slice {}, 0",
                    obj, value.ir
                ));
                Ok(obj)
            }
            Type::Ref(inner) | Type::MutRef(inner) => match &**inner {
                Type::Slice(_) | Type::Builtin(BuiltinType::Bytes) => {
                    let tmp = self.new_temp();
                    self.emit(format!(
                        "{} = load %slice, %slice* {}",
                        tmp, value.ir
                    ));
                    let obj = self.new_temp();
                    self.emit(format!(
                        "{} = extractvalue %slice {}, 0",
                        obj, tmp
                    ));
                    Ok(obj)
                }
                _ => Err("expected ref to slice".to_string()),
            },
            _ => Err("expected slice".to_string()),
        }
    }

    fn emit_map_obj_from_value(&mut self, value: &Value) -> Result<String, String> {
        match &value.ty {
            Type::Map(_, _) => {
                let obj = self.new_temp();
                self.emit(format!(
                    "{} = extractvalue %map {}, 0",
                    obj, value.ir
                ));
                Ok(obj)
            }
            Type::Ref(inner) | Type::MutRef(inner) => match &**inner {
                Type::Map(_, _) => {
                    let tmp = self.new_temp();
                    self.emit(format!(
                        "{} = load %map, %map* {}",
                        tmp, value.ir
                    ));
                    let obj = self.new_temp();
                    self.emit(format!(
                        "{} = extractvalue %map {}, 0",
                        obj, tmp
                    ));
                    Ok(obj)
                }
                _ => Err("expected ref to map".to_string()),
            },
            _ => Err("expected map".to_string()),
        }
    }

    fn emit_shared_obj_from_value(&mut self, value: &Value) -> Result<String, String> {
        match &value.ty {
            Type::Shared(_) => {
                let obj = self.new_temp();
                self.emit(format!(
                    "{} = extractvalue %shared {}, 0",
                    obj, value.ir
                ));
                Ok(obj)
            }
            Type::Ref(inner) | Type::MutRef(inner) => match &**inner {
                Type::Shared(_) => {
                    let tmp = self.new_temp();
                    self.emit(format!(
                        "{} = load %shared, %shared* {}",
                        tmp, value.ir
                    ));
                    let obj = self.new_temp();
                    self.emit(format!(
                        "{} = extractvalue %shared {}, 0",
                        obj, tmp
                    ));
                    Ok(obj)
                }
                _ => Err("expected ref to shared".to_string()),
            },
            _ => Err("expected shared".to_string()),
        }
    }

    fn emit_shared_inc_value(&mut self, value: &Value) -> Result<(), String> {
        if let Type::Shared(_) = value.ty {
            let obj = self.emit_shared_obj_from_value(value)?;
            self.emit(format!("call void @__gost_shared_inc(%shared_obj* {})", obj));
        }
        Ok(())
    }

    fn emit_index_i64(&mut self, idx_val: &Value) -> Result<String, String> {
        match idx_val.ty {
            Type::Builtin(BuiltinType::I64) => Ok(idx_val.ir.clone()),
            Type::Builtin(BuiltinType::I32) => {
                let tmp = self.new_temp();
                self.emit(format!("{} = sext i32 {} to i64", tmp, idx_val.ir));
                Ok(tmp)
            }
            _ => Err("index expects i32 or i64".to_string()),
        }
    }

    fn emit_slice_elem_ptr(
        &mut self,
        slice_obj: &str,
        elem_ty: &Type,
        idx_val: &Value,
        bounds_check: bool,
    ) -> Result<String, String> {
        let idx_ir = self.emit_index_i64(idx_val)?;
        if bounds_check {
            self.emit(format!(
                "call void @__gost_slice_bounds_check(%slice_obj* {}, i64 {})",
                slice_obj, idx_ir
            ));
        }
        let data_ptr = self.new_temp();
        self.emit(format!(
            "{} = call i8* @__gost_slice_data(%slice_obj* {})",
            data_ptr, slice_obj
        ));
        let storage_ty = llvm_storage_type(elem_ty)?;
        let cast_ptr = self.new_temp();
        self.emit(format!(
            "{} = bitcast i8* {} to {}*",
            cast_ptr, data_ptr, storage_ty
        ));
        let elem_ptr = self.new_temp();
        self.emit(format!(
            "{} = getelementptr {}, {}* {}, i64 {}",
            elem_ptr, storage_ty, storage_ty, cast_ptr, idx_ir
        ));
        Ok(elem_ptr)
    }

    fn emit_size_of_llvm(&mut self, llvm_ty: &str) -> Result<Value, String> {
        if llvm_ty == "void" {
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I64),
                ir: "0".to_string(),
            });
        }
        let ptr = self.new_temp();
        self.emit(format!(
            "{} = getelementptr {}, {}* null, i32 1",
            ptr, llvm_ty, llvm_ty
        ));
        let size = self.new_temp();
        self.emit(format!(
            "{} = ptrtoint {}* {} to i64",
            size, llvm_ty, ptr
        ));
        Ok(Value {
            ty: Type::Builtin(BuiltinType::I64),
            ir: size,
        })
    }

    fn needs_drop(&self, ty: &Type) -> bool {
        match ty {
            Type::Builtin(BuiltinType::Bytes) => true,
            Type::Slice(_) | Type::Map(_, _) | Type::Chan(_) | Type::Shared(_) => true,
            Type::Tuple(items) => items.iter().any(|item| self.needs_drop(item)),
            Type::Named(name) => match self.types.get(name) {
                Some(TypeDefKind::Struct(def)) => def
                    .fields
                    .iter()
                    .any(|(_, field_ty)| self.needs_drop(field_ty)),
                Some(TypeDefKind::Enum(def)) => def.variants.iter().any(|(_, fields)| {
                    fields.iter().any(|field_ty| self.needs_drop(field_ty))
                }),
                None => false,
            },
            _ => false,
        }
    }

    fn next_drop_fn_name(&mut self) -> String {
        let name = format!("__gost_drop_fn_{}", self.drop_counter);
        self.drop_counter += 1;
        name
    }

    fn emit_drop_fn_ptr(&mut self, ty: &Type) -> Result<String, String> {
        if !self.needs_drop(ty) {
            return Ok("null".to_string());
        }
        let name = self.next_drop_fn_name();
        self.emit_drop_function(&name, ty)?;
        Ok(format!("@{}", name))
    }

    fn emit_drop_function(&mut self, name: &str, ty: &Type) -> Result<(), String> {
        let mut lines = Vec::new();
        let mut tmp_counter = 0usize;
        let mut next_tmp = || {
            let name = format!("%d{}", tmp_counter);
            tmp_counter += 1;
            name
        };
        lines.push(format!("define void @{}(i8* %ptr) {{", name));
        lines.push("entry:".to_string());
        self.emit_drop_impl(ty, "%ptr", &mut lines, &mut next_tmp)?;
        lines.push("  ret void".to_string());
        lines.push("}".to_string());
        self.extra_functions.push(lines.join("\n"));
        Ok(())
    }

    fn emit_drop_impl<F>(
        &mut self,
        ty: &Type,
        ptr: &str,
        lines: &mut Vec<String>,
        next_tmp: &mut F,
    ) -> Result<(), String>
    where
        F: FnMut() -> String,
    {
        if !self.needs_drop(ty) {
            return Ok(());
        }
        match ty {
            Type::Builtin(BuiltinType::Bytes) | Type::Slice(_) => {
                let cast = next_tmp();
                lines.push(format!(
                    "  {} = bitcast i8* {} to %slice*",
                    cast, ptr
                ));
                let val = next_tmp();
                lines.push(format!("  {} = load %slice, %slice* {}", val, cast));
                let obj = next_tmp();
                lines.push(format!(
                    "  {} = extractvalue %slice {}, 0",
                    obj, val
                ));
                lines.push(format!(
                    "  call void @__gost_slice_drop(%slice_obj* {})",
                    obj
                ));
            }
            Type::Map(_, _) => {
                let cast = next_tmp();
                lines.push(format!("  {} = bitcast i8* {} to %map*", cast, ptr));
                let val = next_tmp();
                lines.push(format!("  {} = load %map, %map* {}", val, cast));
                let obj = next_tmp();
                lines.push(format!("  {} = extractvalue %map {}, 0", obj, val));
                lines.push(format!(
                    "  call void @__gost_map_drop(%map_obj* {})",
                    obj
                ));
            }
            Type::Chan(_) => {
                let cast = next_tmp();
                lines.push(format!(
                    "  {} = bitcast i8* {} to %chan**",
                    cast, ptr
                ));
                let val = next_tmp();
                lines.push(format!("  {} = load %chan*, %chan** {}", val, cast));
                lines.push(format!("  call void @__gost_chan_drop(%chan* {})", val));
            }
            Type::Shared(_) => {
                let cast = next_tmp();
                lines.push(format!(
                    "  {} = bitcast i8* {} to %shared*",
                    cast, ptr
                ));
                let val = next_tmp();
                lines.push(format!(
                    "  {} = load %shared, %shared* {}",
                    val, cast
                ));
                let obj = next_tmp();
                lines.push(format!(
                    "  {} = extractvalue %shared {}, 0",
                    obj, val
                ));
                lines.push(format!(
                    "  call void @__gost_shared_dec(%shared_obj* {})",
                    obj
                ));
            }
            Type::Tuple(items) => {
                let storage_ty = llvm_type(ty)?;
                let cast = next_tmp();
                lines.push(format!(
                    "  {} = bitcast i8* {} to {}*",
                    cast, ptr, storage_ty
                ));
                for (idx, item_ty) in items.iter().enumerate() {
                    if !self.needs_drop(item_ty) {
                        continue;
                    }
                    let field_ptr = next_tmp();
                    lines.push(format!(
                        "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                        field_ptr, storage_ty, storage_ty, cast, idx
                    ));
                    let drop_fn = self.emit_drop_fn_ptr(item_ty)?;
                    if drop_fn != "null" {
                        let cast_ptr = next_tmp();
                        lines.push(format!(
                            "  {} = bitcast {}* {} to i8*",
                            cast_ptr,
                            llvm_type_for_tuple_elem(item_ty)?,
                            field_ptr
                        ));
                        lines.push(format!(
                            "  call void {}(i8* {})",
                            drop_fn, cast_ptr
                        ));
                    }
                }
            }
            Type::Named(name) => match self.types.get(name) {
                Some(TypeDefKind::Struct(def)) => {
                    let cast = next_tmp();
                    lines.push(format!(
                        "  {} = bitcast i8* {} to %{}*",
                        cast, ptr, name
                    ));
                    for (idx, (_, field_ty)) in def.fields.iter().enumerate() {
                        if !self.needs_drop(field_ty) {
                            continue;
                        }
                        let field_ptr = next_tmp();
                        lines.push(format!(
                            "  {} = getelementptr %{}, %{}* {}, i32 0, i32 {}",
                            field_ptr, name, name, cast, idx
                        ));
                        let drop_fn = self.emit_drop_fn_ptr(field_ty)?;
                        if drop_fn != "null" {
                            let cast_ptr = next_tmp();
                            lines.push(format!(
                                "  {} = bitcast {}* {} to i8*",
                                cast_ptr,
                                llvm_storage_type(field_ty)?,
                                field_ptr
                            ));
                            lines.push(format!(
                                "  call void {}(i8* {})",
                                drop_fn, cast_ptr
                            ));
                        }
                    }
                }
                Some(TypeDefKind::Enum(def)) => {
                    let cast = next_tmp();
                    lines.push(format!(
                        "  {} = bitcast i8* {} to %{}*",
                        cast, ptr, name
                    ));
                    let val = next_tmp();
                    lines.push(format!("  {} = load %{}, %{}* {}", val, name, name, cast));
                    let tag = next_tmp();
                    lines.push(format!(
                        "  {} = extractvalue %{} {}, 0",
                        tag, name, val
                    ));
                    let payload = next_tmp();
                    lines.push(format!(
                        "  {} = extractvalue %{} {}, 1",
                        payload, name, val
                    ));
                    let end_label = format!("drop_end_{}", name);
                    let mut cases = Vec::new();
                    for (idx, _) in def.variants.iter().enumerate() {
                        cases.push(format!("i32 {}, label %drop_case{}_{}", idx, name, idx));
                    }
                    lines.push(format!(
                        "  switch i32 {}, label %{} [ {} ]",
                        tag,
                        end_label,
                        cases.join(" ")
                    ));
                    for (idx, (_, fields)) in def.variants.iter().enumerate() {
                        let label = format!("drop_case{}_{}", name, idx);
                        lines.push(format!("{}:", label));
                        if !fields.is_empty() {
                            let payload_ty = self.enum_payload_type_name(name, idx);
                            let payload_ptr = next_tmp();
                            lines.push(format!(
                                "  {} = bitcast i8* {} to {}*",
                                payload_ptr, payload, payload_ty
                            ));
                            for (field_idx, field_ty) in fields.iter().enumerate() {
                                if !self.needs_drop(field_ty) {
                                    continue;
                                }
                                let field_ptr = next_tmp();
                                lines.push(format!(
                                    "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                                    field_ptr, payload_ty, payload_ty, payload_ptr, field_idx
                                ));
                                let drop_fn = self.emit_drop_fn_ptr(field_ty)?;
                                if drop_fn != "null" {
                                    let cast_ptr = next_tmp();
                                    lines.push(format!(
                                        "  {} = bitcast {}* {} to i8*",
                                        cast_ptr,
                                        llvm_storage_type(field_ty)?,
                                        field_ptr
                                    ));
                                    lines.push(format!(
                                        "  call void {}(i8* {})",
                                        drop_fn, cast_ptr
                                    ));
                                }
                            }
                            let size_ptr = next_tmp();
                            lines.push(format!(
                                "  {} = getelementptr {}, {}* null, i32 1",
                                size_ptr, payload_ty, payload_ty
                            ));
                            let size = next_tmp();
                            lines.push(format!(
                                "  {} = ptrtoint {}* {} to i64",
                                size, payload_ty, size_ptr
                            ));
                            lines.push(format!(
                                "  call void @__gost_free(i8* {}, i64 {}, i64 1)",
                                payload, size
                            ));
                        }
                        lines.push(format!("  br label %{}", end_label));
                    }
                    lines.push(format!("{}:", end_label));
                }
                None => {}
            },
            _ => {}
        }
        Ok(())
    }

    fn emit_place_ptr(&mut self, expr: &Expr) -> Result<(Type, String), String> {
        match &expr.kind {
            ExprKind::Ident(name) => {
                let (ty, ptr) = self
                    .locals
                    .get(name)
                    .cloned()
                    .ok_or_else(|| format!("unknown local {}", name))?;
                Ok((ty, ptr))
            }
            ExprKind::Deref { expr: inner } => {
                let inner_val = self.emit_expr(inner)?;
                match inner_val.ty {
                    Type::Ref(inner) | Type::MutRef(inner) => Ok((*inner, inner_val.ir)),
                    _ => Err("deref expects ref type".to_string()),
                }
            }
            ExprKind::Field { base, name } => {
                let (base_ty, base_ptr) = match self.emit_place_ptr(base) {
                    Ok((ty, ptr)) => (ty, ptr),
                    Err(_) => {
                        let base_val = self.emit_expr(base)?;
                        let tmp_ptr = self.materialize_value(&base_val)?;
                        (base_val.ty, tmp_ptr)
                    }
                };
                let (struct_name, struct_ptr) = match base_ty {
                    Type::Named(name) => (name, base_ptr),
                    _ => return Err("field access expects struct".to_string()),
                };
                let (field_idx, field_ty) =
                    self.resolve_struct_field(&struct_name, name)?;
                let gep = self.new_temp();
                self.emit(format!(
                    "{} = getelementptr %{}, %{}* {}, i32 0, i32 {}",
                    gep, struct_name, struct_name, struct_ptr, field_idx
                ));
                Ok((field_ty, gep))
            }
            ExprKind::Index { base, index } => {
                let base_val = self.emit_expr(base)?;
                let elem_ty = match &base_val.ty {
                    Type::Slice(inner) => *inner.clone(),
                    Type::Builtin(BuiltinType::Bytes) => Type::Builtin(BuiltinType::U32),
                    Type::Ref(inner) | Type::MutRef(inner) => match &**inner {
                        Type::Slice(elem) => *elem.clone(),
                        Type::Builtin(BuiltinType::Bytes) => Type::Builtin(BuiltinType::U32),
                        _ => return Err("indexing expects a slice".to_string()),
                    },
                    _ => return Err("indexing expects a slice".to_string()),
                };
                let idx_val = self.emit_expr(index)?;
                let slice_obj = self.emit_slice_obj_from_value(&base_val)?;
                let elem_ptr =
                    self.emit_slice_elem_ptr(&slice_obj, &elem_ty, &idx_val, true)?;
                Ok((elem_ty, elem_ptr))
            }
            _ => Err("expression is not addressable".to_string()),
        }
    }

    fn resolve_enum_variant(
        &self,
        enum_name: &str,
        variant: &str,
    ) -> Result<(usize, Vec<Type>), String> {
        match self.types.get(enum_name) {
            Some(TypeDefKind::Enum(def)) => def
                .variants
                .iter()
                .enumerate()
                .find(|(_, (name, _))| name == variant)
                .map(|(idx, (_, fields))| (idx, fields.clone()))
                .ok_or_else(|| format!("unknown variant `{}` on {}", variant, enum_name)),
            _ => Err("enum constructor expects enum".to_string()),
        }
    }

    fn enum_payload_type_name(&self, enum_name: &str, idx: usize) -> String {
        format!("%{}$payload{}", enum_name, idx)
    }

    fn emit_enum_value(
        &mut self,
        enum_name: &str,
        tag: usize,
        payload_ptr: Option<String>,
    ) -> Result<Value, String> {
        let enum_ty = Type::Named(enum_name.to_string());
        let agg0 = self.new_temp();
        self.emit(format!(
            "{} = insertvalue {} undef, i32 {}, 0",
            agg0,
            llvm_type(&enum_ty)?,
            tag
        ));
        let payload_ir = if let Some(ptr) = payload_ptr {
            let cast = self.new_temp();
            self.emit(format!(
                "{} = bitcast {}* {} to i8*",
                cast,
                self.enum_payload_type_name(enum_name, tag),
                ptr
            ));
            cast
        } else {
            "null".to_string()
        };
        let agg1 = self.new_temp();
        self.emit(format!(
            "{} = insertvalue {} {}, i8* {}, 1",
            agg1,
            llvm_type(&enum_ty)?,
            agg0,
            payload_ir
        ));
        Ok(Value {
            ty: enum_ty,
            ir: agg1,
        })
    }

    fn emit_enum_ctor(
        &mut self,
        enum_name: &str,
        variant: &str,
        args: &[Expr],
    ) -> Result<Value, String> {
        let (variant_idx, field_tys) = self.resolve_enum_variant(enum_name, variant)?;
        if field_tys.len() != args.len() {
            return Err("enum constructor arity mismatch".to_string());
        }
        let mut arg_vals = Vec::new();
        for arg in args {
            arg_vals.push(self.emit_expr(arg)?);
        }
        let payload_ptr = if field_tys.is_empty() {
            None
        } else {
            let payload_ty = self.enum_payload_type_name(enum_name, variant_idx);
            let size_val = self.emit_size_of_llvm(&payload_ty)?;
            let raw = self.new_temp();
            self.emit(format!(
                "{} = call i8* @__gost_alloc(i64 {}, i64 1)",
                raw, size_val.ir
            ));
            let payload_ptr = self.new_temp();
            self.emit(format!(
                "{} = bitcast i8* {} to {}*",
                payload_ptr, raw, payload_ty
            ));
            for (idx, field_ty) in field_tys.iter().enumerate() {
                let field_ptr = self.new_temp();
                self.emit(format!(
                    "{} = getelementptr {}, {}* {}, i32 0, i32 {}",
                    field_ptr, payload_ty, payload_ty, payload_ptr, idx
                ));
                let storage_ty = llvm_type_for_tuple_elem(field_ty)?;
                if *field_ty == Type::Builtin(BuiltinType::Unit) {
                    self.emit(format!(
                        "store {} 0, {}* {}",
                        storage_ty, storage_ty, field_ptr
                    ));
                    continue;
                }
                let val = arg_vals
                    .get(idx)
                    .ok_or_else(|| "missing enum argument".to_string())?;
                if matches!(field_ty, Type::Shared(_)) {
                    self.emit_shared_inc_value(val)?;
                }
                self.emit(format!(
                    "store {} {}, {}* {}",
                    storage_ty, val.ir, storage_ty, field_ptr
                ));
            }
            Some(payload_ptr)
        };
        self.emit_enum_value(enum_name, variant_idx, payload_ptr)
    }

    fn resolve_struct_field(
        &self,
        type_name: &str,
        field: &str,
    ) -> Result<(usize, Type), String> {
        match self.types.get(type_name) {
            Some(TypeDefKind::Struct(def)) => def
                .fields
                .iter()
                .enumerate()
                .find(|(_, (name, _))| name == field)
                .map(|(idx, (_, ty))| (idx, ty.clone()))
                .ok_or_else(|| format!("unknown field `{}` on {}", field, type_name)),
            _ => Err("field access expects struct".to_string()),
        }
    }

    fn materialize_value(&mut self, value: &Value) -> Result<String, String> {
        let storage_ty = llvm_storage_type(&value.ty)?;
        let alloca = self.new_temp();
        self.emit(format!("{} = alloca {}", alloca, storage_ty));
        if value.ty != Type::Builtin(BuiltinType::Unit) {
            self.emit(format!(
                "store {} {}, {}* {}",
                llvm_type(&value.ty)?,
                value.ir,
                storage_ty,
                alloca
            ));
        } else {
            self.emit(format!("store i8 0, i8* {}", alloca));
        }
        Ok(alloca)
    }

    fn emit_send_with_value(
        &mut self,
        chan_val: &Value,
        elem_ty: &Type,
        value_expr: &Expr,
    ) -> Result<(), String> {
        let val = self.emit_expr(value_expr)?;
        let storage_ty = llvm_storage_type(elem_ty)?;
        let alloca = self.new_temp();
        self.emit(format!("{} = alloca {}", alloca, storage_ty));
        if *elem_ty != Type::Builtin(BuiltinType::Unit) {
            self.emit(format!(
                "store {} {}, {}* {}",
                llvm_type(elem_ty)?,
                val.ir,
                storage_ty,
                alloca
            ));
        } else {
            self.emit(format!("store i8 0, i8* {}", alloca));
        }
        let elem_ptr = self.new_temp();
        self.emit(format!(
            "{} = bitcast {}* {} to i8*",
            elem_ptr, storage_ty, alloca
        ));
        let status = self.new_temp();
        self.emit(format!(
            "{} = call i32 @__gost_chan_send(%chan* {}, i8* {})",
            status, chan_val.ir, elem_ptr
        ));
        let is_closed = self.new_temp();
        self.emit(format!("{} = icmp eq i32 {}, 1", is_closed, status));
        let closed_name = self.new_block("send_closed");
        let ok_name = self.new_block("send_ok");
        let closed_idx = self.add_block(closed_name.clone());
        let ok_idx = self.add_block(ok_name.clone());
        self.terminate(format!(
            "br i1 {}, label %{}, label %{}",
            is_closed, closed_name, ok_name
        ));
        self.switch_to(closed_idx);
        self.emit_panic_str("send on closed channel")?;
        self.switch_to(ok_idx);
        Ok(())
    }

    fn emit_panic_str(&mut self, msg: &str) -> Result<(), String> {
        let value = self.emit_string_literal(msg)?;
        let ptr = self.new_temp();
        let len = self.new_temp();
        self.emit(format!("{} = extractvalue %string {}, 0", ptr, value.ir));
        self.emit(format!("{} = extractvalue %string {}, 1", len, value.ir));
        self.emit(format!("call void @__gost_panic(i8* {}, i64 {})", ptr, len));
        self.terminate("unreachable");
        Ok(())
    }

    fn resolve_type_ast(&self, ast: &TypeAst) -> Result<Type, String> {
        match &ast.kind {
            crate::frontend::ast::TypeAstKind::Named(name) => {
                if let Some(ty) = builtin_from_name(name) {
                    Ok(ty)
                } else {
                    Ok(Type::Named(name.clone()))
                }
            }
            crate::frontend::ast::TypeAstKind::Ref(inner) => {
                Ok(Type::Ref(Box::new(self.resolve_type_ast(inner)?)))
            }
            crate::frontend::ast::TypeAstKind::MutRef(inner) => {
                Ok(Type::MutRef(Box::new(self.resolve_type_ast(inner)?)))
            }
            crate::frontend::ast::TypeAstKind::Slice(inner) => {
                Ok(Type::Slice(Box::new(self.resolve_type_ast(inner)?)))
            }
            crate::frontend::ast::TypeAstKind::Map(key, value) => Ok(Type::Map(
                Box::new(self.resolve_type_ast(key)?),
                Box::new(self.resolve_type_ast(value)?),
            )),
            crate::frontend::ast::TypeAstKind::Chan(inner) => {
                Ok(Type::Chan(Box::new(self.resolve_type_ast(inner)?)))
            }
            crate::frontend::ast::TypeAstKind::Shared(inner) => {
                Ok(Type::Shared(Box::new(self.resolve_type_ast(inner)?)))
            }
            crate::frontend::ast::TypeAstKind::Interface => Ok(Type::Interface),
            crate::frontend::ast::TypeAstKind::Tuple(items) => {
                let mut tys = Vec::new();
                for item in items {
                    tys.push(self.resolve_type_ast(item)?);
                }
                Ok(Type::Tuple(tys))
            }
        }
    }

    fn emit_string_literal(&mut self, value: &str) -> Result<Value, String> {
        let name = format!("@.str.{}.{}", self.fn_name, self.string_count);
        self.string_count += 1;
        let bytes = value.as_bytes();
        let mut encoded = String::new();
        for b in bytes {
            encoded.push_str(&format!("\\{:02X}", b));
        }
        encoded.push_str("\\00");
        let len = bytes.len() as u64;
        let array_len = bytes.len() + 1;
        let global = format!(
            "{} = private constant [{} x i8] c\"{}\"",
            name, array_len, encoded
        );
        self.string_literals.push(global);
        let ptr = self.new_temp();
        self.emit(format!(
            "{} = getelementptr inbounds [{} x i8], [{} x i8]* {}, i32 0, i32 0",
            ptr, array_len, array_len, name
        ));
        let tmp = self.new_temp();
        self.emit(format!("{} = insertvalue %string undef, i8* {}, 0", tmp, ptr));
        let str_val = self.new_temp();
        self.emit(format!(
            "{} = insertvalue %string {}, i64 {}, 1",
            str_val, tmp, len
        ));
        Ok(Value {
            ty: Type::Builtin(BuiltinType::String),
            ir: str_val,
        })
    }

    fn is_linear_type(&self, ty: &Type) -> bool {
        matches!(self.types.classify(ty), Some(TypeClass::Linear))
    }

    fn mark_moved(&mut self, name: &str) {
        if let Some(state) = self.linear_states.get_mut(name) {
            *state = false;
        }
    }

    fn mark_assigned(&mut self, name: &str) {
        if let Some(state) = self.linear_states.get_mut(name) {
            *state = true;
        }
    }

    fn emit_drop_for_ptr(&mut self, ty: &Type, ptr: &str) -> Result<(), String> {
        let drop_fn = self.emit_drop_fn_ptr(ty)?;
        if drop_fn == "null" {
            return Ok(());
        }
        let cast = self.new_temp();
        self.emit(format!(
            "{} = bitcast {}* {} to i8*",
            cast,
            llvm_storage_type(ty)?,
            ptr
        ));
        self.emit(format!("call void {}(i8* {})", drop_fn, cast));
        Ok(())
    }

    fn emit_drop_all_scopes_no_pop(&mut self) -> Result<(), String> {
        let deferred = self.deferred.clone();
        for defers in deferred.iter().rev() {
            self.emit_deferred_list(defers)?;
        }
        Ok(())
    }

    fn emit_deferred_list(&mut self, defers: &[DeferredCall]) -> Result<(), String> {
        for deferred in defers.iter().rev() {
            let mut args = Vec::new();
            for (ty, ptr) in &deferred.args {
                if *ty == Type::Builtin(BuiltinType::Unit) {
                    args.push(Value {
                        ty: ty.clone(),
                        ir: String::new(),
                    });
                    continue;
                }
                let tmp = self.new_temp();
                self.emit(format!(
                    "{} = load {}, {}* {}",
                    tmp,
                    llvm_type(ty)?,
                    llvm_type(ty)?,
                    ptr
                ));
                args.push(Value { ty: ty.clone(), ir: tmp });
            }
            let _ = self.emit_call_by_name(&deferred.name, &deferred.type_args, args)?;
        }
        Ok(())
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Vec::new());
        self.deferred.push(Vec::new());
    }

    fn exit_scope(&mut self) -> Result<(), String> {
        let emit_runtime = !self.mir_mode;
        if let Some(defers) = self.deferred.pop() {
            if emit_runtime {
                self.emit_deferred_list(&defers)?;
            }
        }
        if let Some(names) = self.scopes.pop() {
            if emit_runtime {
                for name in names.iter().rev() {
                    if let Some((ty, ptr)) = self.locals.get(name).cloned() {
                        let should_drop = if self.is_linear_type(&ty) {
                            self.linear_states.get(name).copied().unwrap_or(true)
                        } else {
                            true
                        };
                        if should_drop {
                            self.emit_drop_for_ptr(&ty, &ptr)?;
                        }
                    }
                }
            }
            for name in names.iter().rev() {
                self.locals.remove(name);
                self.linear_states.remove(name);
            }
        }
        Ok(())
    }
}
