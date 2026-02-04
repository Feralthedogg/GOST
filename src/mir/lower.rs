use crate::frontend::ast::{
    BinaryOp, Block, Expr, ExprKind, Item, Pattern, Span, Stmt, TypeAst, TypeAstKind,
};
use std::collections::HashMap;

use crate::frontend::ast::{BlockOrExpr, ExprId};
use crate::sema::types::{BuiltinType, Type, TypeClass, TypeDefs};
use crate::sema::Program;
use super::{
    BasicBlock, CleanupItem, Local, LocalId, MirFunction, MirModule, MirStmt, ScopeFrame, ScopeId,
    Terminator,
};

pub fn lower_program(program: &Program) -> Result<MirModule, String> {
    let mut module = MirModule::default();
    let mut next_expr_id = max_expr_id(&program.file) + 1;
    for item in &program.file.items {
        if let Item::Function(func) = item {
            let sig = program
                .functions
                .get(&func.name)
                .ok_or_else(|| format!("missing signature for {}", func.name))?;
            let mut locals = Vec::new();
            for (idx, param) in sig.params.iter().enumerate() {
                let class = program
                    .types
                    .classify(param)
                    .ok_or_else(|| "unknown type".to_string())?;
                locals.push(Local {
                    name: func.params.get(idx).map(|p| p.name.clone()),
                    ty: param.clone(),
                    class,
                });
            }
            let mut lowerer = Lowerer::new(
                sig.ret.clone(),
                locals,
                &program.types,
                &program.expr_types,
                next_expr_id,
            );
            lowerer.lower_block(&func.body, true)?;
            if !lowerer.is_terminated(lowerer.current) {
                lowerer.set_terminator(Terminator::Return { value: None });
            }
            next_expr_id = lowerer.next_expr_id;
            module.functions.push(MirFunction {
                name: func.name.clone(),
                ret_ty: sig.ret.clone(),
                param_count: lowerer.param_count,
                locals: lowerer.locals,
                expr_types: lowerer.synthetic_expr_types.clone(),
                blocks: lowerer.blocks,
                block_depths: lowerer.block_depths,
                scopes: lowerer.scopes,
                block_scopes: lowerer.block_scopes,
                block_exit_scopes: lowerer.block_exit_scopes,
                entry: lowerer.entry,
            });
        }
    }
    Ok(module)
}

struct Lowerer {
    ret_ty: Type,
    locals: Vec<Local>,
    blocks: Vec<BasicBlock>,
    block_depths: Vec<isize>,
    terminated: Vec<bool>,
    current: usize,
    entry: usize,
    loop_stack: Vec<(usize, usize)>,
    defs: TypeDefs,
    expr_types: HashMap<ExprId, Type>,
    next_temp: usize,
    next_expr_id: ExprId,
    synthetic_expr_types: HashMap<ExprId, Type>,
    current_depth: isize,
    scopes: Vec<ScopeFrame>,
    current_scope: Option<ScopeId>,
    block_scopes: Vec<Option<ScopeId>>,
    block_exit_scopes: Vec<Option<ScopeId>>,
    param_count: usize,
    name_bindings: HashMap<String, Vec<LocalId>>,
    scope_bindings: Vec<Vec<String>>,
}

#[derive(Copy, Clone, Debug)]
enum ForInMode {
    Value,
    Ref,
    MutRef,
}

#[derive(Clone, Debug)]
struct IterChain {
    base_expr: Expr,
    base_span: Span,
    stages: Vec<IterStage>,
}

#[derive(Clone, Debug)]
enum IterStage {
    Filter { func: String, span: Span },
    Map { func: String, out_ty: Type, span: Span },
}

impl Lowerer {
    fn new(
        ret_ty: Type,
        locals: Vec<Local>,
        defs: &TypeDefs,
        expr_types: &HashMap<ExprId, Type>,
        next_expr_id: ExprId,
    ) -> Self {
        let entry = 0;
        let blocks = vec![BasicBlock::new(Terminator::Return { value: None })];
        let param_count = locals.len();
        let mut name_bindings = HashMap::new();
        for (idx, local) in locals.iter().enumerate() {
            if let Some(name) = &local.name {
                name_bindings.insert(name.clone(), vec![idx]);
            }
        }
        Self {
            ret_ty,
            locals,
            blocks,
            block_depths: vec![0],
            terminated: vec![false],
            current: entry,
            entry,
            loop_stack: Vec::new(),
            defs: defs.clone(),
            expr_types: expr_types.clone(),
            next_temp: 0,
            next_expr_id,
            synthetic_expr_types: HashMap::new(),
            current_depth: 0,
            scopes: Vec::new(),
            current_scope: None,
            block_scopes: vec![None],
            block_exit_scopes: vec![None],
            param_count,
            name_bindings,
            scope_bindings: Vec::new(),
        }
    }

    fn lower_select_stmt(
        &mut self,
        arms: &[crate::frontend::ast::SelectArm],
    ) -> Result<(), String> {
        self.ensure_open_block();

        #[derive(Clone)]
        struct SelectCase {
            kind: crate::frontend::ast::SelectArmKind,
            chan_name: Option<String>,
            chan_ty: Option<Type>,
            elem_ty: Option<Type>,
            body: BlockOrExpr,
            span: Span,
        }

        let mut cases = Vec::new();
        let mut default_index = None;

        for (idx, arm) in arms.iter().enumerate() {
            match &arm.kind {
                crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                    let chan_expr = self.lower_expr_value(chan)?;
                    let chan_ty = self.expr_type(&chan_expr)?;
                    let elem_ty = match &chan_ty {
                        Type::Chan(inner) => *inner.clone(),
                        _ => return Err("select send expects chan[T]".to_string()),
                    };
                    let (chan_local, chan_name) = self.new_temp_local(chan_ty.clone())?;
                    self.blocks[self.current].stmts.push(MirStmt::Eval {
                        expr: chan_expr,
                        out: vec![chan_local],
                    });
                    cases.push(SelectCase {
                        kind: crate::frontend::ast::SelectArmKind::Send {
                            chan: chan.clone(),
                            value: value.clone(),
                        },
                        chan_name: Some(chan_name),
                        chan_ty: Some(chan_ty),
                        elem_ty: Some(elem_ty),
                        body: arm.body.clone(),
                        span: arm.span.clone(),
                    });
                }
                crate::frontend::ast::SelectArmKind::Recv { chan, bind } => {
                    let chan_expr = self.lower_expr_value(chan)?;
                    let chan_ty = self.expr_type(&chan_expr)?;
                    let elem_ty = match &chan_ty {
                        Type::Chan(inner) => *inner.clone(),
                        _ => return Err("select recv expects chan[T]".to_string()),
                    };
                    let (chan_local, chan_name) = self.new_temp_local(chan_ty.clone())?;
                    self.blocks[self.current].stmts.push(MirStmt::Eval {
                        expr: chan_expr,
                        out: vec![chan_local],
                    });
                    cases.push(SelectCase {
                        kind: crate::frontend::ast::SelectArmKind::Recv {
                            chan: chan.clone(),
                            bind: bind.clone(),
                        },
                        chan_name: Some(chan_name),
                        chan_ty: Some(chan_ty),
                        elem_ty: Some(elem_ty),
                        body: arm.body.clone(),
                        span: arm.span.clone(),
                    });
                }
                crate::frontend::ast::SelectArmKind::After { ms } => {
                    let ms_expr = self.lower_expr_value(ms)?;
                    let after_expr = self.new_expr(
                        ExprKind::After {
                            ms: Box::new(ms_expr),
                        },
                        arm.span.clone(),
                        Some(Type::Chan(Box::new(Type::Builtin(BuiltinType::Unit)))),
                    );
                    let chan_ty = Type::Chan(Box::new(Type::Builtin(BuiltinType::Unit)));
                    let (chan_local, chan_name) = self.new_temp_local(chan_ty.clone())?;
                    self.blocks[self.current].stmts.push(MirStmt::Eval {
                        expr: after_expr,
                        out: vec![chan_local],
                    });
                    cases.push(SelectCase {
                        kind: crate::frontend::ast::SelectArmKind::Recv {
                            chan: ms.clone(),
                            bind: None,
                        },
                        chan_name: Some(chan_name),
                        chan_ty: Some(chan_ty),
                        elem_ty: Some(Type::Builtin(BuiltinType::Unit)),
                        body: arm.body.clone(),
                        span: arm.span.clone(),
                    });
                }
                crate::frontend::ast::SelectArmKind::Default => {
                    default_index = Some(idx);
                    cases.push(SelectCase {
                        kind: crate::frontend::ast::SelectArmKind::Default,
                        chan_name: None,
                        chan_ty: None,
                        elem_ty: None,
                        body: arm.body.clone(),
                        span: arm.span.clone(),
                    });
                }
            }
        }

        let head_bb = self.new_block();
        let exit_bb = self.new_block();
        let wait_bb = if default_index.is_none() {
            Some(self.new_block())
        } else {
            None
        };

        let mut case_blocks = Vec::new();
        for _ in 0..cases.len() {
            case_blocks.push(self.new_block());
        }
        let default_bb = default_index.map(|idx| case_blocks[idx]);

        self.set_terminator(Terminator::Goto(head_bb));

        let non_default_indices: Vec<usize> = cases
            .iter()
            .enumerate()
            .filter_map(|(idx, c)| match c.kind {
                crate::frontend::ast::SelectArmKind::Default => None,
                _ => Some(idx),
            })
            .collect();

        let mut check_bb = head_bb;
        for (pos, case_idx) in non_default_indices.iter().enumerate() {
            let is_last = pos + 1 == non_default_indices.len();
            let next_bb = if !is_last {
                self.new_block()
            } else if let Some(default_bb) = default_bb {
                default_bb
            } else {
                wait_bb.ok_or_else(|| "select wait block missing".to_string())?
            };
            self.set_current(check_bb);
            let case = &cases[*case_idx];
            let chan_name = case
                .chan_name
                .as_ref()
                .ok_or_else(|| "select case missing channel".to_string())?;
            let chan_ty = case
                .chan_ty
                .as_ref()
                .ok_or_else(|| "select case missing channel type".to_string())?;
            let chan_ident = self.ident_expr(chan_name.clone(), case.span.clone(), chan_ty.clone());
            let (call_name, call_ty) = match case.kind {
                crate::frontend::ast::SelectArmKind::Send { .. } => {
                    ("__gost_chan_can_send", BuiltinType::I32)
                }
                _ => ("__gost_chan_can_recv", BuiltinType::I32),
            };
            let call_ty = call_ty.clone();
            let callee_ident = self.new_expr(
                ExprKind::Ident(call_name.to_string()),
                case.span.clone(),
                None,
            );
            let call_expr = self.new_expr(
                ExprKind::Call {
                    callee: Box::new(callee_ident),
                    type_args: Vec::new(),
                    args: vec![chan_ident],
                },
                case.span.clone(),
                Some(Type::Builtin(call_ty.clone())),
            );
            let zero_expr = self.int_expr(0, case.span.clone(), Type::Builtin(call_ty));
            let cond_expr = self.new_expr(
                ExprKind::Binary {
                    op: BinaryOp::NotEq,
                    left: Box::new(call_expr),
                    right: Box::new(zero_expr),
                },
                case.span.clone(),
                Some(Type::Builtin(BuiltinType::Bool)),
            );
            self.set_terminator(Terminator::If {
                cond: cond_expr,
                then_bb: case_blocks[*case_idx],
                else_bb: next_bb,
            });
            check_bb = next_bb;
        }

        if non_default_indices.is_empty() {
            if let Some(default_bb) = default_bb {
                self.set_current(head_bb);
                self.set_terminator(Terminator::Goto(default_bb));
            } else if let Some(wait_bb) = wait_bb {
                self.set_current(head_bb);
                self.set_terminator(Terminator::Goto(wait_bb));
            }
        }

        for (idx, case) in cases.iter().enumerate() {
            let case_bb = case_blocks[idx];
            self.set_current(case_bb);
            match &case.kind {
                crate::frontend::ast::SelectArmKind::Send { value, .. } => {
                    let chan_name = case
                        .chan_name
                        .as_ref()
                        .ok_or_else(|| "select send missing channel".to_string())?;
                    let chan_ty = case
                        .chan_ty
                        .as_ref()
                        .ok_or_else(|| "select send missing channel type".to_string())?;
                    let chan_ident =
                        self.ident_expr(chan_name.clone(), case.span.clone(), chan_ty.clone());
                    let val_expr = self.lower_expr_value(value)?;
                    let send_expr = self.new_expr(
                        ExprKind::Send {
                            chan: Box::new(chan_ident),
                            value: Box::new(val_expr),
                        },
                        case.span.clone(),
                        Some(Type::Builtin(BuiltinType::Unit)),
                    );
                    self.blocks[self.current]
                        .stmts
                        .push(MirStmt::Expr { expr: send_expr });
                }
                crate::frontend::ast::SelectArmKind::Recv { bind, .. } => {
                    let chan_name = case
                        .chan_name
                        .as_ref()
                        .ok_or_else(|| "select recv missing channel".to_string())?;
                    let chan_ty = case
                        .chan_ty
                        .as_ref()
                        .ok_or_else(|| "select recv missing channel type".to_string())?;
                    let elem_ty = case
                        .elem_ty
                        .as_ref()
                        .ok_or_else(|| "select recv missing elem type".to_string())?;
                    let chan_ident =
                        self.ident_expr(chan_name.clone(), case.span.clone(), chan_ty.clone());
                    let recv_expr = self.new_expr(
                        ExprKind::Recv {
                            chan: Box::new(chan_ident),
                        },
                        case.span.clone(),
                        Some(Type::Tuple(vec![
                            elem_ty.clone(),
                            Type::Builtin(BuiltinType::Bool),
                        ])),
                    );
                    let mut prefix_stmts = Vec::new();
                    if let Some((val_name, ok_name)) = bind {
                        let (val_local, val_tmp) = self.new_temp_local(elem_ty.clone())?;
                        let (ok_local, ok_tmp) =
                            self.new_temp_local(Type::Builtin(BuiltinType::Bool))?;
                        self.blocks[self.current].stmts.push(MirStmt::Eval {
                            expr: recv_expr,
                            out: vec![val_local, ok_local],
                        });
                        prefix_stmts.push(Stmt::Let {
                            name: val_name.clone(),
                            ty: None,
                            init: self.ident_expr(val_tmp, case.span.clone(), elem_ty.clone()),
                            span: case.span.clone(),
                        });
                        prefix_stmts.push(Stmt::Let {
                            name: ok_name.clone(),
                            ty: None,
                            init: self.ident_expr(
                                ok_tmp,
                                case.span.clone(),
                                Type::Builtin(BuiltinType::Bool),
                            ),
                            span: case.span.clone(),
                        });
                    } else {
                        let tuple_ty = Type::Tuple(vec![
                            elem_ty.clone(),
                            Type::Builtin(BuiltinType::Bool),
                        ]);
                        let (tmp_local, _tmp_name) = self.new_temp_local(tuple_ty)?;
                        self.blocks[self.current].stmts.push(MirStmt::Eval {
                            expr: recv_expr,
                            out: vec![tmp_local],
                        });
                    }

                    let mut body_block = match &case.body {
                        BlockOrExpr::Block(block) => block.as_ref().clone(),
                        BlockOrExpr::Expr(expr) => Block {
                            stmts: Vec::new(),
                            tail: Some(expr.clone()),
                            span: case.span.clone(),
                        },
                    };
                    if !prefix_stmts.is_empty() {
                        let mut new_stmts = prefix_stmts;
                        new_stmts.extend(body_block.stmts);
                        body_block.stmts = new_stmts;
                    }
                    self.lower_block(&body_block, false)?;
                    if !self.is_terminated(self.current) {
                        self.set_terminator(Terminator::Goto(exit_bb));
                    }
                    continue;
                }
                crate::frontend::ast::SelectArmKind::Default => {}
                crate::frontend::ast::SelectArmKind::After { .. } => {}
            }

            let body_block = match &case.body {
                BlockOrExpr::Block(block) => block.as_ref().clone(),
                BlockOrExpr::Expr(expr) => Block {
                    stmts: Vec::new(),
                    tail: Some(expr.clone()),
                    span: case.span.clone(),
                },
            };
            self.lower_block(&body_block, false)?;
            if !self.is_terminated(self.current) {
                self.set_terminator(Terminator::Goto(exit_bb));
            }
        }

        if let Some(wait_bb) = wait_bb {
            self.set_current(wait_bb);
            let wait_span = Span { start: 0, end: 0, line: 0, column: 0 };
            let wait_callee = self.new_expr(
                ExprKind::Ident("__gost_select_wait".to_string()),
                wait_span.clone(),
                None,
            );
            let mut wait_args = Vec::new();
            for case_idx in &non_default_indices {
                let case = &cases[*case_idx];
                let chan_name = case
                    .chan_name
                    .as_ref()
                    .ok_or_else(|| "select wait missing channel".to_string())?;
                let chan_ty = case
                    .chan_ty
                    .as_ref()
                    .ok_or_else(|| "select wait missing channel type".to_string())?;
                wait_args.push(self.ident_expr(
                    chan_name.clone(),
                    case.span.clone(),
                    chan_ty.clone(),
                ));
            }
            let wait_expr = self.new_expr(
                ExprKind::Call {
                    callee: Box::new(wait_callee),
                    type_args: Vec::new(),
                    args: wait_args,
                },
                wait_span,
                Some(Type::Builtin(BuiltinType::I32)),
            );
            self.blocks[self.current]
                .stmts
                .push(MirStmt::Expr { expr: wait_expr });
            self.set_terminator(Terminator::Goto(head_bb));
        }

        self.set_current(exit_bb);
        Ok(())
    }

    fn new_block(&mut self) -> usize {
        let id = self.blocks.len();
        self.blocks.push(BasicBlock::new(Terminator::Return { value: None }));
        self.block_depths.push(self.current_depth);
        self.block_scopes.push(self.current_scope);
        self.block_exit_scopes.push(None);
        self.terminated.push(false);
        id
    }

    fn is_terminated(&self, block: usize) -> bool {
        *self.terminated.get(block).unwrap_or(&false)
    }

    fn set_terminator(&mut self, term: Terminator) {
        let block = &mut self.blocks[self.current];
        block.term = term;
        self.terminated[self.current] = true;
        if let Some(slot) = self.block_exit_scopes.get_mut(self.current) {
            *slot = self.current_scope;
        }
    }

    fn ensure_open_block(&mut self) {
        if self.is_terminated(self.current) {
            let block = self.new_block();
            self.set_current(block);
        }
    }

    fn set_current(&mut self, block: usize) {
        self.current = block;
        if let Some(depth) = self.block_depths.get(block).copied() {
            self.current_depth = depth;
        }
        if let Some(scope) = self.block_scopes.get(block).copied() {
            self.current_scope = scope;
        }
    }

    fn push_scope(&mut self) {
        let depth = self.current_depth + 1;
        let scope_id = self.scopes.len();
        self.scopes.push(ScopeFrame {
            parent: self.current_scope,
            depth,
            items: Vec::new(),
        });
        self.scope_bindings.push(Vec::new());
        self.current_scope = Some(scope_id);
        self.current_depth = depth;
        self.blocks[self.current]
            .stmts
            .push(MirStmt::EnterScope { scope: scope_id });
    }

    fn pop_scope(&mut self) {
        if let Some(scope_id) = self.current_scope {
            self.blocks[self.current]
                .stmts
                .push(MirStmt::ExitScope { scope: scope_id });
            let parent = self.scopes.get(scope_id).and_then(|frame| frame.parent);
            self.current_scope = parent;
            self.current_depth = parent
                .and_then(|id| self.scopes.get(id).map(|frame| frame.depth))
                .unwrap_or(0);
        }
        self.pop_scope_bindings_only();
    }

    fn pop_scope_bindings_only(&mut self) {
        if let Some(names) = self.scope_bindings.pop() {
            for name in names.into_iter().rev() {
                if let Some(stack) = self.name_bindings.get_mut(&name) {
                    stack.pop();
                    if stack.is_empty() {
                        self.name_bindings.remove(&name);
                    }
                }
            }
        }
    }

    fn new_expr(&mut self, kind: ExprKind, span: Span, ty: Option<Type>) -> Expr {
        let id = self.next_expr_id;
        self.next_expr_id += 1;
        if let Some(ty) = ty {
            self.synthetic_expr_types.insert(id, ty);
        }
        Expr { id, kind, span }
    }

    fn expr_type(&self, expr: &Expr) -> Result<Type, String> {
        if let Some(ty) = self.synthetic_expr_types.get(&expr.id) {
            return Ok(ty.clone());
        }
        self.expr_types
            .get(&expr.id)
            .cloned()
            .ok_or_else(|| format!("missing expression type for {:?} (id {})", expr.kind, expr.id))
    }

    fn new_temp_local(&mut self, ty: Type) -> Result<(LocalId, String), String> {
        let name = format!("$t{}", self.next_temp);
        self.next_temp += 1;
        let class = self
            .defs
            .classify(&ty)
            .ok_or_else(|| "unknown type".to_string())?;
        let local_id = self.locals.len();
        self.locals.push(Local {
            name: Some(name.clone()),
            ty,
            class,
        });
        if let Some(ty) = self.locals.get(local_id).map(|local| local.ty.clone()) {
            if self.needs_drop(&ty) {
                self.register_drop_local(local_id);
            }
        }
        Ok((local_id, name))
    }

    fn new_temp_local_no_drop(&mut self, ty: Type) -> Result<(LocalId, String), String> {
        let name = format!("$t{}", self.next_temp);
        self.next_temp += 1;
        let class = self
            .defs
            .classify(&ty)
            .ok_or_else(|| "unknown type".to_string())?;
        let local_id = self.locals.len();
        self.locals.push(Local {
            name: Some(name.clone()),
            ty,
            class,
        });
        Ok((local_id, name))
    }

    fn new_named_local(&mut self, name: String, ty: Type) -> Result<(LocalId, String), String> {
        let class = self
            .defs
            .classify(&ty)
            .ok_or_else(|| "unknown type".to_string())?;
        let local_id = self.locals.len();
        let unique = format!("{}#{}", name, local_id);
        self.locals.push(Local {
            name: Some(unique.clone()),
            ty,
            class,
        });
        if let Some(ty) = self.locals.get(local_id).map(|local| local.ty.clone()) {
            if self.needs_drop(&ty) {
                self.register_drop_local(local_id);
            }
        }
        self.bind_name(name, local_id);
        Ok((local_id, unique))
    }

    fn bind_name(&mut self, name: String, local: LocalId) {
        self.name_bindings
            .entry(name.clone())
            .or_insert_with(Vec::new)
            .push(local);
        if let Some(scope) = self.scope_bindings.last_mut() {
            scope.push(name);
        }
    }

    fn resolve_name(&self, name: &str) -> Option<LocalId> {
        self.name_bindings
            .get(name)
            .and_then(|stack| stack.last().copied())
    }

    fn unit_expr(&mut self, span: Span) -> Expr {
        let block = Block {
            stmts: Vec::new(),
            tail: None,
            span: span.clone(),
        };
        self.new_expr(ExprKind::Block(Box::new(block)), span, Some(Type::Builtin(BuiltinType::Unit)))
    }

    fn register_cleanup_item(&mut self, item: CleanupItem) {
        if let Some(scope_id) = self.current_scope {
            if let Some(scope) = self.scopes.get_mut(scope_id) {
                scope.items.push(item);
            }
        }
    }

    fn register_drop_local(&mut self, local: LocalId) {
        self.register_cleanup_item(CleanupItem::DropLocal { local });
    }

    fn needs_drop(&self, ty: &Type) -> bool {
        match ty {
            Type::Builtin(BuiltinType::Bytes) => true,
            Type::Builtin(_) => false,
            Type::Slice(_) | Type::Map(_, _) | Type::Chan(_) | Type::Shared(_) => true,
            Type::Tuple(items) => items.iter().any(|item| self.needs_drop(item)),
            Type::Result(ok, err) => self.needs_drop(ok) || self.needs_drop(err),
            Type::Named(name) => match self.defs.get(name) {
                Some(crate::sema::types::TypeDefKind::Struct(def)) => def
                    .fields
                    .iter()
                    .any(|(_, field_ty)| self.needs_drop(field_ty)),
                Some(crate::sema::types::TypeDefKind::Enum(def)) => def
                    .variants
                    .iter()
                    .any(|(_, fields)| fields.iter().any(|field_ty| self.needs_drop(field_ty))),
                None => false,
            },
            _ => false,
        }
    }

    fn ident_expr(&mut self, name: String, span: Span, ty: Type) -> Expr {
        self.new_expr(ExprKind::Ident(name), span, Some(ty))
    }

    fn int_expr(&mut self, value: i64, span: Span, ty: Type) -> Expr {
        self.new_expr(ExprKind::Int(value.to_string()), span, Some(ty))
    }

    fn type_to_ast(&self, ty: &Type, span: Span) -> TypeAst {
        let kind = match ty {
            Type::Builtin(builtin) => {
                let name = match builtin {
                    BuiltinType::Bool => "bool",
                    BuiltinType::I32 => "i32",
                    BuiltinType::I64 => "i64",
                    BuiltinType::U32 => "u32",
                    BuiltinType::U64 => "u64",
                    BuiltinType::F32 => "f32",
                    BuiltinType::F64 => "f64",
                    BuiltinType::Char => "char",
                    BuiltinType::Unit => "unit",
                    BuiltinType::String => "string",
                    BuiltinType::Error => "error",
                    BuiltinType::Bytes => "bytes",
                };
                TypeAstKind::Named(name.to_string())
            }
            Type::Named(name) => TypeAstKind::Named(name.clone()),
            Type::Ref(inner) => TypeAstKind::Ref(Box::new(self.type_to_ast(inner, span.clone()))),
            Type::MutRef(inner) => {
                TypeAstKind::MutRef(Box::new(self.type_to_ast(inner, span.clone())))
            }
            Type::Slice(inner) => {
                TypeAstKind::Slice(Box::new(self.type_to_ast(inner, span.clone())))
            }
            Type::Map(key, value) => TypeAstKind::Map(
                Box::new(self.type_to_ast(key, span.clone())),
                Box::new(self.type_to_ast(value, span.clone())),
            ),
            Type::Result(ok, err) => TypeAstKind::Result(
                Box::new(self.type_to_ast(ok, span.clone())),
                Box::new(self.type_to_ast(err, span.clone())),
            ),
            Type::Iter(_) => TypeAstKind::Named("__iter".to_string()),
            Type::Chan(inner) => TypeAstKind::Chan(Box::new(self.type_to_ast(inner, span.clone()))),
            Type::Shared(inner) => {
                TypeAstKind::Shared(Box::new(self.type_to_ast(inner, span.clone())))
            }
            Type::Interface => TypeAstKind::Interface,
            Type::Tuple(items) => TypeAstKind::Tuple(
                items
                    .iter()
                    .map(|item| self.type_to_ast(item, span.clone()))
                    .collect(),
            ),
        };
        TypeAst { kind, span }
    }

    fn for_in_mode(&self, iter_ty: &Type, _span: Span) -> Result<(Type, ForInMode), String> {
        match iter_ty {
            Type::Slice(inner) => {
                let elem = *inner.clone();
                if self.defs.classify(&elem) != Some(TypeClass::Copy) {
                    return Err(
                        "for-in by value requires Copy element; use &xs or &mut xs".to_string(),
                    );
                }
                Ok((elem, ForInMode::Value))
            }
            Type::Builtin(BuiltinType::Bytes) => {
                let elem = Type::Builtin(BuiltinType::U32);
                Ok((elem, ForInMode::Value))
            }
            Type::Ref(inner) => match &**inner {
                Type::Slice(elem) => Ok(((**elem).clone(), ForInMode::Ref)),
                Type::Builtin(BuiltinType::Bytes) => {
                    let elem = Type::Builtin(BuiltinType::U32);
                    Ok((elem, ForInMode::Ref))
                }
                _ => Err("for-in expects a slice".to_string()),
            },
            Type::MutRef(inner) => match &**inner {
                Type::Slice(elem) => Ok(((**elem).clone(), ForInMode::MutRef)),
                Type::Builtin(BuiltinType::Bytes) => {
                    let elem = Type::Builtin(BuiltinType::U32);
                    Ok((elem, ForInMode::MutRef))
                }
                _ => Err("for-in expects a slice".to_string()),
            },
            _ => Err("for-in expects a slice".to_string()),
        }
    }

    fn parse_iter_chain(&self, expr: &Expr) -> Result<Option<IterChain>, String> {
        let ty = self.expr_type(expr)?;
        if !matches!(ty, Type::Iter(_)) {
            return Ok(None);
        }
        let chain = self.parse_iter_chain_inner(expr)?;
        Ok(Some(chain))
    }

    fn parse_iter_chain_inner(&self, expr: &Expr) -> Result<IterChain, String> {
        match &expr.kind {
            ExprKind::Call { callee, args, .. } => {
                let callee_name = match &callee.kind {
                    ExprKind::Ident(name) => name.as_str(),
                    _ => {
                        return Err(
                            "iterator chain expects iter/iter_mut/filter/map calls".to_string(),
                        )
                    }
                };
                match callee_name {
                    "iter" | "iter_mut" => {
                        if args.len() != 1 {
                            return Err("iter expects one argument".to_string());
                        }
                        Ok(IterChain {
                            base_expr: args[0].clone(),
                            base_span: args[0].span.clone(),
                            stages: Vec::new(),
                        })
                    }
                    "filter" => {
                        if args.len() != 2 {
                            return Err("filter expects two arguments".to_string());
                        }
                        let mut chain = self.parse_iter_chain_inner(&args[0])?;
                        let func = match &args[1].kind {
                            ExprKind::Ident(name) => name.clone(),
                            _ => {
                                return Err(
                                    "filter expects a direct function symbol".to_string(),
                                )
                            }
                        };
                        chain.stages.push(IterStage::Filter {
                            func,
                            span: args[1].span.clone(),
                        });
                        Ok(chain)
                    }
                    "map" => {
                        if args.len() != 2 {
                            return Err("map expects two arguments".to_string());
                        }
                        let mut chain = self.parse_iter_chain_inner(&args[0])?;
                        let func = match &args[1].kind {
                            ExprKind::Ident(name) => name.clone(),
                            _ => {
                                return Err(
                                    "map expects a direct function symbol".to_string(),
                                )
                            }
                        };
                        let out_ty = match self.expr_type(expr)? {
                            Type::Iter(inner) => *inner,
                            _ => return Err("map expects iterator".to_string()),
                        };
                        chain.stages.push(IterStage::Map {
                            func,
                            out_ty,
                            span: args[1].span.clone(),
                        });
                        Ok(chain)
                    }
                    _ => Err("iterator chain expects iter/iter_mut/filter/map calls".to_string()),
                }
            }
            _ => Err("iterator chain expects iter/iter_mut/filter/map calls".to_string()),
        }
    }

    fn lower_block(&mut self, block: &Block, is_fn_body: bool) -> Result<(), String> {
        self.ensure_open_block();
        self.push_scope();
        if is_fn_body {
            for local_id in 0..self.param_count {
                if let Some(local) = self.locals.get(local_id) {
                    if self.needs_drop(&local.ty) {
                        self.register_drop_local(local_id);
                    }
                }
            }
        }
        for stmt in &block.stmts {
            self.lower_stmt(stmt)?;
            if self.is_terminated(self.current) {
                self.pop_scope_bindings_only();
                return Ok(());
            }
        }
        if let Some(tail) = &block.tail {
            if is_fn_body && self.ret_ty != Type::Builtin(BuiltinType::Unit) {
                self.ensure_open_block();
                let value = self.lower_expr_value(tail)?;
                let value_ty = self.expr_type(&value)?;
                let (local_id, local_name) = self.new_temp_local_no_drop(value_ty.clone())?;
                self.blocks[self.current].stmts.push(MirStmt::Eval {
                    expr: value,
                    out: vec![local_id],
                });
                let ident = self.ident_expr(local_name, tail.span.clone(), value_ty);
                self.set_terminator(Terminator::Return { value: Some(ident) });
                return Ok(());
            } else {
                let value = self.lower_expr_value(tail)?;
                self.blocks[self.current]
                    .stmts
                    .push(MirStmt::Expr { expr: value });
            }
        }
        if !self.is_terminated(self.current) {
            self.pop_scope();
        }
        Ok(())
    }

    fn lower_block_expr_into(&mut self, block: &Block, out: LocalId) -> Result<(), String> {
        self.ensure_open_block();
        self.push_scope();
        for stmt in &block.stmts {
            self.lower_stmt(stmt)?;
            if self.is_terminated(self.current) {
                self.pop_scope_bindings_only();
                return Ok(());
            }
        }
        let tail_expr = if let Some(tail) = &block.tail {
            self.lower_expr_value(tail)?
        } else {
            self.unit_expr(block.span.clone())
        };
        if !self.is_terminated(self.current) {
            self.blocks[self.current].stmts.push(MirStmt::Eval {
                expr: tail_expr,
                out: vec![out],
            });
            self.pop_scope();
        }
        Ok(())
    }

    fn lower_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        self.ensure_open_block();
        match stmt {
            Stmt::ForIn { name, iter, body, .. } => {
                self.lower_for_in(name, iter, body)
            }
            Stmt::Select { arms, .. } => {
                self.lower_select_stmt(arms)
            }
            Stmt::Go { expr, .. } => {
                let (callee, type_args, args) = match &expr.kind {
                    ExprKind::Call { callee, type_args, args } => (callee, type_args, args),
                    _ => return Err("go expects a call expression".to_string()),
                };
                if !type_args.is_empty() {
                    return Err("go does not accept type arguments".to_string());
                }
                let callee_name = match &callee.kind {
                    ExprKind::Ident(name) => name.clone(),
                    _ => return Err("go expects a direct call".to_string()),
                };
                let mut lowered_args = Vec::new();
                for arg in args {
                    let lowered = self.lower_expr_value(arg)?;
                    if self.is_terminated(self.current) {
                        return Ok(());
                    }
                    let arg_ty = self.expr_type(&lowered)?;
                    let (local_id, local_name) = self.new_temp_local(arg_ty.clone())?;
                    self.blocks[self.current].stmts.push(MirStmt::Eval {
                        expr: lowered,
                        out: vec![local_id],
                    });
                    let ident = self.ident_expr(local_name, arg.span.clone(), arg_ty);
                    lowered_args.push(ident);
                }
                let callee_ident =
                    self.ident_expr(callee_name, expr.span.clone(), Type::Builtin(BuiltinType::Unit));
                let go_call = self.new_expr(
                    ExprKind::Call {
                        callee: Box::new(callee_ident),
                        type_args: Vec::new(),
                        args: lowered_args,
                    },
                    expr.span.clone(),
                    Some(Type::Builtin(BuiltinType::Unit)),
                );
                self.blocks[self.current]
                    .stmts
                    .push(MirStmt::Go { expr: go_call });
                Ok(())
            }
            Stmt::Defer { expr, span } => {
                let (callee, type_args, args) = match &expr.kind {
                    ExprKind::Call { callee, type_args, args } => {
                        (callee, type_args, args)
                    }
                    _ => return Err("defer expects a call expression".to_string()),
                };
                let callee_name = match &callee.kind {
                    ExprKind::Ident(name) => name.clone(),
                    _ => return Err("defer expects a direct call".to_string()),
                };
                let mut lowered_args = Vec::new();
                for arg in args {
                    let lowered = self.lower_expr_value(arg)?;
                    if self.is_terminated(self.current) {
                        return Ok(());
                    }
                    let arg_ty = self.expr_type(&lowered)?;
                    let (local_id, local_name) = self.new_temp_local(arg_ty.clone())?;
                    self.blocks[self.current].stmts.push(MirStmt::Eval {
                        expr: lowered,
                        out: vec![local_id],
                    });
                    let ident = self.ident_expr(local_name, arg.span.clone(), arg_ty);
                    lowered_args.push(ident);
                }
                let callee_ident =
                    self.ident_expr(callee_name, expr.span.clone(), Type::Builtin(BuiltinType::Unit));
                let call_expr = self.new_expr(
                    ExprKind::Call {
                        callee: Box::new(callee_ident),
                        type_args: type_args.clone(),
                        args: lowered_args,
                    },
                    span.clone(),
                    None,
                );
                self.register_cleanup_item(CleanupItem::DeferCall { call: call_expr });
                Ok(())
            }
            Stmt::Let { name, ty: _, init, .. } => {
                let value = self.lower_expr_value(init)?;
                let init_ty = self.expr_type(&value)?;
                let (local_id, _local_name) = self.new_named_local(name.clone(), init_ty)?;
                self.blocks[self.current]
                    .stmts
                    .push(MirStmt::Eval { expr: value, out: vec![local_id] });
                Ok(())
            }
            Stmt::Assign { target, value, .. } => {
                let target_expr = self.lower_expr_value(target)?;
                let value_expr = self.lower_expr_value(value)?;
                self.blocks[self.current].stmts.push(MirStmt::Assign {
                    target: target_expr,
                    value: value_expr,
                });
                Ok(())
            }
            Stmt::Expr { expr, .. } => self.lower_expr_stmt(expr),
            Stmt::Return { expr, .. } => {
                if let Some(expr) = expr {
                    let value = self.lower_expr_value(expr)?;
                    let value_ty = self.expr_type(&value)?;
                    let (local_id, local_name) = self.new_temp_local_no_drop(value_ty.clone())?;
                    self.blocks[self.current].stmts.push(MirStmt::Eval {
                        expr: value,
                        out: vec![local_id],
                    });
                    let ident = self.ident_expr(local_name, expr.span.clone(), value_ty);
                    self.set_terminator(Terminator::Return { value: Some(ident) });
                } else {
                    self.set_terminator(Terminator::Return { value: None });
                }
                Ok(())
            }
            Stmt::Break { .. } => {
                let (break_bb, _) = self
                    .loop_stack
                    .last()
                    .ok_or_else(|| "break outside loop".to_string())?
                    .clone();
                self.set_terminator(Terminator::Goto(break_bb));
                Ok(())
            }
            Stmt::Continue { .. } => {
                let (_, continue_bb) = self
                    .loop_stack
                    .last()
                    .ok_or_else(|| "continue outside loop".to_string())?
                    .clone();
                self.set_terminator(Terminator::Goto(continue_bb));
                Ok(())
            }
        }
    }

    fn lower_for_in(&mut self, name: &str, iter: &Expr, body: &Block) -> Result<(), String> {
        self.ensure_open_block();
        if let Some(chain) = self.parse_iter_chain(iter)? {
            return self.lower_for_iter_chain(name, chain, body);
        }
        let iter_expr = self.lower_expr_value(iter)?;
        let iter_ty = self.expr_type(&iter_expr)?;
        let (elem_ty, mode) = self.for_in_mode(&iter_ty, iter.span.clone())?;

        let (iter_local, iter_name) = self.new_temp_local(iter_ty.clone())?;
        self.blocks[self.current].stmts.push(MirStmt::Eval {
            expr: iter_expr,
            out: vec![iter_local],
        });

        let idx_ty = Type::Builtin(BuiltinType::I64);
        let (idx_local, idx_name) = self.new_temp_local(idx_ty.clone())?;
        let zero_expr = self.int_expr(0, iter.span.clone(), idx_ty.clone());
        self.blocks[self.current].stmts.push(MirStmt::Eval {
            expr: zero_expr,
            out: vec![idx_local],
        });

        let (len_local, len_name) = self.new_temp_local(idx_ty.clone())?;
        let len_expr = {
            let callee = self.new_expr(
                ExprKind::Ident("slice_len".to_string()),
                iter.span.clone(),
                None,
            );
            let iter_ident = self.ident_expr(iter_name.clone(), iter.span.clone(), iter_ty.clone());
            let arg = match &iter_ty {
                Type::Slice(_) | Type::Builtin(BuiltinType::Bytes) => self.new_expr(
                    ExprKind::Borrow {
                        is_mut: false,
                        expr: Box::new(iter_ident),
                    },
                    iter.span.clone(),
                    None,
                ),
                _ => iter_ident,
            };
            let type_arg = self.type_to_ast(&elem_ty, iter.span.clone());
            self.new_expr(
                ExprKind::Call {
                    callee: Box::new(callee),
                    type_args: vec![type_arg],
                    args: vec![arg],
                },
                iter.span.clone(),
                Some(Type::Builtin(BuiltinType::I64)),
            )
        };
        self.blocks[self.current].stmts.push(MirStmt::Eval {
            expr: len_expr,
            out: vec![len_local],
        });

        let head_bb = self.new_block();
        let body_bb = self.new_block();
        let step_bb = self.new_block();
        let exit_bb = self.new_block();

        self.set_terminator(Terminator::Goto(head_bb));

        self.set_current(head_bb);
        let idx_ident = self.ident_expr(idx_name.clone(), iter.span.clone(), idx_ty.clone());
        let len_ident = self.ident_expr(len_name.clone(), iter.span.clone(), idx_ty.clone());
        let cond_expr = self.new_expr(
            ExprKind::Binary {
                op: BinaryOp::Lt,
                left: Box::new(idx_ident),
                right: Box::new(len_ident),
            },
            iter.span.clone(),
            Some(Type::Builtin(BuiltinType::Bool)),
        );
        self.set_terminator(Terminator::If {
            cond: cond_expr,
            then_bb: body_bb,
            else_bb: exit_bb,
        });

        self.set_current(body_bb);
        let loop_var_ty = match mode {
            ForInMode::Value => elem_ty.clone(),
            ForInMode::Ref => Type::Ref(Box::new(elem_ty.clone())),
            ForInMode::MutRef => Type::MutRef(Box::new(elem_ty.clone())),
        };
        let elem_expr = {
            let idx_expr = self.ident_expr(idx_name.clone(), iter.span.clone(), idx_ty.clone());
            let iter_ident = self.ident_expr(iter_name.clone(), iter.span.clone(), iter_ty.clone());
            let (callee_name, arg_expr) = match mode {
                ForInMode::Value => {
                    let borrowed = self.new_expr(
                        ExprKind::Borrow {
                            is_mut: false,
                            expr: Box::new(iter_ident),
                        },
                        iter.span.clone(),
                        None,
                    );
                    ("slice_get_copy", borrowed)
                }
                ForInMode::Ref => ("slice_ref", iter_ident),
                ForInMode::MutRef => ("slice_mutref", iter_ident),
            };
            let callee = self.new_expr(
                ExprKind::Ident(callee_name.to_string()),
                iter.span.clone(),
                None,
            );
            let type_arg = self.type_to_ast(&elem_ty, iter.span.clone());
            self.new_expr(
                ExprKind::Call {
                    callee: Box::new(callee),
                    type_args: vec![type_arg],
                    args: vec![arg_expr, idx_expr],
                },
                iter.span.clone(),
                Some(loop_var_ty.clone()),
            )
        };

        let mut lowered_body = body.clone();
        lowered_body.stmts.insert(
            0,
            Stmt::Let {
                name: name.to_string(),
                ty: None,
                init: elem_expr,
                span: body.span.clone(),
            },
        );
        self.loop_stack.push((exit_bb, step_bb));
        self.lower_block(&lowered_body, false)?;
        self.loop_stack.pop();
        if !self.is_terminated(self.current) {
            self.set_terminator(Terminator::Goto(step_bb));
        }

        self.set_current(step_bb);
        let idx_expr = self.ident_expr(idx_name, iter.span.clone(), idx_ty.clone());
        let one_expr = self.int_expr(1, iter.span.clone(), idx_ty.clone());
        let add_expr = self.new_expr(
            ExprKind::Binary {
                op: BinaryOp::Add,
                left: Box::new(idx_expr),
                right: Box::new(one_expr),
            },
            iter.span.clone(),
            Some(idx_ty.clone()),
        );
        self.blocks[self.current].stmts.push(MirStmt::Eval {
            expr: add_expr,
            out: vec![idx_local],
        });
        self.set_terminator(Terminator::Goto(head_bb));

        self.set_current(exit_bb);
        Ok(())
    }

    fn lower_for_iter_chain(
        &mut self,
        name: &str,
        chain: IterChain,
        body: &Block,
    ) -> Result<(), String> {
        self.ensure_open_block();

        let base_expr = self.lower_expr_value(&chain.base_expr)?;
        let base_ty = self.expr_type(&base_expr)?;
        let (elem_ty, mode) = self.for_in_mode(&base_ty, chain.base_span.clone())?;
        if matches!(mode, ForInMode::Value) {
            return Err("iter expects &slice or &mut slice".to_string());
        }

        let (iter_local, iter_name) = self.new_temp_local(base_ty.clone())?;
        self.blocks[self.current].stmts.push(MirStmt::Eval {
            expr: base_expr,
            out: vec![iter_local],
        });

        let idx_ty = Type::Builtin(BuiltinType::I64);
        let (idx_local, idx_name) = self.new_temp_local(idx_ty.clone())?;
        let zero_expr = self.int_expr(0, chain.base_span.clone(), idx_ty.clone());
        self.blocks[self.current].stmts.push(MirStmt::Eval {
            expr: zero_expr,
            out: vec![idx_local],
        });

        let (len_local, len_name) = self.new_temp_local(idx_ty.clone())?;
        let len_expr = {
            let callee = self.new_expr(
                ExprKind::Ident("slice_len".to_string()),
                chain.base_span.clone(),
                None,
            );
            let iter_ident =
                self.ident_expr(iter_name.clone(), chain.base_span.clone(), base_ty.clone());
            let type_arg = self.type_to_ast(&elem_ty, chain.base_span.clone());
            self.new_expr(
                ExprKind::Call {
                    callee: Box::new(callee),
                    type_args: vec![type_arg],
                    args: vec![iter_ident],
                },
                chain.base_span.clone(),
                Some(Type::Builtin(BuiltinType::I64)),
            )
        };
        self.blocks[self.current].stmts.push(MirStmt::Eval {
            expr: len_expr,
            out: vec![len_local],
        });

        let head_bb = self.new_block();
        let base_bb = self.new_block();
        let step_bb = self.new_block();
        let exit_bb = self.new_block();

        self.set_terminator(Terminator::Goto(head_bb));

        self.set_current(head_bb);
        let idx_ident = self.ident_expr(idx_name.clone(), chain.base_span.clone(), idx_ty.clone());
        let len_ident = self.ident_expr(len_name.clone(), chain.base_span.clone(), idx_ty.clone());
        let cond_expr = self.new_expr(
            ExprKind::Binary {
                op: BinaryOp::Lt,
                left: Box::new(idx_ident),
                right: Box::new(len_ident),
            },
            chain.base_span.clone(),
            Some(Type::Builtin(BuiltinType::Bool)),
        );
        self.set_terminator(Terminator::If {
            cond: cond_expr,
            then_bb: base_bb,
            else_bb: exit_bb,
        });

        self.set_current(base_bb);
        self.push_scope();

        let loop_var_ty = match mode {
            ForInMode::Ref => Type::Ref(Box::new(elem_ty.clone())),
            ForInMode::MutRef => Type::MutRef(Box::new(elem_ty.clone())),
            ForInMode::Value => elem_ty.clone(),
        };
        let elem_expr = {
            let idx_expr = self.ident_expr(idx_name.clone(), chain.base_span.clone(), idx_ty.clone());
            let iter_ident =
                self.ident_expr(iter_name.clone(), chain.base_span.clone(), base_ty.clone());
            let (callee_name, arg_expr) = match mode {
                ForInMode::Ref => ("slice_ref", iter_ident),
                ForInMode::MutRef => ("slice_mutref", iter_ident),
                ForInMode::Value => {
                    let borrowed = self.new_expr(
                        ExprKind::Borrow {
                            is_mut: false,
                            expr: Box::new(iter_ident),
                        },
                        chain.base_span.clone(),
                        None,
                    );
                    ("slice_get_copy", borrowed)
                }
            };
            let callee = self.new_expr(
                ExprKind::Ident(callee_name.to_string()),
                chain.base_span.clone(),
                None,
            );
            let type_arg = self.type_to_ast(&elem_ty, chain.base_span.clone());
            self.new_expr(
                ExprKind::Call {
                    callee: Box::new(callee),
                    type_args: vec![type_arg],
                    args: vec![arg_expr, idx_expr],
                },
                chain.base_span.clone(),
                Some(loop_var_ty.clone()),
            )
        };

        let (_cur_local, mut cur_name) = self.new_temp_local(loop_var_ty.clone())?;
        self.blocks[self.current].stmts.push(MirStmt::Eval {
            expr: elem_expr,
            out: vec![_cur_local],
        });
        let mut cur_ty = loop_var_ty.clone();

        let mut stage_blocks = Vec::new();
        for _ in &chain.stages {
            stage_blocks.push(self.new_block());
        }
        let body_bb = self.new_block();

        let first_bb = stage_blocks.first().copied().unwrap_or(body_bb);
        self.set_terminator(Terminator::Goto(first_bb));

        for (idx, stage) in chain.stages.iter().enumerate() {
            let stage_bb = stage_blocks[idx];
            let next_bb = if idx + 1 < stage_blocks.len() {
                stage_blocks[idx + 1]
            } else {
                body_bb
            };
            self.set_current(stage_bb);
            match stage {
                IterStage::Filter { func, span } => {
                    let callee = self.new_expr(
                        ExprKind::Ident(func.clone()),
                        span.clone(),
                        None,
                    );
                    let arg = self.ident_expr(cur_name.clone(), span.clone(), cur_ty.clone());
                    let cond_expr = self.new_expr(
                        ExprKind::Call {
                            callee: Box::new(callee),
                            type_args: Vec::new(),
                            args: vec![arg],
                        },
                        span.clone(),
                        Some(Type::Builtin(BuiltinType::Bool)),
                    );
                    self.set_terminator(Terminator::If {
                        cond: cond_expr,
                        then_bb: next_bb,
                        else_bb: step_bb,
                    });
                }
                IterStage::Map { func, out_ty, span } => {
                    let callee = self.new_expr(
                        ExprKind::Ident(func.clone()),
                        span.clone(),
                        None,
                    );
                    let arg = self.ident_expr(cur_name.clone(), span.clone(), cur_ty.clone());
                    let call_expr = self.new_expr(
                        ExprKind::Call {
                            callee: Box::new(callee),
                            type_args: Vec::new(),
                            args: vec![arg],
                        },
                        span.clone(),
                        Some(out_ty.clone()),
                    );
                    let (_next_local, next_name) = self.new_temp_local(out_ty.clone())?;
                    self.blocks[self.current].stmts.push(MirStmt::Eval {
                        expr: call_expr,
                        out: vec![_next_local],
                    });
                    cur_name = next_name;
                    cur_ty = out_ty.clone();
                    self.set_terminator(Terminator::Goto(next_bb));
                }
            }
        }

        self.set_current(body_bb);
        let mut lowered_body = body.clone();
        let loop_value = self.ident_expr(cur_name.clone(), body.span.clone(), cur_ty.clone());
        lowered_body.stmts.insert(
            0,
            Stmt::Let {
                name: name.to_string(),
                ty: None,
                init: loop_value,
                span: body.span.clone(),
            },
        );
        self.loop_stack.push((exit_bb, step_bb));
        self.lower_block(&lowered_body, false)?;
        self.loop_stack.pop();
        if !self.is_terminated(self.current) {
            self.pop_scope();
            self.set_terminator(Terminator::Goto(step_bb));
        } else {
            self.pop_scope_bindings_only();
        }

        self.set_current(step_bb);
        let idx_expr = self.ident_expr(idx_name, chain.base_span.clone(), idx_ty.clone());
        let one_expr = self.int_expr(1, chain.base_span.clone(), idx_ty.clone());
        let add_expr = self.new_expr(
            ExprKind::Binary {
                op: BinaryOp::Add,
                left: Box::new(idx_expr),
                right: Box::new(one_expr),
            },
            chain.base_span.clone(),
            Some(idx_ty.clone()),
        );
        self.blocks[self.current].stmts.push(MirStmt::Eval {
            expr: add_expr,
            out: vec![idx_local],
        });
        self.set_terminator(Terminator::Goto(head_bb));

        self.set_current(exit_bb);
        Ok(())
    }

    fn lower_expr_stmt(&mut self, expr: &Expr) -> Result<(), String> {
        let expr = self.lower_expr_value(expr)?;
        self.blocks[self.current].stmts.push(MirStmt::Expr { expr });
        Ok(())
    }

    fn lower_expr_value(&mut self, expr: &Expr) -> Result<Expr, String> {
        match &expr.kind {
            ExprKind::Try { expr: inner } => {
                self.ensure_open_block();
                let inner_ty = self.expr_type(inner)?;
                let lowered_inner = self.lower_expr_value(inner)?;
                let (ok_ty, err_ty) = match &inner_ty {
                    Type::Builtin(BuiltinType::Error) => (None, Type::Builtin(BuiltinType::Error)),
                    Type::Result(ok_ty, err_ty) => {
                        if **err_ty != Type::Builtin(BuiltinType::Error) {
                            return Err("`?` expects error or (T, error)".to_string());
                        }
                        (Some(*ok_ty.clone()), *err_ty.clone())
                    }
                    Type::Tuple(items) if items.len() == 2 => {
                        let ok_ty = items[0].clone();
                        let err_ty = items[1].clone();
                        if err_ty != Type::Builtin(BuiltinType::Error) {
                            return Err("`?` expects error or (T, error)".to_string());
                        }
                        (Some(ok_ty), err_ty)
                    }
                    _ => return Err("`?` expects error or (T, error)".to_string()),
                };
                let mut out = Vec::new();
                let ok_local = if let Some(ok_ty) = &ok_ty {
                    let (local_id, _) = self.new_temp_local(ok_ty.clone())?;
                    out.push(local_id);
                    Some(local_id)
                } else {
                    None
                };
                let (err_local, err_name) = self.new_temp_local(err_ty.clone())?;
                out.push(err_local);
                self.blocks[self.current]
                    .stmts
                    .push(MirStmt::Eval { expr: lowered_inner, out });
                let err_expr = self.ident_expr(
                    err_name.clone(),
                    expr.span.clone(),
                    Type::Builtin(BuiltinType::Error),
                );
                let nil_expr = self.new_expr(
                    ExprKind::Nil,
                    expr.span.clone(),
                    Some(Type::Builtin(BuiltinType::Error)),
                );
                let cond_expr = self.new_expr(
                    ExprKind::Binary {
                        op: BinaryOp::Eq,
                        left: Box::new(err_expr.clone()),
                        right: Box::new(nil_expr),
                    },
                    expr.span.clone(),
                    Some(Type::Builtin(BuiltinType::Bool)),
                );
                let ok_bb = self.new_block();
                let err_bb = self.new_block();
                self.set_terminator(Terminator::If {
                    cond: cond_expr,
                    then_bb: ok_bb,
                    else_bb: err_bb,
                });
                self.set_current(err_bb);
                self.set_terminator(Terminator::ReturnError { err: err_expr });
                self.set_current(ok_bb);
                if let Some(ok_local) = ok_local {
                    let ok_name = self
                        .locals
                        .get(ok_local)
                        .and_then(|local| local.name.clone())
                        .ok_or_else(|| "missing temp local".to_string())?;
                    Ok(self.ident_expr(ok_name, expr.span.clone(), ok_ty.unwrap()))
                } else {
                    Ok(self.unit_expr(expr.span.clone()))
                }
            }
            ExprKind::Block(block) => {
                if !block_contains_try(block) {
                    return Ok(self.rewrite_expr_names(expr));
                }
                let block_ty = self.expr_type(expr)?;
                let (local_id, local_name) = self.new_temp_local(block_ty.clone())?;
                self.lower_block_expr_into(block, local_id)?;
                if self.is_terminated(self.current) {
                    return Ok(self.unit_expr(expr.span.clone()));
                }
                Ok(self.ident_expr(local_name, expr.span.clone(), block_ty))
            }
            ExprKind::If { cond, then_block, else_block } => {
                if !expr_contains_try(expr) {
                    return Ok(self.rewrite_expr_names(expr));
                }
                let result_ty = self.expr_type(expr)?;
                let (result_local, result_name) = self.new_temp_local(result_ty.clone())?;
                let cond_expr = self.lower_expr_value(cond)?;
                let then_bb = self.new_block();
                let else_bb = self.new_block();
                let join_bb = self.new_block();
                self.set_terminator(Terminator::If {
                    cond: cond_expr,
                    then_bb,
                    else_bb,
                });
                self.set_current(then_bb);
                self.lower_block_expr_into(then_block, result_local)?;
                if !self.is_terminated(self.current) {
                    self.set_terminator(Terminator::Goto(join_bb));
                }
                self.set_current(else_bb);
                if let Some(block) = else_block {
                    self.lower_block_expr_into(block, result_local)?;
                } else {
                    let empty_block = Block {
                        stmts: Vec::new(),
                        tail: None,
                        span: expr.span.clone(),
                    };
                    self.lower_block_expr_into(&empty_block, result_local)?;
                }
                if !self.is_terminated(self.current) {
                    self.set_terminator(Terminator::Goto(join_bb));
                }
                self.set_current(join_bb);
                Ok(self.ident_expr(result_name, expr.span.clone(), result_ty))
            }
            ExprKind::Match { scrutinee, arms } => {
                if !expr_contains_try(expr) {
                    return Ok(self.rewrite_expr_names(expr));
                }
                if !self.can_lower_match_stmt(arms) {
                    return Err("`?` inside match requires literal patterns in MIR lowering".to_string());
                }
                if !arms.iter().any(|arm| matches!(arm.pattern, Pattern::Wildcard)) {
                    return Err("`?` inside match requires a wildcard arm in MIR lowering".to_string());
                }
                let result_ty = self.expr_type(expr)?;
                let (result_local, result_name) = self.new_temp_local(result_ty.clone())?;
                let scrutinee_expr = self.lower_expr_value(scrutinee)?;
                let mut arm_blocks = Vec::new();
                let mut arm_infos = Vec::new();
                let mut default_bb = None;
                for arm in arms {
                    let arm_bb = self.new_block();
                    if matches!(arm.pattern, Pattern::Wildcard) && default_bb.is_none() {
                        default_bb = Some(arm_bb);
                    } else {
                        arm_blocks.push((arm.pattern.clone(), arm_bb));
                    }
                    arm_infos.push((arm.pattern.clone(), arm.body.clone(), arm_bb));
                }
                let default_bb = default_bb.unwrap_or_else(|| self.new_block());
                self.set_terminator(Terminator::Match {
                    scrutinee: scrutinee_expr,
                    arms: arm_blocks.clone(),
                    default: Some(default_bb),
                });
                let join_bb = self.new_block();
                for (pattern, body, arm_bb) in arm_infos {
                    let target_bb = if matches!(pattern, Pattern::Wildcard) {
                        default_bb
                    } else {
                        arm_bb
                    };
                    self.set_current(target_bb);
                    match &body {
                        crate::frontend::ast::BlockOrExpr::Block(block) => {
                            self.lower_block_expr_into(block, result_local)?;
                        }
                        crate::frontend::ast::BlockOrExpr::Expr(expr) => {
                            let expr_block = Block {
                                stmts: Vec::new(),
                                tail: Some(expr.clone()),
                                span: expr.span.clone(),
                            };
                            self.lower_block_expr_into(&expr_block, result_local)?;
                        }
                    }
                    if !self.is_terminated(self.current) {
                        self.set_terminator(Terminator::Goto(join_bb));
                    }
                }
                self.set_current(join_bb);
                Ok(self.ident_expr(result_name, expr.span.clone(), result_ty))
            }
            ExprKind::Unary { op, expr: inner } => {
                let lowered = self.lower_expr_value(inner)?;
                Ok(Expr {
                    id: expr.id,
                    kind: ExprKind::Unary {
                        op: op.clone(),
                        expr: Box::new(lowered),
                    },
                    span: expr.span.clone(),
                })
            }
            ExprKind::Binary { op, left, right } => {
                let left_expr = self.lower_expr_value(left)?;
                let right_expr = self.lower_expr_value(right)?;
                Ok(Expr {
                    id: expr.id,
                    kind: ExprKind::Binary {
                        op: op.clone(),
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    span: expr.span.clone(),
                })
            }
            ExprKind::Tuple(items) => {
                let mut lowered = Vec::new();
                for item in items {
                    lowered.push(self.lower_expr_value(item)?);
                }
                Ok(Expr {
                    id: expr.id,
                    kind: ExprKind::Tuple(lowered),
                    span: expr.span.clone(),
                })
            }
            ExprKind::Call { callee, type_args, args } => {
                let callee_expr = self.lower_expr_value(callee)?;
                let mut lowered_args = Vec::new();
                for arg in args {
                    lowered_args.push(self.lower_expr_value(arg)?);
                }
                Ok(Expr {
                    id: expr.id,
                    kind: ExprKind::Call {
                        callee: Box::new(callee_expr),
                        type_args: type_args.clone(),
                        args: lowered_args,
                    },
                    span: expr.span.clone(),
                })
            }
            ExprKind::Field { base, name } => {
                let base_expr = self.lower_expr_value(base)?;
                Ok(Expr {
                    id: expr.id,
                    kind: ExprKind::Field {
                        base: Box::new(base_expr),
                        name: name.clone(),
                    },
                    span: expr.span.clone(),
                })
            }
            ExprKind::Index { base, index } => {
                let base_expr = self.lower_expr_value(base)?;
                let index_expr = self.lower_expr_value(index)?;
                Ok(Expr {
                    id: expr.id,
                    kind: ExprKind::Index {
                        base: Box::new(base_expr),
                        index: Box::new(index_expr),
                    },
                    span: expr.span.clone(),
                })
            }
            ExprKind::Borrow { is_mut, expr: inner } => {
                let inner_expr = self.lower_expr_value(inner)?;
                Ok(Expr {
                    id: expr.id,
                    kind: ExprKind::Borrow {
                        is_mut: *is_mut,
                        expr: Box::new(inner_expr),
                    },
                    span: expr.span.clone(),
                })
            }
            ExprKind::Deref { expr: inner } => {
                let inner_expr = self.lower_expr_value(inner)?;
                Ok(Expr {
                    id: expr.id,
                    kind: ExprKind::Deref {
                        expr: Box::new(inner_expr),
                    },
                    span: expr.span.clone(),
                })
            }
            ExprKind::Send { chan, value } => {
                let chan_expr = self.lower_expr_value(chan)?;
                let value_expr = self.lower_expr_value(value)?;
                Ok(Expr {
                    id: expr.id,
                    kind: ExprKind::Send {
                        chan: Box::new(chan_expr),
                        value: Box::new(value_expr),
                    },
                    span: expr.span.clone(),
                })
            }
            ExprKind::Recv { chan } => {
                let chan_expr = self.lower_expr_value(chan)?;
                Ok(Expr {
                    id: expr.id,
                    kind: ExprKind::Recv {
                        chan: Box::new(chan_expr),
                    },
                    span: expr.span.clone(),
                })
            }
            ExprKind::Close { chan } => {
                let chan_expr = self.lower_expr_value(chan)?;
                Ok(Expr {
                    id: expr.id,
                    kind: ExprKind::Close {
                        chan: Box::new(chan_expr),
                    },
                    span: expr.span.clone(),
                })
            }
            ExprKind::After { ms } => {
                let ms_expr = self.lower_expr_value(ms)?;
                Ok(Expr {
                    id: expr.id,
                    kind: ExprKind::After {
                        ms: Box::new(ms_expr),
                    },
                    span: expr.span.clone(),
                })
            }
            ExprKind::Ident(name) => {
                if let Some(local_id) = self.resolve_name(name) {
                    if let Some(local) = self.locals.get(local_id) {
                        if let Some(local_name) = &local.name {
                            return Ok(self.ident_expr(
                                local_name.clone(),
                                expr.span.clone(),
                                local.ty.clone(),
                            ));
                        }
                    }
                }
                Ok(expr.clone())
            }
            _ => Ok(expr.clone()),
        }
    }

    fn rewrite_expr_names(&self, expr: &Expr) -> Expr {
        let mut env = HashMap::new();
        for (name, stack) in &self.name_bindings {
            if let Some(local_id) = stack.last().copied() {
                env.insert(name.clone(), Some(local_id));
            }
        }
        self.rewrite_expr_with_env(expr, &mut env)
    }

    fn rewrite_expr_with_env(
        &self,
        expr: &Expr,
        env: &mut HashMap<String, Option<LocalId>>,
    ) -> Expr {
        match &expr.kind {
            ExprKind::Ident(name) => {
                if let Some(Some(local_id)) = env.get(name) {
                    if let Some(local) = self.locals.get(*local_id) {
                        if let Some(local_name) = &local.name {
                            return Expr {
                                id: expr.id,
                                kind: ExprKind::Ident(local_name.clone()),
                                span: expr.span.clone(),
                            };
                        }
                    }
                }
                expr.clone()
            }
            ExprKind::Block(block) => {
                let mut block_env = env.clone();
                let new_block = self.rewrite_block_with_env(block, &mut block_env);
                Expr {
                    id: expr.id,
                    kind: ExprKind::Block(Box::new(new_block)),
                    span: expr.span.clone(),
                }
            }
            ExprKind::If { cond, then_block, else_block } => {
                let cond_expr = self.rewrite_expr_with_env(cond, env);
                let mut then_env = env.clone();
                let then_block = self.rewrite_block_with_env(then_block, &mut then_env);
                let else_block = else_block.as_ref().map(|block| {
                    let mut else_env = env.clone();
                    Box::new(self.rewrite_block_with_env(block, &mut else_env))
                });
                Expr {
                    id: expr.id,
                    kind: ExprKind::If {
                        cond: Box::new(cond_expr),
                        then_block: Box::new(then_block),
                        else_block,
                    },
                    span: expr.span.clone(),
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                let scrutinee_expr = self.rewrite_expr_with_env(scrutinee, env);
                let mut new_arms = Vec::with_capacity(arms.len());
                for arm in arms {
                    let mut arm_env = env.clone();
                    self.shadow_pattern_bindings(&arm.pattern, &mut arm_env);
                    let new_body = self.rewrite_block_or_expr_with_env(&arm.body, &mut arm_env);
                    new_arms.push(crate::frontend::ast::MatchArm {
                        pattern: arm.pattern.clone(),
                        body: new_body,
                        span: arm.span.clone(),
                    });
                }
                Expr {
                    id: expr.id,
                    kind: ExprKind::Match {
                        scrutinee: Box::new(scrutinee_expr),
                        arms: new_arms,
                    },
                    span: expr.span.clone(),
                }
            }
            ExprKind::Unary { op, expr: inner } => Expr {
                id: expr.id,
                kind: ExprKind::Unary {
                    op: op.clone(),
                    expr: Box::new(self.rewrite_expr_with_env(inner, env)),
                },
                span: expr.span.clone(),
            },
            ExprKind::Binary { op, left, right } => Expr {
                id: expr.id,
                kind: ExprKind::Binary {
                    op: op.clone(),
                    left: Box::new(self.rewrite_expr_with_env(left, env)),
                    right: Box::new(self.rewrite_expr_with_env(right, env)),
                },
                span: expr.span.clone(),
            },
            ExprKind::Tuple(items) => {
                let mut new_items = Vec::with_capacity(items.len());
                for item in items {
                    new_items.push(self.rewrite_expr_with_env(item, env));
                }
                Expr {
                    id: expr.id,
                    kind: ExprKind::Tuple(new_items),
                    span: expr.span.clone(),
                }
            }
            ExprKind::Call { callee, type_args, args } => {
                let callee_expr = self.rewrite_expr_with_env(callee, env);
                let mut new_args = Vec::with_capacity(args.len());
                for arg in args {
                    new_args.push(self.rewrite_expr_with_env(arg, env));
                }
                Expr {
                    id: expr.id,
                    kind: ExprKind::Call {
                        callee: Box::new(callee_expr),
                        type_args: type_args.clone(),
                        args: new_args,
                    },
                    span: expr.span.clone(),
                }
            }
            ExprKind::Field { base, name } => Expr {
                id: expr.id,
                kind: ExprKind::Field {
                    base: Box::new(self.rewrite_expr_with_env(base, env)),
                    name: name.clone(),
                },
                span: expr.span.clone(),
            },
            ExprKind::Index { base, index } => Expr {
                id: expr.id,
                kind: ExprKind::Index {
                    base: Box::new(self.rewrite_expr_with_env(base, env)),
                    index: Box::new(self.rewrite_expr_with_env(index, env)),
                },
                span: expr.span.clone(),
            },
            ExprKind::Borrow { is_mut, expr: inner } => Expr {
                id: expr.id,
                kind: ExprKind::Borrow {
                    is_mut: *is_mut,
                    expr: Box::new(self.rewrite_expr_with_env(inner, env)),
                },
                span: expr.span.clone(),
            },
            ExprKind::Deref { expr: inner } => Expr {
                id: expr.id,
                kind: ExprKind::Deref {
                    expr: Box::new(self.rewrite_expr_with_env(inner, env)),
                },
                span: expr.span.clone(),
            },
            ExprKind::Try { expr: inner } => Expr {
                id: expr.id,
                kind: ExprKind::Try {
                    expr: Box::new(self.rewrite_expr_with_env(inner, env)),
                },
                span: expr.span.clone(),
            },
            ExprKind::Send { chan, value } => Expr {
                id: expr.id,
                kind: ExprKind::Send {
                    chan: Box::new(self.rewrite_expr_with_env(chan, env)),
                    value: Box::new(self.rewrite_expr_with_env(value, env)),
                },
                span: expr.span.clone(),
            },
            ExprKind::Recv { chan } => Expr {
                id: expr.id,
                kind: ExprKind::Recv {
                    chan: Box::new(self.rewrite_expr_with_env(chan, env)),
                },
                span: expr.span.clone(),
            },
            ExprKind::Close { chan } => Expr {
                id: expr.id,
                kind: ExprKind::Close {
                    chan: Box::new(self.rewrite_expr_with_env(chan, env)),
                },
                span: expr.span.clone(),
            },
            ExprKind::After { ms } => Expr {
                id: expr.id,
                kind: ExprKind::After {
                    ms: Box::new(self.rewrite_expr_with_env(ms, env)),
                },
                span: expr.span.clone(),
            },
            _ => expr.clone(),
        }
    }

    fn rewrite_block_with_env(
        &self,
        block: &Block,
        env: &mut HashMap<String, Option<LocalId>>,
    ) -> Block {
        let mut stmts = Vec::with_capacity(block.stmts.len());
        for stmt in &block.stmts {
            stmts.push(self.rewrite_stmt_with_env(stmt, env));
        }
        let tail = block
            .tail
            .as_ref()
            .map(|expr| self.rewrite_expr_with_env(expr, env));
        Block {
            stmts,
            tail,
            span: block.span.clone(),
        }
    }

    fn rewrite_block_or_expr_with_env(
        &self,
        body: &BlockOrExpr,
        env: &mut HashMap<String, Option<LocalId>>,
    ) -> BlockOrExpr {
        match body {
            BlockOrExpr::Block(block) => {
                BlockOrExpr::Block(Box::new(self.rewrite_block_with_env(block, env)))
            }
            BlockOrExpr::Expr(expr) => BlockOrExpr::Expr(self.rewrite_expr_with_env(expr, env)),
        }
    }

    fn rewrite_stmt_with_env(
        &self,
        stmt: &Stmt,
        env: &mut HashMap<String, Option<LocalId>>,
    ) -> Stmt {
        match stmt {
            Stmt::Let { name, ty, init, span } => {
                let init_expr = self.rewrite_expr_with_env(init, env);
                env.insert(name.clone(), None);
                Stmt::Let {
                    name: name.clone(),
                    ty: ty.clone(),
                    init: init_expr,
                    span: span.clone(),
                }
            }
            Stmt::Assign { target, value, span } => Stmt::Assign {
                target: self.rewrite_expr_with_env(target, env),
                value: self.rewrite_expr_with_env(value, env),
                span: span.clone(),
            },
            Stmt::Expr { expr, span } => Stmt::Expr {
                expr: self.rewrite_expr_with_env(expr, env),
                span: span.clone(),
            },
            Stmt::Return { expr, span } => Stmt::Return {
                expr: expr.as_ref().map(|inner| self.rewrite_expr_with_env(inner, env)),
                span: span.clone(),
            },
            Stmt::Break { span } => Stmt::Break { span: span.clone() },
            Stmt::Continue { span } => Stmt::Continue { span: span.clone() },
            Stmt::ForIn { name, iter, body, span } => {
                let iter_expr = self.rewrite_expr_with_env(iter, env);
                let mut body_env = env.clone();
                body_env.insert(name.clone(), None);
                let body_block = self.rewrite_block_with_env(body, &mut body_env);
                Stmt::ForIn {
                    name: name.clone(),
                    iter: iter_expr,
                    body: body_block,
                    span: span.clone(),
                }
            }
            Stmt::Select { arms, span } => {
                let mut new_arms = Vec::with_capacity(arms.len());
                for arm in arms {
                    let mut arm_env = env.clone();
                    let kind = match &arm.kind {
                        crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                            crate::frontend::ast::SelectArmKind::Send {
                                chan: self.rewrite_expr_with_env(chan, &mut arm_env),
                                value: self.rewrite_expr_with_env(value, &mut arm_env),
                            }
                        }
                        crate::frontend::ast::SelectArmKind::Recv { chan, bind } => {
                            if let Some((val, ok)) = bind {
                                arm_env.insert(val.clone(), None);
                                arm_env.insert(ok.clone(), None);
                            }
                            crate::frontend::ast::SelectArmKind::Recv {
                                chan: self.rewrite_expr_with_env(chan, &mut arm_env),
                                bind: bind.clone(),
                            }
                        }
                        crate::frontend::ast::SelectArmKind::After { ms } => {
                            crate::frontend::ast::SelectArmKind::After {
                                ms: self.rewrite_expr_with_env(ms, &mut arm_env),
                            }
                        }
                        crate::frontend::ast::SelectArmKind::Default => {
                            crate::frontend::ast::SelectArmKind::Default
                        }
                    };
                    let body = self.rewrite_block_or_expr_with_env(&arm.body, &mut arm_env);
                    new_arms.push(crate::frontend::ast::SelectArm {
                        kind,
                        body,
                        span: arm.span.clone(),
                    });
                }
                Stmt::Select {
                    arms: new_arms,
                    span: span.clone(),
                }
            }
            Stmt::Go { expr, span } => Stmt::Go {
                expr: self.rewrite_expr_with_env(expr, env),
                span: span.clone(),
            },
            Stmt::Defer { expr, span } => Stmt::Defer {
                expr: self.rewrite_expr_with_env(expr, env),
                span: span.clone(),
            },
        }
    }

    fn shadow_pattern_bindings(
        &self,
        pattern: &Pattern,
        env: &mut HashMap<String, Option<LocalId>>,
    ) {
        match pattern {
            Pattern::Ident(name) => {
                env.insert(name.clone(), None);
            }
            Pattern::Variant { binds, .. } => {
                for name in binds {
                    env.insert(name.clone(), None);
                }
            }
            _ => {}
        }
    }

    fn can_lower_match_stmt(&self, arms: &[crate::frontend::ast::MatchArm]) -> bool {
        for arm in arms {
            match &arm.pattern {
                Pattern::Wildcard | Pattern::Bool(_) | Pattern::Int(_) => {}
                _ => return false,
            }
        }
        true
    }
}

fn max_expr_id(file: &crate::frontend::ast::FileAst) -> ExprId {
    let mut max_id = 0;
    for item in &file.items {
        if let Item::Function(func) = item {
            visit_block(&func.body, &mut max_id);
        }
    }
    max_id
}

fn visit_block(block: &Block, max_id: &mut ExprId) {
    for stmt in &block.stmts {
        visit_stmt(stmt, max_id);
    }
    if let Some(expr) = &block.tail {
        visit_expr(expr, max_id);
    }
}

fn visit_stmt(stmt: &Stmt, max_id: &mut ExprId) {
    match stmt {
        Stmt::Let { init, .. } => visit_expr(init, max_id),
        Stmt::Assign { target, value, .. } => {
            visit_expr(target, max_id);
            visit_expr(value, max_id);
        }
        Stmt::Expr { expr, .. } => visit_expr(expr, max_id),
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                visit_expr(expr, max_id);
            }
        }
        Stmt::ForIn { iter, body, .. } => {
            visit_expr(iter, max_id);
            visit_block(body, max_id);
        }
        Stmt::Select { arms, .. } => {
            for arm in arms {
                match &arm.kind {
                    crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                        visit_expr(chan, max_id);
                        visit_expr(value, max_id);
                    }
                    crate::frontend::ast::SelectArmKind::Recv { chan, .. } => {
                        visit_expr(chan, max_id);
                    }
                    crate::frontend::ast::SelectArmKind::After { ms } => {
                        visit_expr(ms, max_id);
                    }
                    crate::frontend::ast::SelectArmKind::Default => {}
                }
                match &arm.body {
                    BlockOrExpr::Block(block) => visit_block(block, max_id),
                    BlockOrExpr::Expr(expr) => visit_expr(expr, max_id),
                }
            }
        }
        Stmt::Go { expr, .. } | Stmt::Defer { expr, .. } => visit_expr(expr, max_id),
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
}

fn visit_expr(expr: &Expr, max_id: &mut ExprId) {
    if expr.id > *max_id {
        *max_id = expr.id;
    }
    match &expr.kind {
        ExprKind::Tuple(items) => {
            for item in items {
                visit_expr(item, max_id);
            }
        }
        ExprKind::Block(block) => visit_block(block, max_id),
        ExprKind::If { cond, then_block, else_block } => {
            visit_expr(cond, max_id);
            visit_block(then_block, max_id);
            if let Some(block) = else_block {
                visit_block(block, max_id);
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            visit_expr(scrutinee, max_id);
            for arm in arms {
                match &arm.body {
                    BlockOrExpr::Block(block) => visit_block(block, max_id),
                    BlockOrExpr::Expr(expr) => visit_expr(expr, max_id),
                }
            }
        }
        ExprKind::Call { callee, args, .. } => {
            visit_expr(callee, max_id);
            for arg in args {
                visit_expr(arg, max_id);
            }
        }
        ExprKind::Field { base, .. }
        | ExprKind::Unary { expr: base, .. }
        | ExprKind::Borrow { expr: base, .. }
        | ExprKind::Deref { expr: base, .. }
        | ExprKind::Try { expr: base }
        | ExprKind::Recv { chan: base }
        | ExprKind::Close { chan: base }
        | ExprKind::After { ms: base } => {
            visit_expr(base, max_id);
        }
        ExprKind::Index { base, index } => {
            visit_expr(base, max_id);
            visit_expr(index, max_id);
        }
        ExprKind::Binary { left, right, .. } => {
            visit_expr(left, max_id);
            visit_expr(right, max_id);
        }
        ExprKind::Send { chan, value } => {
            visit_expr(chan, max_id);
            visit_expr(value, max_id);
        }
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil
        | ExprKind::Ident(_) => {}
    }
}

fn expr_contains_try(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Try { .. } => true,
        ExprKind::Block(block) => block_contains_try(block),
        ExprKind::If { cond, then_block, else_block } => {
            expr_contains_try(cond)
                || block_contains_try(then_block)
                || else_block
                    .as_ref()
                    .map(|block| block_contains_try(block))
                    .unwrap_or(false)
        }
        ExprKind::Match { scrutinee, arms } => {
            if expr_contains_try(scrutinee) {
                return true;
            }
            for arm in arms {
                match &arm.body {
                    BlockOrExpr::Block(block) => {
                        if block_contains_try(block) {
                            return true;
                        }
                    }
                    BlockOrExpr::Expr(expr) => {
                        if expr_contains_try(expr) {
                            return true;
                        }
                    }
                }
            }
            false
        }
        ExprKind::Call { callee, args, .. } => {
            if expr_contains_try(callee) {
                return true;
            }
            args.iter().any(expr_contains_try)
        }
        ExprKind::Tuple(items) => items.iter().any(expr_contains_try),
        ExprKind::Field { base, .. }
        | ExprKind::Unary { expr: base, .. }
        | ExprKind::Borrow { expr: base, .. }
        | ExprKind::Deref { expr: base, .. }
        | ExprKind::Recv { chan: base }
        | ExprKind::Close { chan: base }
        | ExprKind::After { ms: base } => expr_contains_try(base),
        ExprKind::Index { base, index } => {
            expr_contains_try(base) || expr_contains_try(index)
        }
        ExprKind::Binary { left, right, .. } => {
            expr_contains_try(left) || expr_contains_try(right)
        }
        ExprKind::Send { chan, value } => {
            expr_contains_try(chan) || expr_contains_try(value)
        }
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil
        | ExprKind::Ident(_) => false,
    }
}

fn block_contains_try(block: &Block) -> bool {
    for stmt in &block.stmts {
        if stmt_contains_try(stmt) {
            return true;
        }
    }
    if let Some(expr) = &block.tail {
        return expr_contains_try(expr);
    }
    false
}

fn stmt_contains_try(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Let { init, .. } => expr_contains_try(init),
        Stmt::Assign { target, value, .. } => {
            expr_contains_try(target) || expr_contains_try(value)
        }
        Stmt::Expr { expr, .. } => expr_contains_try(expr),
        Stmt::Return { .. } => true,
        Stmt::ForIn { iter, body, .. } => {
            expr_contains_try(iter) || block_contains_try(body)
        }
        Stmt::Select { arms, .. } => {
            for arm in arms {
                match &arm.kind {
                    crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                        if expr_contains_try(chan) || expr_contains_try(value) {
                            return true;
                        }
                    }
                    crate::frontend::ast::SelectArmKind::Recv { chan, .. }
                    | crate::frontend::ast::SelectArmKind::After { ms: chan } => {
                        if expr_contains_try(chan) {
                            return true;
                        }
                    }
                    crate::frontend::ast::SelectArmKind::Default => {}
                }
                match &arm.body {
                    BlockOrExpr::Block(block) => {
                        if block_contains_try(block) {
                            return true;
                        }
                    }
                    BlockOrExpr::Expr(expr) => {
                        if expr_contains_try(expr) {
                            return true;
                        }
                    }
                }
            }
            false
        }
        Stmt::Go { expr, .. } | Stmt::Defer { expr, .. } => expr_contains_try(expr),
        Stmt::Break { .. } | Stmt::Continue { .. } => true,
    }
}
