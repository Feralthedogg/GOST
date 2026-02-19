use std::collections::{HashMap, HashSet};

// Purpose: Emit LLVM IR from validated MIR and semantic type metadata.
// Inputs/Outputs: Consumes MIR statements/expressions and appends LLVM instructions per block.
// Invariants: Backend treats semantic rejects as internal invariants; no user-facing rejects here.
// Gotchas: RC-handle operations depend on fresh-expression tracking and runtime ABI conventions.

use crate::frontend::ast::{Block, Expr, ExprId, ExprKind, Stmt, TypeAst};
use crate::frontend::symbols::logical_method_name;
use crate::intrinsics::{intrinsic_args_error, intrinsic_type_args_error};
use crate::mir::{MirStmt, Terminator};
use crate::sema::types::{BuiltinType, Type, TypeClass, TypeDefKind, TypeDefs, builtin_from_name};
use crate::sema::{ConstSig, ConstValue, ExternGlobalSig, FunctionSig, GlobalVarSig};

use super::{
    is_float_type, llvm_call_conv, llvm_storage_type, llvm_type, llvm_type_for_tuple_elem,
    zero_value,
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

fn escape_llvm_inline_asm(s: &str) -> String {
    let mut out = String::new();
    for b in s.bytes() {
        if (0x20..=0x7E).contains(&b) && b != b'"' && b != b'\\' {
            out.push(b as char);
        } else {
            out.push_str(&format!("\\{:02X}", b));
        }
    }
    out
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AsmOutputKind {
    WriteOnly,
    ReadWrite,
}

#[derive(Clone, Debug, Default)]
struct AsmConstraintSpec {
    outputs: Vec<AsmOutputKind>,
    readwrite_outputs: usize,
    input_count: usize,
    label_count: usize,
}

fn parse_asm_constraint_spec(raw: &str) -> AsmConstraintSpec {
    let mut spec = AsmConstraintSpec::default();
    for item in raw.split(',') {
        let c = item.trim();
        if c.is_empty() {
            continue;
        }
        if c == "!i" {
            spec.label_count += 1;
            continue;
        }
        if c.starts_with("~{") {
            continue;
        }
        if c.starts_with('=') {
            spec.outputs.push(AsmOutputKind::WriteOnly);
            continue;
        }
        if c.starts_with('+') {
            spec.outputs.push(AsmOutputKind::ReadWrite);
            spec.readwrite_outputs += 1;
            continue;
        }
        spec.input_count += 1;
    }
    spec
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

#[derive(Clone)]
struct EnumScrutInfo {
    enum_name: String,
    tag_ir: String,
    tag_llvm_ty: String,
    payload_ir: Option<String>,
}

#[derive(Clone)]
struct MapKeyOps {
    eq_fn: String,
    hash_fn: String,
    clone_fn: String,
    drop_fn: String,
}

pub(crate) struct FnEmitter<'a> {
    fn_name: String,
    fn_sigs: &'a HashMap<String, FunctionSig>,
    fn_symbols: &'a HashMap<String, String>,
    extern_globals: &'a HashMap<String, ExternGlobalSig>,
    globals: &'a HashMap<String, GlobalVarSig>,
    consts: &'a HashMap<String, ConstSig>,
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
    pub(crate) extra_functions: Vec<String>,
    go_counter: usize,
    drop_counter: usize,
    drop_fn_cache: HashMap<Type, String>,
    drop_fn_in_progress: HashSet<Type>,
    map_key_ops_cache: HashMap<Type, MapKeyOps>,
    map_key_ops_counter: usize,
    linear_states: HashMap<String, bool>,
    asm_labels: HashMap<String, usize>,
    mir_mode: bool,
    mir_local_ptrs: Vec<Option<MirLocalInfo>>,
    mir_expr_types: HashMap<ExprId, Type>,
}

impl<'a> FnEmitter<'a> {
    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub(crate) fn new(
        fn_name: impl Into<String>,
        fn_sigs: &'a HashMap<String, FunctionSig>,
        fn_symbols: &'a HashMap<String, String>,
        extern_globals: &'a HashMap<String, ExternGlobalSig>,
        globals: &'a HashMap<String, GlobalVarSig>,
        consts: &'a HashMap<String, ConstSig>,
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
            fn_symbols,
            extern_globals,
            globals,
            consts,
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
            extra_functions: Vec::new(),
            go_counter: 0,
            drop_counter: 0,
            drop_fn_cache: HashMap::new(),
            drop_fn_in_progress: HashSet::new(),
            map_key_ops_cache: HashMap::new(),
            map_key_ops_counter: 0,
            linear_states: HashMap::new(),
            asm_labels: HashMap::new(),
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

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
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

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub(crate) fn current_block_terminated(&self) -> bool {
        self.blocks[self.current].terminated
    }

    fn llvm_function_symbol(&self, name: &str) -> String {
        self.fn_symbols
            .get(name)
            .cloned()
            .unwrap_or_else(|| name.to_string())
    }

    fn invariant_violation(msg: &str) -> String {
        format!("internal codegen invariant violated: {}", msg)
    }

    fn is_integer_literal_ir(ir: &str) -> bool {
        if ir.is_empty() {
            return false;
        }
        let body = ir.strip_prefix('-').unwrap_or(ir);
        !body.is_empty() && body.bytes().all(|b| b.is_ascii_digit())
    }

    fn require_invariant(&self, cond: bool, msg: &str) -> Result<(), String> {
        debug_assert!(cond, "{}", msg);
        if cond {
            Ok(())
        } else {
            self.invariant_err(msg)
        }
    }

    fn require_not_invariant(&self, bad: bool, msg: &str) -> Result<(), String> {
        self.require_invariant(!bad, msg)
    }

    fn require_not_invariant_fmt(&self, bad: bool, msg: String) -> Result<(), String> {
        if bad {
            debug_assert!(!bad, "{}", msg);
            self.invariant_err_fmt(msg)
        } else {
            Ok(())
        }
    }

    fn invariant_err<T>(&self, msg: &str) -> Result<T, String> {
        Err(Self::invariant_violation(msg))
    }

    fn invariant_err_fmt<T>(&self, msg: String) -> Result<T, String> {
        Err(Self::invariant_violation(&msg))
    }

    #[cold]
    #[inline(never)]
    fn unreachable_internal(&self, msg: &str) -> ! {
        unreachable!("internal codegen invariant violated: {}", msg)
    }

    fn extract_direct_call<'b>(
        &self,
        expr: &'b Expr,
        call_expr_msg: &str,
        direct_call_msg: &str,
    ) -> Result<(&'b str, &'b [TypeAst], &'b [Expr]), String> {
        let (callee, type_args, args) = match &expr.kind {
            ExprKind::Call {
                callee,
                type_args,
                args,
            } => (callee, type_args.as_slice(), args.as_slice()),
            _ => return self.invariant_err(call_expr_msg),
        };
        match &callee.kind {
            ExprKind::Ident(name) => Ok((name.as_str(), type_args, args)),
            _ => self.invariant_err(direct_call_msg),
        }
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

    fn enum_def(&self, enum_name: &str) -> Result<&crate::sema::types::EnumDef, String> {
        match self.types.get(enum_name) {
            Some(TypeDefKind::Enum(def)) => Ok(def),
            _ => self.invariant_err_fmt(format!("unknown enum {}", enum_name)),
        }
    }

    fn enum_is_tag_only(&self, enum_name: &str) -> Result<bool, String> {
        let def = self.enum_def(enum_name)?;
        let fieldless = def.variants.iter().all(|(_, fields)| fields.is_empty());
        Ok(fieldless)
    }

    fn enum_tag_llvm_type(&self, enum_name: &str) -> Result<&'static str, String> {
        let def = self.enum_def(enum_name)?;
        Ok(def
            .layout
            .repr_int
            .map(|repr| repr.llvm_int_type())
            .unwrap_or("i32"))
    }

    fn build_enum_scrut_info(
        &mut self,
        enum_name: &str,
        enum_ir: &str,
    ) -> Result<EnumScrutInfo, String> {
        let tag = self.new_temp();
        self.emit(format!(
            "{} = extractvalue %{} {}, 0",
            tag, enum_name, enum_ir
        ));
        let payload_ir = if self.enum_is_tag_only(enum_name)? {
            None
        } else {
            let payload = self.new_temp();
            self.emit(format!(
                "{} = extractvalue %{} {}, 1",
                payload, enum_name, enum_ir
            ));
            Some(payload)
        };
        Ok(EnumScrutInfo {
            enum_name: enum_name.to_string(),
            tag_ir: tag,
            tag_llvm_ty: self.enum_tag_llvm_type(enum_name)?.to_string(),
            payload_ir,
        })
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub(crate) fn add_block(&mut self, name: String) -> usize {
        self.blocks.push(BlockInsts {
            name,
            instrs: Vec::new(),
            terminated: false,
        });
        self.blocks.len() - 1
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub(crate) fn switch_to(&mut self, idx: usize) {
        self.current = idx;
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub(crate) fn emit_prologue(
        &mut self,
        params: &[Type],
        names: &[String],
    ) -> Result<(), String> {
        for (idx, ty) in params.iter().enumerate() {
            let alloca = self.new_temp();
            self.emit(format!("{} = alloca {}", alloca, llvm_type(ty)?));
            self.emit(format!(
                "store {} %arg{}, {}* {}",
                llvm_type(ty)?,
                idx,
                llvm_type(ty)?,
                alloca
            ));
            let name = names
                .get(idx)
                .cloned()
                .unwrap_or_else(|| format!("arg{}", idx));
            self.locals.insert(name, (ty.clone(), alloca));
            if self.is_linear_type(ty) {
                let name = names
                    .get(idx)
                    .cloned()
                    .unwrap_or_else(|| format!("arg{}", idx));
                self.linear_states.insert(name, true);
            }
        }
        Ok(())
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub(crate) fn set_mir_mode(&mut self, enabled: bool) {
        self.mir_mode = enabled;
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
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
            self.mir_local_ptrs
                .push(Some(MirLocalInfo { name, ty, ptr }));
        }
        Ok(())
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub(crate) fn set_mir_expr_types(&mut self, expr_types: &HashMap<ExprId, Type>) {
        self.mir_expr_types = expr_types.clone();
    }

    fn mir_expr_type_override(&self, expr: &Expr) -> Option<Type> {
        self.mir_expr_types.get(&expr.id).cloned()
    }

    fn pattern_has_bindings(pattern: &crate::frontend::ast::Pattern) -> bool {
        match pattern {
            crate::frontend::ast::Pattern::Ident(_) => true,
            crate::frontend::ast::Pattern::Variant { binds, .. } => {
                binds.iter().any(|bind| bind != "_")
            }
            crate::frontend::ast::Pattern::Or(items) => {
                items.iter().any(Self::pattern_has_bindings)
            }
            _ => false,
        }
    }

    fn emit_match_pattern_cond(
        &mut self,
        pattern: &crate::frontend::ast::Pattern,
        scrut: &Value,
        enum_info: Option<&EnumScrutInfo>,
        result_tag: Option<&String>,
    ) -> Result<String, String> {
        match pattern {
            crate::frontend::ast::Pattern::Wildcard | crate::frontend::ast::Pattern::Ident(_) => {
                Ok("1".to_string())
            }
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
                Ok(tmp)
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
                Ok(tmp)
            }
            crate::frontend::ast::Pattern::Variant {
                enum_name, variant, ..
            } => {
                if let Some(tag_ir) = result_tag {
                    self.require_not_invariant(
                        enum_name != "Result" && enum_name != "result",
                        "enum pattern does not match scrutinee type",
                    )?;
                    let variant_idx = match variant.as_str() {
                        "Ok" | "ok" => 0,
                        "Err" | "err" => 1,
                        _ => return self.invariant_err("unknown enum variant"),
                    };
                    let tmp = self.new_temp();
                    self.emit(format!("{} = icmp eq i8 {}, {}", tmp, tag_ir, variant_idx));
                    Ok(tmp)
                } else {
                    let info = enum_info
                        .ok_or_else(|| Self::invariant_violation("enum pattern expects enum"))?;
                    self.require_not_invariant(
                        info.enum_name != enum_name.as_str(),
                        "enum pattern does not match scrutinee type",
                    )?;
                    let (variant_idx, _) = self.resolve_enum_variant(enum_name, variant)?;
                    let tmp = self.new_temp();
                    self.emit(format!(
                        "{} = icmp eq {} {}, {}",
                        tmp, info.tag_llvm_ty, info.tag_ir, variant_idx
                    ));
                    Ok(tmp)
                }
            }
            crate::frontend::ast::Pattern::Or(items) => {
                self.require_not_invariant(
                    items.is_empty(),
                    "or-pattern must contain at least one alternative",
                )?;
                let mut conds = Vec::new();
                for item in items {
                    let cond = self.emit_match_pattern_cond(item, scrut, enum_info, result_tag)?;
                    if cond == "1" {
                        return Ok("1".to_string());
                    }
                    if cond != "0" {
                        conds.push(cond);
                    }
                }
                if conds.is_empty() {
                    return Ok("0".to_string());
                }
                let mut acc = conds[0].clone();
                for cond in conds.iter().skip(1) {
                    let merged = self.new_temp();
                    self.emit(format!("{} = or i1 {}, {}", merged, acc, cond));
                    acc = merged;
                }
                Ok(acc)
            }
        }
    }

    fn emit_match_pattern_bindings(
        &mut self,
        pattern: &crate::frontend::ast::Pattern,
        scrut: &Value,
        enum_info: Option<&EnumScrutInfo>,
        result_tag: Option<&String>,
        shared_slots: Option<&HashMap<String, String>>,
    ) -> Result<(), String> {
        fn store_binding_value(
            this: &mut FnEmitter<'_>,
            name: &str,
            ty: &Type,
            value_ir: &str,
            shared_slots: Option<&HashMap<String, String>>,
        ) -> Result<(), String> {
            let storage_ty = llvm_type_for_tuple_elem(ty)?;
            if this.mir_mode
                && let Some((existing_ty, existing_ptr)) = this.locals.get(name).cloned()
            {
                this.require_invariant(existing_ty == *ty, "match binding local type mismatch")?;
                this.emit(format!(
                    "store {} {}, {}* {}",
                    storage_ty, value_ir, storage_ty, existing_ptr
                ));
                return Ok(());
            }
            if let Some(slot) = shared_slots.and_then(|slots| slots.get(name)) {
                this.emit(format!(
                    "store {} {}, {}* {}",
                    storage_ty, value_ir, storage_ty, slot
                ));
                return Ok(());
            }
            let alloca = this.new_temp();
            this.emit(format!("{} = alloca {}", alloca, storage_ty));
            this.emit(format!(
                "store {} {}, {}* {}",
                storage_ty, value_ir, storage_ty, alloca
            ));
            this.locals.insert(name.to_string(), (ty.clone(), alloca));
            if let Some(scope) = this.scopes.last_mut() {
                scope.push(name.to_string());
            }
            Ok(())
        }

        fn infer_pattern_binding_types(
            this: &FnEmitter<'_>,
            pattern: &crate::frontend::ast::Pattern,
            scrut_ty: &Type,
        ) -> Result<HashMap<String, Type>, String> {
            match pattern {
                crate::frontend::ast::Pattern::Wildcard
                | crate::frontend::ast::Pattern::Bool(_)
                | crate::frontend::ast::Pattern::Int(_) => Ok(HashMap::new()),
                crate::frontend::ast::Pattern::Ident(name) => {
                    let mut out = HashMap::new();
                    out.insert(name.clone(), scrut_ty.clone());
                    Ok(out)
                }
                crate::frontend::ast::Pattern::Variant {
                    enum_name,
                    variant,
                    binds,
                } => {
                    let mut out = HashMap::new();
                    if let Type::Result(ok_ty, err_ty) = scrut_ty {
                        this.require_not_invariant(
                            enum_name != "Result" && enum_name != "result",
                            "enum pattern does not match scrutinee type",
                        )?;
                        let field_ty = match variant.as_str() {
                            "Ok" | "ok" => ok_ty.as_ref().clone(),
                            "Err" | "err" => err_ty.as_ref().clone(),
                            _ => return this.invariant_err("unknown enum variant"),
                        };
                        if let Some(bind) = binds.first()
                            && bind != "_"
                        {
                            out.insert(bind.clone(), field_ty);
                        }
                        return Ok(out);
                    }
                    let scrut_name = match scrut_ty {
                        Type::Named(name) => name,
                        _ => return this.invariant_err("enum pattern expects enum"),
                    };
                    this.require_not_invariant(
                        scrut_name != enum_name,
                        "enum pattern does not match scrutinee type",
                    )?;
                    let (_, field_tys) = this.resolve_enum_variant(enum_name, variant)?;
                    for (idx, field_ty) in field_tys.iter().enumerate() {
                        if let Some(bind) = binds.get(idx)
                            && bind != "_"
                        {
                            out.insert(bind.clone(), field_ty.clone());
                        }
                    }
                    Ok(out)
                }
                crate::frontend::ast::Pattern::Or(items) => {
                    if items.is_empty() {
                        return this
                            .invariant_err("or-pattern must contain at least one alternative");
                    }
                    let mut canonical: Option<HashMap<String, Type>> = None;
                    for item in items {
                        let binds = infer_pattern_binding_types(this, item, scrut_ty)?;
                        if let Some(expected) = &canonical {
                            if expected != &binds {
                                return this.invariant_err(
                                    "or-pattern bindings must have identical names and types",
                                );
                            }
                        } else {
                            canonical = Some(binds);
                        }
                    }
                    Ok(canonical.unwrap_or_default())
                }
            }
        }

        match pattern {
            crate::frontend::ast::Pattern::Ident(name) => {
                if scrut.ty != Type::Builtin(BuiltinType::Unit) {
                    self.emit_shared_inc_value(scrut)?;
                    store_binding_value(self, name, &scrut.ty, &scrut.ir, shared_slots)?;
                } else if shared_slots.is_none() {
                    let alloca = self.new_temp();
                    self.emit(format!("{} = alloca i8", alloca));
                    self.locals.insert(name.clone(), (scrut.ty.clone(), alloca));
                    if let Some(scope) = self.scopes.last_mut() {
                        scope.push(name.clone());
                    }
                }
                Ok(())
            }
            crate::frontend::ast::Pattern::Variant {
                enum_name,
                variant,
                binds,
            } => {
                if let Type::Result(ok_ty, err_ty) = &scrut.ty {
                    self.require_not_invariant(
                        enum_name != "Result" && enum_name != "result",
                        "enum pattern does not match scrutinee type",
                    )?;
                    let (field_ty, field_idx) = match variant.as_str() {
                        "Ok" | "ok" => (ok_ty.as_ref().clone(), 1usize),
                        "Err" | "err" => (err_ty.as_ref().clone(), 2usize),
                        _ => return self.invariant_err("unknown enum variant"),
                    };
                    if let Some(bind) = binds.first().cloned()
                        && bind != "_"
                    {
                        let tmp = self.new_temp();
                        self.emit(format!(
                            "{} = extractvalue {} {}, {}",
                            tmp,
                            llvm_type(&scrut.ty)?,
                            scrut.ir,
                            field_idx
                        ));
                        let val = Value {
                            ty: field_ty.clone(),
                            ir: tmp.clone(),
                        };
                        self.emit_shared_inc_value(&val)?;
                        store_binding_value(self, &bind, &field_ty, &tmp, shared_slots)?;
                    }
                    return Ok(());
                }
                let info = enum_info
                    .ok_or_else(|| Self::invariant_violation("enum pattern expects enum"))?;
                self.require_not_invariant(
                    info.enum_name != enum_name.as_str(),
                    "enum pattern does not match scrutinee type",
                )?;
                let (variant_idx, field_tys) = self.resolve_enum_variant(enum_name, variant)?;
                if !field_tys.is_empty() {
                    let payload_ir = info.payload_ir.as_ref().ok_or_else(|| {
                        Self::invariant_violation(
                            "enum variant payload missing in tag-only enum representation",
                        )
                    })?;
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
                        let val = Value {
                            ty: field_ty.clone(),
                            ir: tmp.clone(),
                        };
                        self.emit_shared_inc_value(&val)?;
                        store_binding_value(self, &bind, field_ty, &tmp, shared_slots)?;
                    }
                }
                Ok(())
            }
            crate::frontend::ast::Pattern::Or(items) => {
                self.require_not_invariant(
                    items.is_empty(),
                    "or-pattern must contain at least one alternative",
                )?;
                if !Self::pattern_has_bindings(pattern) {
                    return Ok(());
                }
                let mut owned_slots: Option<HashMap<String, String>> = None;
                if shared_slots.is_none() {
                    let mut inferred = infer_pattern_binding_types(self, pattern, &scrut.ty)?;
                    let mut names = inferred.keys().cloned().collect::<Vec<_>>();
                    names.sort();
                    let mut slots = HashMap::new();
                    for name in names {
                        let ty = inferred.remove(&name).ok_or_else(|| {
                            Self::invariant_violation("missing inferred binding type")
                        })?;
                        let alloca = self.new_temp();
                        self.emit(format!(
                            "{} = alloca {}",
                            alloca,
                            llvm_type_for_tuple_elem(&ty)?
                        ));
                        self.locals.insert(name.clone(), (ty, alloca.clone()));
                        if let Some(scope) = self.scopes.last_mut() {
                            scope.push(name.clone());
                        }
                        slots.insert(name, alloca);
                    }
                    owned_slots = Some(slots);
                }
                let active_slots = shared_slots.or(owned_slots.as_ref());
                let done_name = self.new_block("match_or_bind_done");
                let done_idx = self.add_block(done_name.clone());
                for (idx, item) in items.iter().enumerate() {
                    let bind_name = self.new_block("match_or_bind_alt");
                    let bind_idx = self.add_block(bind_name.clone());
                    let is_last = idx + 1 == items.len();
                    let next_name = if is_last {
                        self.new_block("match_or_bind_fail")
                    } else {
                        self.new_block("match_or_bind_next")
                    };
                    let next_idx = self.add_block(next_name.clone());
                    let cond_ir =
                        self.emit_match_pattern_cond(item, scrut, enum_info, result_tag)?;
                    if cond_ir == "1" {
                        self.terminate(format!("br label %{}", bind_name));
                    } else if cond_ir == "0" {
                        self.terminate(format!("br label %{}", next_name));
                    } else {
                        self.terminate(format!(
                            "br i1 {}, label %{}, label %{}",
                            cond_ir, bind_name, next_name
                        ));
                    }
                    self.switch_to(bind_idx);
                    self.emit_match_pattern_bindings(
                        item,
                        scrut,
                        enum_info,
                        result_tag,
                        active_slots,
                    )?;
                    if !self.current_block_terminated() {
                        self.terminate(format!("br label %{}", done_name));
                    }
                    self.switch_to(next_idx);
                    if is_last {
                        self.terminate("unreachable");
                    }
                }
                self.switch_to(done_idx);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn collect_switch_pattern_literals(
        &self,
        pattern: &crate::frontend::ast::Pattern,
        out: &mut Vec<String>,
    ) -> Result<bool, String> {
        match pattern {
            crate::frontend::ast::Pattern::Bool(value) => {
                out.push(if *value {
                    "1".to_string()
                } else {
                    "0".to_string()
                });
                Ok(false)
            }
            crate::frontend::ast::Pattern::Int(value) => {
                out.push(value.clone());
                Ok(false)
            }
            crate::frontend::ast::Pattern::Wildcard | crate::frontend::ast::Pattern::Ident(_) => {
                Ok(true)
            }
            crate::frontend::ast::Pattern::Or(items) => {
                self.require_not_invariant(
                    items.is_empty(),
                    "or-pattern must contain at least one alternative",
                )?;
                for item in items {
                    if self.collect_switch_pattern_literals(item, out)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            _ => self.invariant_err("unsupported pattern in MIR match terminator"),
        }
    }

    fn emit_block_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Let { name, init, .. } | Stmt::Const { name, init, .. } => {
                let value = self.emit_expr(init)?;
                if !self.is_rc_fresh_expr(init) {
                    self.emit_shared_inc_value(&value)?;
                }
                let alloca = self.new_temp();
                self.emit(format!(
                    "{} = alloca {}",
                    alloca,
                    llvm_storage_type(&value.ty)?
                ));
                if value.ty == Type::Builtin(BuiltinType::Unit) {
                    self.emit(format!("store i8 0, i8* {}", alloca));
                } else {
                    self.emit(format!(
                        "store {} {}, {}* {}",
                        llvm_type(&value.ty)?,
                        value.ir,
                        llvm_storage_type(&value.ty)?,
                        alloca
                    ));
                }
                self.locals.insert(name.clone(), (value.ty.clone(), alloca));
                if let Some(scope) = self.scopes.last_mut() {
                    scope.push(name.clone());
                }
                if self.is_linear_type(&value.ty) {
                    self.linear_states.insert(name.clone(), true);
                }
                Ok(())
            }
            Stmt::Assign {
                op, target, value, ..
            } => {
                let (target_ty, target_ptr) = self.emit_place_ptr(target)?;
                let target_name = match &target.kind {
                    ExprKind::Ident(name) => Some(name.as_str()),
                    _ => None,
                };
                if *op == crate::frontend::ast::AssignOp::Assign && self.needs_drop(&target_ty) {
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
                let mut val = self.emit_expr(value)?;
                if !self.is_rc_fresh_expr(value) {
                    self.emit_shared_inc_value(&val)?;
                }
                if *op != crate::frontend::ast::AssignOp::Assign {
                    val = self.emit_compound_assign_value(op, &target_ty, &target_ptr, val)?;
                }
                if target_ty != val.ty && self.is_int_type(&target_ty) && self.is_int_type(&val.ty)
                {
                    val = self.cast_int_value(val, &target_ty)?;
                } else if target_ty != val.ty && is_float_type(&target_ty) && is_float_type(&val.ty)
                {
                    val = self.cast_value(val, &target_ty)?;
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
                if let Some(name) = target_name
                    && self.is_linear_type(&target_ty)
                {
                    self.mark_assigned(name);
                }
                Ok(())
            }
            Stmt::Expr { expr, .. } => {
                let _ = self.emit_expr(expr)?;
                Ok(())
            }
            Stmt::Return { expr, .. } => {
                if let Some(expr) = expr {
                    let val = self.emit_expr(expr)?;
                    self.emit_return_value(Some(val))
                } else {
                    self.emit_return_value(None)
                }
            }
            Stmt::Go { expr, .. } => self.emit_go_stmt(expr),
            _ => self.invariant_err("unsupported statement in MIR block expression"),
        }
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub(crate) fn emit_block(&mut self, block: &Block) -> Result<Option<Value>, String> {
        self.require_invariant(
            self.mir_mode,
            "high-level AST block codegen path is removed; lower to MIR before codegen",
        )?;
        self.enter_scope();
        for stmt in &block.stmts {
            self.emit_block_stmt(stmt)?;
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

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
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

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub(crate) fn emit_error_return(&mut self, err_ir: String) -> Result<(), String> {
        let err_val = Value {
            ty: Type::Builtin(BuiltinType::Error),
            ir: err_ir,
        };
        let ret_type = self.ret_type.clone();
        match &ret_type {
            Type::Builtin(BuiltinType::Error) => self.emit_return_value(Some(err_val)),
            Type::Result(ok_ty, err_ty) => {
                self.require_not_invariant(
                    **err_ty != Type::Builtin(BuiltinType::Error),
                    "`?` expects function return type error or (T, error)",
                )?;
                let result_ty = Type::Result(ok_ty.clone(), err_ty.clone());
                let llvm_result = llvm_type(&result_ty)?;
                let zero = if **ok_ty == Type::Builtin(BuiltinType::Unit) {
                    "0".to_string()
                } else {
                    zero_value(ok_ty)?
                };
                let tag_tmp = self.new_temp();
                self.emit(format!(
                    "{} = insertvalue {} undef, i8 1, 0",
                    tag_tmp, llvm_result
                ));
                let ok_tmp = self.new_temp();
                self.emit(format!(
                    "{} = insertvalue {} {}, {} {}, 1",
                    ok_tmp,
                    llvm_result,
                    tag_tmp,
                    llvm_type_for_tuple_elem(ok_ty)?,
                    zero
                ));
                let err_tmp = self.new_temp();
                self.emit(format!(
                    "{} = insertvalue {} {}, {} {}, 2",
                    err_tmp,
                    llvm_result,
                    ok_tmp,
                    llvm_type_for_tuple_elem(err_ty)?,
                    err_val.ir
                ));
                self.emit_return_value(Some(Value {
                    ty: result_ty,
                    ir: err_tmp,
                }))
            }
            Type::Tuple(items) if items.len() == 2 => {
                let ok_ty = &items[0];
                let err_ty = &items[1];
                self.require_not_invariant(
                    *err_ty != Type::Builtin(BuiltinType::Error),
                    "`?` expects function return type error or (T, error)",
                )?;
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
                self.emit_return_value(Some(Value {
                    ty: tuple_ty,
                    ir: tmp1,
                }))
            }
            _ => self.invariant_err("`?` expects function return type error or (T, error)"),
        }
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub(crate) fn emit_error_return_value(&mut self, value: Value) -> Result<(), String> {
        self.emit_error_return(value.ir)
    }

    fn emit_const_value(&mut self, sig: &ConstSig) -> Result<Value, String> {
        let ty = sig.ty.clone();
        let ir = match (&sig.ty, &sig.value) {
            (Type::Builtin(BuiltinType::Bool), ConstValue::Bool(v)) => {
                if *v {
                    "1".to_string()
                } else {
                    "0".to_string()
                }
            }
            (Type::Builtin(BuiltinType::I8), ConstValue::Int(v))
            | (Type::Builtin(BuiltinType::I16), ConstValue::Int(v))
            | (Type::Builtin(BuiltinType::I32), ConstValue::Int(v))
            | (Type::Builtin(BuiltinType::I64), ConstValue::Int(v))
            | (Type::Builtin(BuiltinType::Isize), ConstValue::Int(v)) => v.to_string(),
            (Type::Builtin(BuiltinType::U8), ConstValue::UInt(v))
            | (Type::Builtin(BuiltinType::U16), ConstValue::UInt(v))
            | (Type::Builtin(BuiltinType::U32), ConstValue::UInt(v))
            | (Type::Builtin(BuiltinType::U64), ConstValue::UInt(v))
            | (Type::Builtin(BuiltinType::Usize), ConstValue::UInt(v)) => v.to_string(),
            (Type::Builtin(BuiltinType::Char), ConstValue::Char(c)) => (*c as u32).to_string(),
            (Type::Builtin(BuiltinType::F32), ConstValue::Float(v)) => format!("{}", *v as f32),
            (Type::Builtin(BuiltinType::F64), ConstValue::Float(v)) => format!("{}", v),
            (Type::Builtin(BuiltinType::String), ConstValue::String(s)) => {
                return self.emit_string_literal(s);
            }
            _ => {
                self.unreachable_internal("unsupported const value");
            }
        };
        Ok(Value { ty, ir })
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub(crate) fn emit_expr(&mut self, expr: &Expr) -> Result<Value, String> {
        match &expr.kind {
            ExprKind::Bool(value) => Ok(Value {
                ty: Type::Builtin(BuiltinType::Bool),
                ir: if *value {
                    "1".to_string()
                } else {
                    "0".to_string()
                },
            }),
            ExprKind::Int(value) => Ok(Value {
                ty: self
                    .mir_expr_type_override(expr)
                    .unwrap_or(Type::Builtin(BuiltinType::I32)),
                ir: value.clone(),
            }),
            ExprKind::Float(value) => Ok(Value {
                ty: self
                    .mir_expr_type_override(expr)
                    .unwrap_or(Type::Builtin(BuiltinType::F64)),
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
                        return Ok(Value {
                            ty,
                            ir: String::new(),
                        });
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
                } else if let Some(c) = self.consts.get(name) {
                    self.emit_const_value(c)
                } else if let Some(sig) = self.fn_sigs.get(name) {
                    Ok(Value {
                        ty: Type::FnPtr {
                            params: sig.params.clone(),
                            ret: Box::new(sig.ret.clone()),
                            is_variadic: sig.is_variadic,
                        },
                        ir: format!("@{}", self.llvm_function_symbol(name)),
                    })
                } else if let Some(global) = self.globals.get(name) {
                    let ty = global.ty.clone();
                    if ty == Type::Builtin(BuiltinType::Unit) {
                        return Ok(Value {
                            ty,
                            ir: String::new(),
                        });
                    }
                    let tmp = self.new_temp();
                    self.emit(format!(
                        "{} = load {}, {}* @{}",
                        tmp,
                        llvm_type(&ty)?,
                        llvm_type(&ty)?,
                        name
                    ));
                    Ok(Value { ty, ir: tmp })
                } else if let Some(global) = self.extern_globals.get(name) {
                    let ty = global.ty.clone();
                    if ty == Type::Builtin(BuiltinType::Unit) {
                        return Ok(Value {
                            ty,
                            ir: String::new(),
                        });
                    }
                    let tmp = self.new_temp();
                    self.emit(format!(
                        "{} = load {}, {}* @{}",
                        tmp,
                        llvm_type(&ty)?,
                        llvm_type(&ty)?,
                        name
                    ));
                    Ok(Value { ty, ir: tmp })
                } else {
                    let block_name = self
                        .blocks
                        .get(self.current)
                        .map(|b| b.name.clone())
                        .unwrap_or_else(|| "<unknown>".to_string());
                    self.invariant_err_fmt(format!(
                        "unknown local {} in {}@{}",
                        name, self.fn_name, block_name
                    ))
                }
            }
            ExprKind::StructLit { name, fields } => {
                let def = match self.types.get(name) {
                    Some(TypeDefKind::Struct(def)) => def,
                    _ => return self.invariant_err_fmt(format!("unknown struct {}", name)),
                };
                let mut map: HashMap<String, &Expr> = HashMap::new();
                for (fname, fexpr) in fields.iter() {
                    map.insert(fname.clone(), fexpr);
                }
                let struct_ty = Type::Named(name.clone());
                let llvm_struct = llvm_type(&struct_ty)?;
                let mut cur = "undef".to_string();
                let mut have_any = false;
                for (idx, field) in def.fields.iter().enumerate() {
                    let fexpr = map.get(&field.name).ok_or_else(|| {
                        Self::invariant_violation(&format!(
                            "missing field {} in {}",
                            field.name, name
                        ))
                    })?;
                    let val = self.emit_expr(fexpr)?;
                    if !self.is_rc_fresh_expr(fexpr) {
                        self.emit_shared_inc_value(&val)?;
                    }
                    let val_ir = if val.ty == Type::Builtin(BuiltinType::Unit) {
                        "0".to_string()
                    } else {
                        val.ir.clone()
                    };
                    let base = if have_any {
                        cur.clone()
                    } else {
                        "undef".to_string()
                    };
                    let tmp = self.new_temp();
                    self.emit(format!(
                        "{} = insertvalue {} {}, {} {}, {}",
                        tmp,
                        llvm_struct,
                        base,
                        llvm_type(&field.ty)?,
                        val_ir,
                        idx
                    ));
                    cur = tmp;
                    have_any = true;
                }
                Ok(Value {
                    ty: struct_ty,
                    ir: cur,
                })
            }
            ExprKind::ArrayLit(items) => {
                let mut values = Vec::new();
                let mut elem_ty: Option<Type> = None;
                let mut target_len: Option<usize> = None;
                if let Some(Type::Array(inner, len)) = self.mir_expr_type_override(expr) {
                    elem_ty = Some((*inner).clone());
                    target_len = Some(len);
                }
                for item in items {
                    let val = if let Some(expected_elem) = elem_ty.as_ref() {
                        let mut emitted = self.emit_expr(item)?;
                        if emitted.ty != *expected_elem {
                            emitted = self.cast_value(emitted, expected_elem)?;
                        }
                        emitted
                    } else {
                        self.emit_expr(item)?
                    };
                    if elem_ty.is_none() {
                        elem_ty = Some(val.ty.clone());
                    }
                    if !self.is_rc_fresh_expr(item) {
                        self.emit_shared_inc_value(&val)?;
                    }
                    values.push(val);
                }
                let elem_ty = elem_ty.ok_or_else(|| {
                    Self::invariant_violation("cannot infer type of empty array literal")
                })?;
                let array_len = target_len.unwrap_or(values.len());
                let array_ty = Type::Array(Box::new(elem_ty.clone()), array_len);
                if values.is_empty() {
                    return Ok(Value {
                        ty: array_ty,
                        ir: "zeroinitializer".to_string(),
                    });
                }
                let llvm_array = llvm_type(&array_ty)?;
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
                        llvm_array,
                        current,
                        llvm_type_for_tuple_elem(&elem_ty)?,
                        elem_ir,
                        idx
                    ));
                    current = tmp;
                }
                Ok(Value {
                    ty: array_ty,
                    ir: current,
                })
            }
            ExprKind::Tuple(items) => {
                let mut values = Vec::new();
                let mut tys = Vec::new();
                for item in items {
                    let val = self.emit_expr(item)?;
                    if !self.is_rc_fresh_expr(item) {
                        self.emit_shared_inc_value(&val)?;
                    }
                    tys.push(val.ty.clone());
                    values.push(val);
                }
                let override_ty = self.mir_expr_type_override(expr);
                let result_ty = match &override_ty {
                    Some(Type::Result(ok, err)) => Some(Type::Result(ok.clone(), err.clone())),
                    _ => {
                        if tys.len() == 2 && tys[1] == Type::Builtin(BuiltinType::Error) {
                            Some(Type::Result(
                                Box::new(tys[0].clone()),
                                Box::new(tys[1].clone()),
                            ))
                        } else {
                            None
                        }
                    }
                };
                if let Some(result_ty) = result_ty {
                    let (ok_ty, err_ty) = match &result_ty {
                        Type::Result(ok, err) => (ok.as_ref().clone(), err.as_ref().clone()),
                        _ => unreachable!(),
                    };
                    self.require_not_invariant(
                        err_ty != Type::Builtin(BuiltinType::Error),
                        "Result tuple literal requires error in second position",
                    )?;
                    let ok_val = values
                        .first()
                        .ok_or_else(|| Self::invariant_violation("missing ok value"))?;
                    let err_val = values
                        .get(1)
                        .ok_or_else(|| Self::invariant_violation("missing err value"))?;
                    let is_ok = self.new_temp();
                    self.emit(format!("{} = icmp eq i32 {}, 0", is_ok, err_val.ir));
                    let tag_val = self.new_temp();
                    self.emit(format!("{} = select i1 {}, i8 0, i8 1", tag_val, is_ok));
                    let llvm_result = llvm_type(&result_ty)?;
                    let tag_tmp = self.new_temp();
                    self.emit(format!(
                        "{} = insertvalue {} undef, i8 {}, 0",
                        tag_tmp, llvm_result, tag_val
                    ));
                    let ok_ir = if ok_val.ty == Type::Builtin(BuiltinType::Unit) {
                        "0".to_string()
                    } else {
                        ok_val.ir.clone()
                    };
                    let ok_tmp = self.new_temp();
                    self.emit(format!(
                        "{} = insertvalue {} {}, {} {}, 1",
                        ok_tmp,
                        llvm_result,
                        tag_tmp,
                        llvm_type_for_tuple_elem(&ok_ty)?,
                        ok_ir
                    ));
                    let err_ir = if err_val.ty == Type::Builtin(BuiltinType::Unit) {
                        "0".to_string()
                    } else {
                        err_val.ir.clone()
                    };
                    let err_tmp = self.new_temp();
                    self.emit(format!(
                        "{} = insertvalue {} {}, {} {}, 2",
                        err_tmp,
                        llvm_result,
                        ok_tmp,
                        llvm_type_for_tuple_elem(&err_ty)?,
                        err_ir
                    ));
                    return Ok(Value {
                        ty: result_ty,
                        ir: err_tmp,
                    });
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
            ExprKind::UnsafeBlock(block) => {
                let value = self.emit_block(block)?;
                Ok(value.unwrap_or(Value {
                    ty: Type::Builtin(BuiltinType::Unit),
                    ir: String::new(),
                }))
            }
            ExprKind::If {
                cond,
                then_block,
                else_block,
            } => {
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
                let match_result_ty = self
                    .mir_expr_type_override(expr)
                    .unwrap_or(Type::Builtin(BuiltinType::Unit));
                let scrut = self.emit_expr(scrutinee)?;
                let enum_info = match &scrut.ty {
                    Type::Named(name) => match self.types.get(name) {
                        Some(TypeDefKind::Enum(_)) => {
                            Some(self.build_enum_scrut_info(name, &scrut.ir)?)
                        }
                        _ => None,
                    },
                    _ => None,
                };
                let result_tag = match &scrut.ty {
                    Type::Result(_, _) => {
                        let tag = self.new_temp();
                        self.emit(format!(
                            "{} = extractvalue {} {}, 0",
                            tag,
                            llvm_type(&scrut.ty)?,
                            scrut.ir
                        ));
                        Some(tag)
                    }
                    _ => None,
                };
                let end_name = self.new_block("match_end");
                let end_idx = self.add_block(end_name.clone());
                let nomatch_name = self.new_block("match_nomatch");
                let nomatch_idx = self.add_block(nomatch_name.clone());
                let mut incoming: Vec<(String, String, Type)> = Vec::new();
                let mut result_ty: Option<Type> =
                    (match_result_ty != Type::Builtin(BuiltinType::Unit))
                        .then_some(match_result_ty.clone());
                for (idx, arm) in arms.iter().enumerate() {
                    let is_last = idx + 1 == arms.len();
                    let arm_name = self.new_block("match_arm");
                    let arm_idx = self.add_block(arm_name.clone());
                    let next_name = if !is_last {
                        Some(self.new_block("match_next"))
                    } else {
                        None
                    };
                    let cond_ir = self.emit_match_pattern_cond(
                        &arm.pattern,
                        &scrut,
                        enum_info.as_ref(),
                        result_tag.as_ref(),
                    )?;
                    let fallback_name = next_name.clone().unwrap_or_else(|| nomatch_name.clone());
                    if cond_ir == "1" {
                        self.terminate(format!("br label %{}", arm_name));
                    } else {
                        self.terminate(format!(
                            "br i1 {}, label %{}, label %{}",
                            cond_ir, arm_name, fallback_name
                        ));
                    }
                    self.switch_to(arm_idx);
                    self.enter_scope();
                    self.emit_match_pattern_bindings(
                        &arm.pattern,
                        &scrut,
                        enum_info.as_ref(),
                        result_tag.as_ref(),
                        None,
                    )?;
                    if let Some(guard_expr) = &arm.guard {
                        let body_name = self.new_block("match_body");
                        let body_idx = self.add_block(body_name.clone());
                        let guard_val = self.emit_expr(guard_expr)?;
                        self.terminate(format!(
                            "br i1 {}, label %{}, label %{}",
                            guard_val.ir, body_name, fallback_name
                        ));
                        self.switch_to(body_idx);
                    }
                    let arm_val = match &arm.body {
                        crate::frontend::ast::BlockOrExpr::Block(block) => {
                            self.emit_block(block)?
                        }
                        crate::frontend::ast::BlockOrExpr::Expr(expr) => {
                            Some(self.emit_expr(expr)?)
                        }
                    };
                    self.exit_scope()?;
                    if let Some(value) = arm_val {
                        if !self.current_block_terminated() {
                            self.terminate(format!("br label %{}", end_name));
                        }
                        if value.ty != Type::Builtin(BuiltinType::Unit) {
                            if result_ty.is_none() {
                                result_ty = Some(value.ty.clone());
                            }
                            let mut target_ty = result_ty
                                .as_ref()
                                .cloned()
                                .unwrap_or_else(|| value.ty.clone());
                            if target_ty != value.ty
                                && self.is_int_type(&target_ty)
                                && self.is_int_type(&value.ty)
                                && let (Some((_t_signed, t_bits)), Some((_v_signed, v_bits))) =
                                    (self.int_info(&target_ty), self.int_info(&value.ty))
                                && v_bits > t_bits
                            {
                                for (prev_ir, _prev_block, prev_ty) in &mut incoming {
                                    if *prev_ty == target_ty
                                        && !Self::is_integer_literal_ir(prev_ir)
                                    {
                                        return self.invariant_err(
                                            "match integer arm widening requires literal earlier arm values",
                                        );
                                    }
                                    if self.is_int_type(prev_ty) && Self::is_integer_literal_ir(prev_ir) {
                                        *prev_ty = value.ty.clone();
                                    }
                                }
                                target_ty = value.ty.clone();
                                result_ty = Some(target_ty.clone());
                            }
                            let value = if value.ty != target_ty {
                                self.cast_value(value, &target_ty)?
                            } else {
                                value
                            };
                            let incoming_block = self.blocks[self.current].name.clone();
                            incoming.push((value.ir, incoming_block, target_ty));
                        }
                    }
                    if let Some(next_name) = next_name {
                        let next_idx = self.add_block(next_name.clone());
                        self.switch_to(next_idx);
                    }
                }
                let nomatch_returns_unit = match &result_ty {
                    Some(ty) => *ty == Type::Builtin(BuiltinType::Unit),
                    None => match_result_ty == Type::Builtin(BuiltinType::Unit),
                };
                self.switch_to(nomatch_idx);
                if nomatch_returns_unit {
                    self.terminate(format!("br label %{}", end_name));
                } else {
                    self.terminate("unreachable");
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
                        for (val, block, _ty) in incoming {
                            incoming_ir.push(format!("[ {}, %{} ]", val, block));
                        }
                        self.emit(format!(
                            "{} = phi {} {}",
                            phi,
                            llvm_type(&res_ty)?,
                            incoming_ir.join(", ")
                        ));
                        Ok(Value {
                            ty: res_ty,
                            ir: phi,
                        })
                    }
                } else {
                    Ok(Value {
                        ty: Type::Builtin(BuiltinType::Unit),
                        ir: String::new(),
                    })
                }
            }
            ExprKind::Closure { .. } => self.unreachable_internal(
                "MIR lowering invariant: closure literal must be lowered before backend",
            ),
            ExprKind::Call {
                callee,
                type_args,
                args,
            } => {
                if let ExprKind::Field { base, name } = &callee.kind
                    && let ExprKind::Ident(pkg_name) = &base.kind
                {
                    let namespaced = format!("{}.{}", pkg_name, name);
                    if self.fn_sigs.contains_key(&namespaced) {
                        let mut resolved_args = Vec::new();
                        for arg in args {
                            resolved_args.push(self.emit_expr(arg)?);
                        }
                        let mut resolved_types = Vec::new();
                        for arg in type_args {
                            resolved_types.push(self.resolve_type_ast(arg)?);
                        }
                        return self.emit_call_by_name(&namespaced, &resolved_types, resolved_args);
                    }
                }
                if let ExprKind::Field {
                    base,
                    name: variant,
                } = &callee.kind
                    && let ExprKind::Ident(enum_name) = &base.kind
                    && !self.locals.contains_key(enum_name)
                {
                    if enum_name == "Result" || enum_name == "result" {
                        let mut resolved_types = Vec::new();
                        for arg in type_args {
                            resolved_types.push(self.resolve_type_ast(arg)?);
                        }
                        return self.emit_result_ctor(expr, variant, &resolved_types, args);
                    }
                    return self.emit_enum_ctor(enum_name, variant, args);
                }
                if let ExprKind::Field { base, name: method } = &callee.kind {
                    let base_val = self.emit_expr(base)?;
                    if Self::is_interface_object_type(&base_val.ty) {
                        let mut resolved_args = Vec::new();
                        for arg in args {
                            resolved_args.push(self.emit_expr(arg)?);
                        }
                        return self.emit_interface_method_call(base_val, method, resolved_args);
                    }
                }
                if let ExprKind::Ident(name) = &callee.kind {
                    if name == "asm" || name == "asm_pure" || name == "asm_volatile" {
                        return self.emit_inline_asm_call(name, type_args, args);
                    }
                    if name == "asm_label" {
                        return self.emit_inline_asm_label(args);
                    }
                    if name == "asm_goto" {
                        return self.emit_inline_asm_goto_call(type_args, args);
                    }
                }
                let mut resolved_args = Vec::new();
                for arg in args {
                    resolved_args.push(self.emit_expr(arg)?);
                }
                if let ExprKind::Ident(name) = &callee.kind {
                    if self.locals.contains_key(name)
                        || self.globals.contains_key(name)
                        || self.extern_globals.contains_key(name)
                        || self.consts.contains_key(name)
                    {
                        let callee_val = self.emit_expr(callee)?;
                        return self.emit_call_ptr(callee_val, resolved_args);
                    }
                    let mut resolved_types = Vec::new();
                    for arg in type_args {
                        resolved_types.push(self.resolve_type_ast(arg)?);
                    }
                    return self.emit_call_by_name(name, &resolved_types, resolved_args);
                }
                let callee_val = self.emit_expr(callee)?;
                self.emit_call_ptr(callee_val, resolved_args)
            }
            ExprKind::Borrow {
                is_mut,
                expr: inner,
            } => {
                let (ty, ptr) = self.emit_place_ptr(inner)?;
                let ref_ty = if *is_mut {
                    Type::MutRef(Box::new(ty))
                } else {
                    Type::Ref(Box::new(ty))
                };
                Ok(Value {
                    ty: ref_ty,
                    ir: ptr,
                })
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
                        Ok(Value {
                            ty: *inner,
                            ir: tmp,
                        })
                    }
                    _ => self.invariant_err("deref expects ref type"),
                }
            }
            ExprKind::Try { expr: inner } => {
                if self.mir_mode {
                    self.unreachable_internal("`?` must be lowered to MIR before codegen");
                }
                let value = self.emit_expr(inner)?;
                match &value.ty {
                    Type::Builtin(BuiltinType::Error) => {
                        let is_err = self.new_temp();
                        self.emit(format!("{} = icmp ne i32 {}, 0", is_err, value.ir));
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
                    Type::Result(ok_ty, err_ty) => {
                        self.require_not_invariant(
                            **err_ty != Type::Builtin(BuiltinType::Error),
                            "`?` expects Result[T, error]",
                        )?;
                        let err_tmp = self.new_temp();
                        self.emit(format!(
                            "{} = extractvalue {} {}, 2",
                            err_tmp,
                            llvm_type(&value.ty)?,
                            value.ir
                        ));
                        let is_err = self.new_temp();
                        self.emit(format!("{} = icmp ne i32 {}, 0", is_err, err_tmp));
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
                            "{} = extractvalue {} {}, 1",
                            ok_val,
                            llvm_type(&value.ty)?,
                            value.ir
                        ));
                        self.terminate(format!("br label %{}", cont_name));
                        self.switch_to(cont_idx);
                        if **ok_ty == Type::Builtin(BuiltinType::Unit) {
                            Ok(Value {
                                ty: *ok_ty.clone(),
                                ir: String::new(),
                            })
                        } else {
                            Ok(Value {
                                ty: *ok_ty.clone(),
                                ir: ok_val,
                            })
                        }
                    }
                    Type::Tuple(items) if items.len() == 2 => {
                        self.require_not_invariant(
                            items[1] != Type::Builtin(BuiltinType::Error),
                            "`?` expects (T, error)",
                        )?;
                        let err_tmp = self.new_temp();
                        self.emit(format!(
                            "{} = extractvalue {} {}, 1",
                            err_tmp,
                            llvm_type(&value.ty)?,
                            value.ir
                        ));
                        let is_err = self.new_temp();
                        self.emit(format!("{} = icmp ne i32 {}, 0", is_err, err_tmp));
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
                    _ => self.invariant_err("`?` expects error or (T, error)"),
                }
            }
            ExprKind::Send { chan, value } => {
                let chan_val = self.emit_expr(chan)?;
                let elem_ty = match &chan_val.ty {
                    Type::Chan(inner) => *inner.clone(),
                    _ => return self.invariant_err("send expects chan[T]"),
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
                    _ => return self.invariant_err("recv expects chan[T]"),
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
                self.emit(format!("{} = icmp eq i32 {}, 0", ok, status));
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
                let elem_llvm = llvm_type_for_tuple_elem(&elem_ty)?;
                let selected_val = self.new_temp();
                self.emit(format!(
                    "{} = select i1 {}, {} {}, {} {}",
                    selected_val, ok, elem_llvm, ready_val, elem_llvm, closed_val
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
                    tmp1, llvm_tuple, tmp0, ok
                ));
                Ok(Value {
                    ty: tuple_ty,
                    ir: tmp1,
                })
            }
            ExprKind::Close { chan } => {
                let chan_val = self.emit_expr(chan)?;
                self.require_not_invariant(
                    !matches!(chan_val.ty, Type::Chan(_)),
                    "close expects chan[T]",
                )?;
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
                    return self.invariant_err("after expects i32 milliseconds");
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
                    if let ExprKind::Ident(pkg_name) = &base.kind {
                        let namespaced = format!("{}.{}", pkg_name, name);
                        if let Some(sig) = self.fn_sigs.get(&namespaced) {
                            return Ok(Value {
                                ty: Type::FnPtr {
                                    params: sig.params.clone(),
                                    ret: Box::new(sig.ret.clone()),
                                    is_variadic: sig.is_variadic,
                                },
                                ir: format!("@{}", self.llvm_function_symbol(&namespaced)),
                            });
                        }
                    }
                    if let ExprKind::Ident(enum_name) = &base.kind
                        && !self.locals.contains_key(enum_name)
                        && let Ok((variant_idx, fields)) =
                            self.resolve_enum_variant(enum_name, name)
                    {
                        self.require_not_invariant(
                            !fields.is_empty(),
                            "enum variant requires arguments",
                        )?;
                        return self.emit_enum_value(enum_name, variant_idx, None);
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
                let val = Value {
                    ty: field_ty,
                    ir: tmp,
                };
                self.emit_shared_inc_value(&val)?;
                Ok(val)
            }
            ExprKind::Index { .. } => {
                if let ExprKind::Index { base, index } = &expr.kind {
                    let base_val = self.emit_expr(base)?;
                    if let Type::Tuple(items) = &base_val.ty {
                        let idx = match &index.kind {
                            ExprKind::Int(v) => v.parse::<usize>().ok(),
                            _ => None,
                        }
                        .ok_or_else(|| {
                            Self::invariant_violation("tuple index must be integer literal")
                        })?;
                        let elem_ty = items.get(idx).cloned().ok_or_else(|| {
                            Self::invariant_violation("tuple index out of bounds")
                        })?;
                        if elem_ty == Type::Builtin(BuiltinType::Unit) {
                            return Ok(Value {
                                ty: elem_ty,
                                ir: String::new(),
                            });
                        }
                        let tmp = self.new_temp();
                        self.emit(format!(
                            "{} = extractvalue {} {}, {}",
                            tmp,
                            llvm_type(&base_val.ty)?,
                            base_val.ir,
                            idx
                        ));
                        let val = Value {
                            ty: elem_ty,
                            ir: tmp,
                        };
                        self.emit_shared_inc_value(&val)?;
                        return Ok(val);
                    }
                }
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
                let val = Value {
                    ty: elem_ty,
                    ir: tmp,
                };
                self.emit_shared_inc_value(&val)?;
                Ok(val)
            }
            ExprKind::Unary { op, expr: inner } => {
                let value = self.emit_expr(inner)?;
                match op {
                    crate::frontend::ast::UnaryOp::Neg => {
                        if is_float_type(&value.ty) {
                            let tmp = self.new_temp();
                            self.emit(format!(
                                "{} = fsub {} 0.0, {}",
                                tmp,
                                llvm_type(&value.ty)?,
                                value.ir
                            ));
                            Ok(Value {
                                ty: value.ty,
                                ir: tmp,
                            })
                        } else {
                            let tmp = self.new_temp();
                            self.emit(format!(
                                "{} = sub {} 0, {}",
                                tmp,
                                llvm_type(&value.ty)?,
                                value.ir
                            ));
                            Ok(Value {
                                ty: value.ty,
                                ir: tmp,
                            })
                        }
                    }
                    crate::frontend::ast::UnaryOp::Not => {
                        self.require_not_invariant(
                            value.ty != Type::Builtin(BuiltinType::Bool),
                            "`!` expects bool",
                        )?;
                        let tmp = self.new_temp();
                        self.emit(format!("{} = xor i1 {}, true", tmp, value.ir));
                        Ok(Value {
                            ty: Type::Builtin(BuiltinType::Bool),
                            ir: tmp,
                        })
                    }
                    crate::frontend::ast::UnaryOp::BitNot => {
                        self.require_not_invariant(
                            !self.is_int_type(&value.ty),
                            "`~` expects integer",
                        )?;
                        let tmp = self.new_temp();
                        self.emit(format!(
                            "{} = xor {} {}, -1",
                            tmp,
                            llvm_type(&value.ty)?,
                            value.ir
                        ));
                        Ok(Value {
                            ty: value.ty,
                            ir: tmp,
                        })
                    }
                }
            }
            ExprKind::Cast { expr: inner, ty } => {
                let value = self.emit_expr(inner)?;
                let target_ty = self.resolve_type_ast(ty)?;
                self.cast_value(value, &target_ty)
            }
            ExprKind::Binary { op, left, right } => {
                let lhs0 = self.emit_expr(left)?;
                let rhs0 = self.emit_expr(right)?;
                if matches!(op, crate::frontend::ast::BinaryOp::Add)
                    && lhs0.ty == Type::Builtin(BuiltinType::String)
                    && rhs0.ty == Type::Builtin(BuiltinType::String)
                {
                    let (a_ptr, a_len) = self.emit_string_ptr_len_from_value(&lhs0)?;
                    let (b_ptr, b_len) = self.emit_string_ptr_len_from_value(&rhs0)?;
                    let out_ptr = self.new_temp();
                    self.emit(format!("{} = alloca %string", out_ptr));
                    self.emit(format!(
                        "call void @__gost_string_concat(%string* {}, i8* {}, i64 {}, i8* {}, i64 {})",
                        out_ptr, a_ptr, a_len, b_ptr, b_len
                    ));
                    let out = self.new_temp();
                    self.emit(format!("{} = load %string, %string* {}", out, out_ptr));
                    return Ok(Value {
                        ty: Type::Builtin(BuiltinType::String),
                        ir: out,
                    });
                }
                let (lhs, rhs) = if self.is_int_type(&lhs0.ty) && self.is_int_type(&rhs0.ty) {
                    self.coerce_int_pair(lhs0, rhs0)?
                } else {
                    (lhs0, rhs0)
                };
                if is_float_type(&lhs.ty) {
                    let tmp = self.new_temp();
                    let instr = match op {
                        crate::frontend::ast::BinaryOp::Add => "fadd",
                        crate::frontend::ast::BinaryOp::Sub => "fsub",
                        crate::frontend::ast::BinaryOp::Mul => "fmul",
                        crate::frontend::ast::BinaryOp::Div => "fdiv",
                        crate::frontend::ast::BinaryOp::Rem => "frem",
                        crate::frontend::ast::BinaryOp::Eq => "fcmp oeq",
                        crate::frontend::ast::BinaryOp::NotEq => "fcmp one",
                        crate::frontend::ast::BinaryOp::Lt => "fcmp olt",
                        crate::frontend::ast::BinaryOp::Lte => "fcmp ole",
                        crate::frontend::ast::BinaryOp::Gt => "fcmp ogt",
                        crate::frontend::ast::BinaryOp::Gte => "fcmp oge",
                        _ => return self.invariant_err("unsupported float binary op"),
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
                        crate::frontend::ast::BinaryOp::Div => {
                            if self.is_signed_int_type(&lhs.ty) {
                                "sdiv"
                            } else {
                                "udiv"
                            }
                        }
                        crate::frontend::ast::BinaryOp::Rem => {
                            if self.is_signed_int_type(&lhs.ty) {
                                "srem"
                            } else {
                                "urem"
                            }
                        }
                        crate::frontend::ast::BinaryOp::Eq => "icmp eq",
                        crate::frontend::ast::BinaryOp::NotEq => "icmp ne",
                        crate::frontend::ast::BinaryOp::Lt => {
                            if self.is_signed_int_type(&lhs.ty) {
                                "icmp slt"
                            } else {
                                "icmp ult"
                            }
                        }
                        crate::frontend::ast::BinaryOp::Lte => {
                            if self.is_signed_int_type(&lhs.ty) {
                                "icmp sle"
                            } else {
                                "icmp ule"
                            }
                        }
                        crate::frontend::ast::BinaryOp::Gt => {
                            if self.is_signed_int_type(&lhs.ty) {
                                "icmp sgt"
                            } else {
                                "icmp ugt"
                            }
                        }
                        crate::frontend::ast::BinaryOp::Gte => {
                            if self.is_signed_int_type(&lhs.ty) {
                                "icmp sge"
                            } else {
                                "icmp uge"
                            }
                        }
                        crate::frontend::ast::BinaryOp::And => "and",
                        crate::frontend::ast::BinaryOp::Or => "or",
                        crate::frontend::ast::BinaryOp::BitAnd => "and",
                        crate::frontend::ast::BinaryOp::BitOr => "or",
                        crate::frontend::ast::BinaryOp::BitXor => "xor",
                        crate::frontend::ast::BinaryOp::Shl => "shl",
                        crate::frontend::ast::BinaryOp::Shr => {
                            if self.is_signed_int_type(&lhs.ty) {
                                "ashr"
                            } else {
                                "lshr"
                            }
                        }
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
        if let Some(msg) = intrinsic_type_args_error(name, type_args.len()) {
            return self.invariant_err(msg);
        }
        if let Some(msg) = intrinsic_args_error(name, args.len()) {
            return self.invariant_err(msg);
        }
        if name == "panic" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "panic does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 1, "panic expects 1 argument")?;
            self.require_not_invariant(
                args[0].ty != Type::Builtin(BuiltinType::String),
                "panic expects string message",
            )?;
            let (msg_ptr, msg_len) = self.emit_string_ptr_len_from_value(&args[0])?;
            self.emit(format!(
                "call void @__gost_user_panic(i8* {}, i64 {})",
                msg_ptr, msg_len
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::Unit),
                ir: String::new(),
            });
        }
        if name == "recover" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "recover does not take type arguments",
            )?;
            self.require_not_invariant(!args.is_empty(), "recover expects 0 arguments")?;
            let out = self.new_temp();
            self.emit(format!("{} = call i32 @__gost_recover()", out));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::Error),
                ir: out,
            });
        }
        if name == "__gost_println" {
            self.require_invariant(
                type_args.is_empty(),
                "__gost_println does not take type arguments",
            )?;
            self.require_invariant(args.len() == 1, "__gost_println expects 1 argument")?;
            let arg = &args[0];
            self.require_invariant(
                arg.ty == Type::Builtin(BuiltinType::String),
                "__gost_println expects string",
            )?;
            let ptr = self.new_temp();
            let len = self.new_temp();
            self.emit(format!("{} = extractvalue %string {}, 0", ptr, arg.ir));
            self.emit(format!("{} = extractvalue %string {}, 1", len, arg.ir));
            self.emit(format!(
                "call void @__gost_println_str(i8* {}, i64 {})",
                ptr, len
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::Unit),
                ir: String::new(),
            });
        }
        if name == "string_len" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "string_len does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 1, "string_len expects one argument")?;
            self.require_not_invariant(
                args[0].ty != Type::Builtin(BuiltinType::String),
                "string_len expects string",
            )?;
            let (s_ptr, s_len) = self.emit_string_ptr_len_from_value(&args[0])?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i64 @__gost_string_len(i8* {}, i64 {})",
                tmp, s_ptr, s_len
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I64),
                ir: tmp,
            });
        }
        if name == "string_get" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "string_get does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 2, "string_get expects two arguments")?;
            self.require_not_invariant(
                args[0].ty != Type::Builtin(BuiltinType::String),
                "string_get expects string as first argument",
            )?;
            let (s_ptr, s_len) = self.emit_string_ptr_len_from_value(&args[0])?;
            let idx_ir = self.emit_index_i64(&args[1])?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i32 @__gost_string_get(i8* {}, i64 {}, i64 {})",
                tmp, s_ptr, s_len, idx_ir
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "string_slice" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "string_slice does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 3, "string_slice expects three arguments")?;
            self.require_not_invariant(
                args[0].ty != Type::Builtin(BuiltinType::String),
                "string_slice expects string as first argument",
            )?;
            let (s_ptr, s_len) = self.emit_string_ptr_len_from_value(&args[0])?;
            let start_ir = self.emit_index_i64(&args[1])?;
            let len_ir = self.emit_index_i64(&args[2])?;
            let out_ptr = self.new_temp();
            self.emit(format!("{} = alloca %string", out_ptr));
            self.emit(format!(
                "call void @__gost_string_slice(%string* {}, i8* {}, i64 {}, i64 {}, i64 {})",
                out_ptr, s_ptr, s_len, start_ir, len_ir
            ));
            let tmp = self.new_temp();
            self.emit(format!("{} = load %string, %string* {}", tmp, out_ptr));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::String),
                ir: tmp,
            });
        }
        if name == "string_concat" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "string_concat does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 2, "string_concat expects two arguments")?;
            self.require_not_invariant(
                args[0].ty != Type::Builtin(BuiltinType::String)
                    || args[1].ty != Type::Builtin(BuiltinType::String),
                "string_concat expects string arguments",
            )?;
            let (a_ptr, a_len) = self.emit_string_ptr_len_from_value(&args[0])?;
            let (b_ptr, b_len) = self.emit_string_ptr_len_from_value(&args[1])?;
            let out_ptr = self.new_temp();
            self.emit(format!("{} = alloca %string", out_ptr));
            self.emit(format!(
                "call void @__gost_string_concat(%string* {}, i8* {}, i64 {}, i8* {}, i64 {})",
                out_ptr, a_ptr, a_len, b_ptr, b_len
            ));
            let tmp = self.new_temp();
            self.emit(format!("{} = load %string, %string* {}", tmp, out_ptr));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::String),
                ir: tmp,
            });
        }
        if name == "string_from_byte" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "string_from_byte does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 1, "string_from_byte expects one argument")?;
            let b_ir = match args[0].ty {
                Type::Builtin(BuiltinType::I32) => args[0].ir.clone(),
                Type::Builtin(BuiltinType::I64) => {
                    let tmp = self.new_temp();
                    self.emit(format!("{} = trunc i64 {} to i32", tmp, args[0].ir));
                    tmp
                }
                _ => return self.invariant_err("string_from_byte expects i32 byte value"),
            };
            let out_ptr = self.new_temp();
            self.emit(format!("{} = alloca %string", out_ptr));
            self.emit(format!(
                "call void @__gost_string_from_byte(%string* {}, i32 {})",
                out_ptr, b_ir
            ));
            let tmp = self.new_temp();
            self.emit(format!("{} = load %string, %string* {}", tmp, out_ptr));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::String),
                ir: tmp,
            });
        }
        if name == "make_chan" {
            self.require_not_invariant(
                type_args.len() != 1,
                "make_chan MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 1, "make_chan expects one argument")?;
            let elem_ty = type_args[0].clone();
            let size_val = self.emit_size_of(&elem_ty)?;
            let cap_val = &args[0];
            self.require_not_invariant(
                cap_val.ty != Type::Builtin(BuiltinType::I32)
                    && cap_val.ty != Type::Builtin(BuiltinType::I64),
                "make_chan expects i32 cap",
            )?;
            let cap_ir = if cap_val.ty == Type::Builtin(BuiltinType::I32) {
                cap_val.ir.clone()
            } else {
                let tmp = self.new_temp();
                self.emit(format!("{} = trunc i64 {} to i32", tmp, cap_val.ir));
                tmp
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
            self.require_not_invariant(
                type_args.len() != 1,
                "make_slice MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 2, "make_slice expects two arguments")?;
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
        if name == "$for_in_len" {
            self.require_not_invariant(
                type_args.len() != 1,
                "$for_in_len MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 1, "invalid MIR arity for $for_in_len")?;
            let len_ir = self.emit_for_in_container_len(&args[0])?;
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I64),
                ir: len_ir,
            });
        }
        if name == "$for_in_get" {
            self.require_not_invariant(
                type_args.len() != 1,
                "$for_in_get MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 2, "invalid MIR arity for $for_in_get")?;
            let elem_ty = type_args[0].clone();
            let is_string_container = match &args[0].ty {
                Type::Builtin(BuiltinType::String) => true,
                Type::Ref(inner) | Type::MutRef(inner) => {
                    matches!(**inner, Type::Builtin(BuiltinType::String))
                }
                _ => false,
            };
            if is_string_container {
                self.require_not_invariant(
                    elem_ty != Type::Builtin(BuiltinType::U32),
                    "for_in string element type mismatch",
                )?;
                let (ptr, len) = self.emit_string_ptr_len_from_value(&args[0])?;
                let idx = if args[1].ty == Type::Builtin(BuiltinType::I64) {
                    args[1].ir.clone()
                } else {
                    let cast = self.new_temp();
                    self.emit(format!("{} = sext i32 {} to i64", cast, args[1].ir));
                    cast
                };
                let tmp = self.new_temp();
                self.emit(format!(
                    "{} = call i32 @__gost_string_get(i8* {}, i64 {}, i64 {})",
                    tmp, ptr, len, idx
                ));
                return Ok(Value {
                    ty: elem_ty,
                    ir: tmp,
                });
            }
            let elem_ptr = self.emit_for_in_elem_ptr(&args[0], &elem_ty, &args[1])?;
            let storage_ty = llvm_storage_type(&elem_ty)?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = load {}, {}* {}",
                tmp,
                llvm_type(&elem_ty)?,
                storage_ty,
                elem_ptr
            ));
            let val = Value {
                ty: elem_ty,
                ir: tmp,
            };
            self.emit_shared_inc_value(&val)?;
            return Ok(val);
        }
        if name == "$for_in_take" {
            self.require_not_invariant(
                type_args.len() != 1,
                "$for_in_take MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 2, "invalid MIR arity for $for_in_take")?;
            self.require_not_invariant(
                !matches!(args[0].ty, Type::MutRef(_)),
                "invalid MIR receiver kind for $for_in_take",
            )?;
            let elem_ty = type_args[0].clone();
            let elem_ptr = self.emit_for_in_elem_ptr(&args[0], &elem_ty, &args[1])?;
            let storage_ty = llvm_storage_type(&elem_ty)?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = load {}, {}* {}",
                tmp,
                llvm_type(&elem_ty)?,
                storage_ty,
                elem_ptr
            ));
            if elem_ty == Type::Builtin(BuiltinType::Unit) {
                self.emit(format!("store i8 0, i8* {}", elem_ptr));
            } else {
                let zero = zero_value(&elem_ty)?;
                self.emit(format!(
                    "store {} {}, {}* {}",
                    llvm_type(&elem_ty)?,
                    zero,
                    storage_ty,
                    elem_ptr
                ));
            }
            return Ok(Value {
                ty: elem_ty,
                ir: tmp,
            });
        }
        if name == "$for_in_ref" {
            self.require_not_invariant(
                type_args.len() != 1,
                "$for_in_ref MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 2, "invalid MIR arity for $for_in_ref")?;
            let elem_ty = type_args[0].clone();
            let elem_ptr = self.emit_for_in_elem_ptr(&args[0], &elem_ty, &args[1])?;
            return Ok(Value {
                ty: Type::Ref(Box::new(elem_ty)),
                ir: elem_ptr,
            });
        }
        if name == "$for_in_mutref" {
            self.require_not_invariant(
                type_args.len() != 1,
                "$for_in_mutref MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 2, "invalid MIR arity for $for_in_mutref")?;
            self.require_not_invariant(
                !matches!(args[0].ty, Type::MutRef(_)),
                "invalid MIR receiver kind for $for_in_mutref",
            )?;
            let elem_ty = type_args[0].clone();
            let elem_ptr = self.emit_for_in_elem_ptr(&args[0], &elem_ty, &args[1])?;
            return Ok(Value {
                ty: Type::MutRef(Box::new(elem_ty)),
                ir: elem_ptr,
            });
        }
        if name == "$for_in_map_key" {
            self.require_not_invariant(
                type_args.len() != 1,
                "$for_in_map_key MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 2, "invalid MIR arity for $for_in_map_key")?;
            let key_ty = type_args[0].clone();
            let key_spec = self
                .types
                .map_key_spec(&key_ty)
                .ok_or_else(|| Self::invariant_violation("missing runtime map key spec"))?;
            let runtime_key_ty = key_spec.runtime_ty.clone();
            let map_obj = self.emit_map_obj_from_value(&args[0])?;
            let idx_ir = self.emit_index_i64(&args[1])?;
            let storage_ty = llvm_storage_type(&runtime_key_ty)?;
            let alloca = self.new_temp();
            self.emit(format!("{} = alloca {}", alloca, storage_ty));
            if runtime_key_ty != Type::Builtin(BuiltinType::Unit) {
                let zero = zero_value(&runtime_key_ty)?;
                self.emit(format!(
                    "store {} {}, {}* {}",
                    llvm_type(&runtime_key_ty)?,
                    zero,
                    storage_ty,
                    alloca
                ));
            }
            let out_ptr = self.new_temp();
            self.emit(format!(
                "{} = bitcast {}* {} to i8*",
                out_ptr, storage_ty, alloca
            ));
            let ok = self.new_temp();
            self.emit(format!(
                "{} = call i32 @__gost_map_slot_key(%map_obj* {}, i64 {}, i8* {})",
                ok, map_obj, idx_ir, out_ptr
            ));
            let failed = self.new_temp();
            self.emit(format!("{} = icmp eq i32 {}, 0", failed, ok));
            let panic_name = self.new_block("map_for_in_key_err");
            let cont_name = self.new_block("map_for_in_key_ok");
            let panic_idx = self.add_block(panic_name.clone());
            let cont_idx = self.add_block(cont_name.clone());
            self.terminate(format!(
                "br i1 {}, label %{}, label %{}",
                failed, panic_name, cont_name
            ));
            self.switch_to(panic_idx);
            self.emit("call void @__gost_panic(i8* null, i64 0)");
            self.terminate(format!("br label %{}", cont_name));
            self.switch_to(cont_idx);
            let loaded = self.new_temp();
            self.emit(format!(
                "{} = load {}, {}* {}",
                loaded,
                llvm_type(&runtime_key_ty)?,
                storage_ty,
                alloca
            ));
            let runtime_val = Value {
                ty: runtime_key_ty,
                ir: loaded,
            };
            return self.emit_map_key_from_runtime(&key_ty, runtime_val);
        }
        if name == "$for_in_map_val" {
            self.require_not_invariant(
                type_args.len() != 1,
                "$for_in_map_val MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 2, "invalid MIR arity for $for_in_map_val")?;
            let val_ty = type_args[0].clone();
            let map_obj = self.emit_map_obj_from_value(&args[0])?;
            let idx_ir = self.emit_index_i64(&args[1])?;
            let storage_ty = llvm_storage_type(&val_ty)?;
            let alloca = self.new_temp();
            self.emit(format!("{} = alloca {}", alloca, storage_ty));
            if val_ty != Type::Builtin(BuiltinType::Unit) {
                let zero = zero_value(&val_ty)?;
                self.emit(format!(
                    "store {} {}, {}* {}",
                    llvm_type(&val_ty)?,
                    zero,
                    storage_ty,
                    alloca
                ));
            }
            let out_ptr = self.new_temp();
            self.emit(format!(
                "{} = bitcast {}* {} to i8*",
                out_ptr, storage_ty, alloca
            ));
            let ok = self.new_temp();
            self.emit(format!(
                "{} = call i32 @__gost_map_slot_val(%map_obj* {}, i64 {}, i8* {})",
                ok, map_obj, idx_ir, out_ptr
            ));
            let failed = self.new_temp();
            self.emit(format!("{} = icmp eq i32 {}, 0", failed, ok));
            let panic_name = self.new_block("map_for_in_val_err");
            let cont_name = self.new_block("map_for_in_val_ok");
            let panic_idx = self.add_block(panic_name.clone());
            let cont_idx = self.add_block(cont_name.clone());
            self.terminate(format!(
                "br i1 {}, label %{}, label %{}",
                failed, panic_name, cont_name
            ));
            self.switch_to(panic_idx);
            self.emit("call void @__gost_panic(i8* null, i64 0)");
            self.terminate(format!("br label %{}", cont_name));
            self.switch_to(cont_idx);
            let loaded = self.new_temp();
            self.emit(format!(
                "{} = load {}, {}* {}",
                loaded,
                llvm_type(&val_ty)?,
                storage_ty,
                alloca
            ));
            let val = Value {
                ty: val_ty,
                ir: loaded,
            };
            self.emit_shared_inc_value(&val)?;
            return Ok(val);
        }
        if name == "$for_in_map_next_slot" {
            self.require_not_invariant(
                type_args.len() != 1,
                "$for_in_map_next_slot MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(
                args.len() != 2,
                "invalid MIR arity for $for_in_map_next_slot",
            )?;
            let map_obj = self.emit_map_obj_from_value(&args[0])?;
            let prev_slot = self.emit_index_i64(&args[1])?;
            let next = self.new_temp();
            self.emit(format!(
                "{} = call i64 @__gost_map_next_slot(%map_obj* {}, i64 {})",
                next, map_obj, prev_slot
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I64),
                ir: next,
            });
        }
        if name == "slice_len" {
            self.require_not_invariant(
                type_args.len() != 1,
                "slice_len MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 1, "slice_len expects one argument")?;
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
            self.require_not_invariant(
                type_args.len() != 1,
                "slice_get_copy MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 2, "slice_get_copy expects two arguments")?;
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
            let val = Value {
                ty: elem_ty.clone(),
                ir: tmp.clone(),
            };
            self.emit_shared_inc_value(&val)?;
            return Ok(Value {
                ty: elem_ty,
                ir: tmp,
            });
        }
        if name == "slice_set" {
            self.require_not_invariant(
                type_args.len() != 1,
                "slice_set MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 3, "slice_set expects three arguments")?;
            let elem_ty = type_args[0].clone();
            let slice_obj = self.emit_slice_obj_from_value(&args[0])?;
            let elem_ptr = self.emit_slice_elem_ptr(&slice_obj, &elem_ty, &args[1], true)?;
            let storage_ty = llvm_storage_type(&elem_ty)?;
            if matches!(elem_ty, Type::Alias(_) | Type::Shared(_) | Type::Chan(_)) {
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
        if name == "__gost_chan_can_send" {
            self.require_not_invariant(args.len() != 1, "__gost_chan_can_send expects 1 argument")?;
            let chan = &args[0];
            self.require_not_invariant(
                !matches!(chan.ty, Type::Chan(_)),
                "__gost_chan_can_send expects chan[T]",
            )?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i32 @__gost_chan_can_send(%chan* {})",
                tmp, chan.ir
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_chan_can_recv" {
            self.require_not_invariant(args.len() != 1, "__gost_chan_can_recv expects 1 argument")?;
            let chan = &args[0];
            self.require_not_invariant(
                !matches!(chan.ty, Type::Chan(_)),
                "__gost_chan_can_recv expects chan[T]",
            )?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i32 @__gost_chan_can_recv(%chan* {})",
                tmp, chan.ir
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_select_wait" {
            let tmp = self.new_temp();
            let mut chan_args: Vec<&Value> = Vec::new();
            let mut op_args: Vec<&Value> = Vec::new();
            for arg in args.iter() {
                if matches!(arg.ty, Type::Chan(_)) {
                    chan_args.push(arg);
                } else if arg.ty == Type::Builtin(BuiltinType::I32) {
                    op_args.push(arg);
                } else {
                    return self
                        .invariant_err("__gost_select_wait expects chan[T] and i32 op arguments");
                }
            }

            if chan_args.is_empty() {
                self.emit(format!(
                    "{} = call i32 @__gost_select_wait(%chan** null, i32* null, i32 0)",
                    tmp
                ));
            } else {
                self.require_not_invariant(
                    !op_args.is_empty() && op_args.len() != chan_args.len(),
                    "__gost_select_wait expects one op per channel",
                )?;
                let arr_len = chan_args.len();
                let arr_ty = format!("[{} x %chan*]", arr_len);
                let arr_ptr = self.new_temp();
                self.emit(format!("{} = alloca {}", arr_ptr, arr_ty));
                for (i, arg) in chan_args.iter().enumerate() {
                    let slot = self.new_temp();
                    self.emit(format!(
                        "{} = getelementptr {}, {}* {}, i32 0, i32 {}",
                        slot, arr_ty, arr_ty, arr_ptr, i
                    ));
                    self.emit(format!("store %chan* {}, %chan** {}", arg.ir, slot));
                }
                let cast_ptr = self.new_temp();
                self.emit(format!(
                    "{} = bitcast {}* {} to %chan**",
                    cast_ptr, arr_ty, arr_ptr
                ));
                let op_ptr = if op_args.is_empty() {
                    "null".to_string()
                } else {
                    let op_arr_ty = format!("[{} x i32]", arr_len);
                    let op_arr_ptr = self.new_temp();
                    self.emit(format!("{} = alloca {}", op_arr_ptr, op_arr_ty));
                    for (i, op) in op_args.iter().enumerate() {
                        let slot = self.new_temp();
                        self.emit(format!(
                            "{} = getelementptr {}, {}* {}, i32 0, i32 {}",
                            slot, op_arr_ty, op_arr_ty, op_arr_ptr, i
                        ));
                        self.emit(format!("store i32 {}, i32* {}", op.ir, slot));
                    }
                    let cast_op_ptr = self.new_temp();
                    self.emit(format!(
                        "{} = bitcast {}* {} to i32*",
                        cast_op_ptr, op_arr_ty, op_arr_ptr
                    ));
                    cast_op_ptr
                };
                self.emit(format!(
                    "{} = call i32 @__gost_select_wait(%chan** {}, i32* {}, i32 {})",
                    tmp, cast_ptr, op_ptr, arr_len
                ));
            }
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_error_new" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_error_new does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 1, "__gost_error_new expects 1 argument")?;
            let arg = &args[0];
            self.require_not_invariant(
                arg.ty != Type::Builtin(BuiltinType::String),
                "__gost_error_new expects string",
            )?;
            let ptr = self.new_temp();
            let len = self.new_temp();
            self.emit(format!("{} = extractvalue %string {}, 0", ptr, arg.ir));
            self.emit(format!("{} = extractvalue %string {}, 1", len, arg.ir));
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i32 @__gost_error_new(i8* {}, i64 {})",
                tmp, ptr, len
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::Error),
                ir: tmp,
            });
        }
        if name == "__gost_error_message" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_error_message does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 1, "__gost_error_message expects 1 argument")?;
            self.require_not_invariant(
                args[0].ty != Type::Builtin(BuiltinType::Error),
                "__gost_error_message expects error",
            )?;
            let out_ptr = self.new_temp();
            self.emit(format!("{} = alloca %string", out_ptr));
            self.emit(format!(
                "call void @__gost_error_message(%string* {}, i32 {})",
                out_ptr, args[0].ir
            ));
            let tmp = self.new_temp();
            self.emit(format!("{} = load %string, %string* {}", tmp, out_ptr));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::String),
                ir: tmp,
            });
        }
        if name == "__gost_singleton_acquire" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_singleton_acquire does not take type arguments",
            )?;
            self.require_not_invariant(
                args.len() != 1,
                "__gost_singleton_acquire expects 1 argument",
            )?;
            self.require_not_invariant(
                args[0].ty != Type::Builtin(BuiltinType::String),
                "__gost_singleton_acquire expects string name",
            )?;
            let (ptr, len) = self.emit_string_ptr_len_from_value(&args[0])?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i32 @__gost_singleton_acquire(i8* {}, i64 {})",
                tmp, ptr, len
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_now_ms" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_now_ms does not take type arguments",
            )?;
            self.require_not_invariant(!args.is_empty(), "__gost_now_ms expects 0 arguments")?;
            let tmp = self.new_temp();
            self.emit(format!("{} = call i64 @__gost_now_ms()", tmp));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I64),
                ir: tmp,
            });
        }
        if name == "__gost_process_exit" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_process_exit does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 1, "__gost_process_exit expects 1 argument")?;
            let code_i32 = match args[0].ty {
                Type::Builtin(BuiltinType::I32) => args[0].ir.clone(),
                Type::Builtin(BuiltinType::I64) => {
                    let tmp = self.new_temp();
                    self.emit(format!("{} = trunc i64 {} to i32", tmp, args[0].ir));
                    tmp
                }
                _ => return self.invariant_err("__gost_process_exit expects i32 or i64"),
            };
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i32 @__gost_process_exit(i32 {})",
                tmp, code_i32
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_os_last_status" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_os_last_status does not take type arguments",
            )?;
            self.require_not_invariant(
                !args.is_empty(),
                "__gost_os_last_status expects 0 arguments",
            )?;
            let tmp = self.new_temp();
            self.emit(format!("{} = call i32 @__gost_os_last_status()", tmp));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_os_last_error"
            || name == "__gost_os_last_output"
            || name == "__gost_os_getwd"
            || name == "__gost_os_args"
        {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_os_* does not take type arguments",
            )?;
            self.require_not_invariant(!args.is_empty(), "__gost_os_* expects 0 arguments")?;
            let out_ptr = self.new_temp();
            self.emit(format!("{} = alloca %string", out_ptr));
            self.emit(format!("call void @{}(%string* {})", name, out_ptr));
            let tmp = self.new_temp();
            self.emit(format!("{} = load %string, %string* {}", tmp, out_ptr));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::String),
                ir: tmp,
            });
        }
        if name == "__gost_os_exec" || name == "__gost_os_pipe" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_os_* does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 1, "__gost_os_* expects 1 argument")?;
            self.require_not_invariant(
                args[0].ty != Type::Builtin(BuiltinType::String),
                "__gost_os_* expects string argument",
            )?;
            let (cmd_ptr, cmd_len) = self.emit_string_ptr_len_from_value(&args[0])?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i32 @{}(i8* {}, i64 {})",
                tmp, name, cmd_ptr, cmd_len
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_os_read_file"
            || name == "__gost_os_readdir"
            || name == "__gost_os_getenv"
        {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_os_* does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 1, "__gost_os_* expects 1 argument")?;
            self.require_not_invariant(
                args[0].ty != Type::Builtin(BuiltinType::String),
                "__gost_os_* expects string argument",
            )?;
            let (path_ptr, path_len) = self.emit_string_ptr_len_from_value(&args[0])?;
            let out_ptr = self.new_temp();
            self.emit(format!("{} = alloca %string", out_ptr));
            self.emit(format!(
                "call void @{}(%string* {}, i8* {}, i64 {})",
                name, out_ptr, path_ptr, path_len
            ));
            let tmp = self.new_temp();
            self.emit(format!("{} = load %string, %string* {}", tmp, out_ptr));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::String),
                ir: tmp,
            });
        }
        if name == "__gost_os_write_file" || name == "__gost_os_setenv" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_os_* does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 2, "__gost_os_* expects 2 arguments")?;
            self.require_not_invariant(
                args[0].ty != Type::Builtin(BuiltinType::String)
                    || args[1].ty != Type::Builtin(BuiltinType::String),
                "__gost_os_* expects string arguments",
            )?;
            let (a_ptr, a_len) = self.emit_string_ptr_len_from_value(&args[0])?;
            let (b_ptr, b_len) = self.emit_string_ptr_len_from_value(&args[1])?;
            let tmp = self.new_temp();
            if name == "__gost_os_write_file" {
                self.emit(format!(
                    "{} = call i64 @__gost_os_write_file(i8* {}, i64 {}, i8* {}, i64 {})",
                    tmp, a_ptr, a_len, b_ptr, b_len
                ));
                return Ok(Value {
                    ty: Type::Builtin(BuiltinType::I64),
                    ir: tmp,
                });
            }
            self.emit(format!(
                "{} = call i32 @__gost_os_setenv(i8* {}, i64 {}, i8* {}, i64 {})",
                tmp, a_ptr, a_len, b_ptr, b_len
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_os_remove"
            || name == "__gost_os_mkdir"
            || name == "__gost_os_chdir"
            || name == "__gost_os_stat_size"
        {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_os_* does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 1, "__gost_os_* expects 1 argument")?;
            self.require_not_invariant(
                args[0].ty != Type::Builtin(BuiltinType::String),
                "__gost_os_* expects string argument",
            )?;
            let (path_ptr, path_len) = self.emit_string_ptr_len_from_value(&args[0])?;
            let tmp = self.new_temp();
            if name == "__gost_os_stat_size" {
                self.emit(format!(
                    "{} = call i64 @__gost_os_stat_size(i8* {}, i64 {})",
                    tmp, path_ptr, path_len
                ));
                return Ok(Value {
                    ty: Type::Builtin(BuiltinType::I64),
                    ir: tmp,
                });
            }
            self.emit(format!(
                "{} = call i32 @{}(i8* {}, i64 {})",
                tmp, name, path_ptr, path_len
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_sync_mutex_new"
            || name == "__gost_sync_waitgroup_new"
            || name == "__gost_sync_once_new"
        {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_sync_* does not take type arguments",
            )?;
            self.require_not_invariant(!args.is_empty(), "__gost_sync_* expects 0 arguments")?;
            let tmp = self.new_temp();
            self.emit(format!("{} = call i64 @{}()", tmp, name));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I64),
                ir: tmp,
            });
        }
        if name == "__gost_sync_mutex_lock"
            || name == "__gost_sync_mutex_try_lock"
            || name == "__gost_sync_mutex_unlock"
            || name == "__gost_sync_waitgroup_wait"
            || name == "__gost_sync_once_begin"
        {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_sync_* does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 1, "__gost_sync_* expects 1 argument")?;
            let handle = self.emit_index_i64(&args[0])?;
            let tmp = self.new_temp();
            self.emit(format!("{} = call i32 @{}(i64 {})", tmp, name, handle));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_sync_waitgroup_add" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_sync_waitgroup_add does not take type arguments",
            )?;
            self.require_not_invariant(
                args.len() != 2,
                "__gost_sync_waitgroup_add expects 2 arguments",
            )?;
            let handle = self.emit_index_i64(&args[0])?;
            let delta_i32 = match args[1].ty {
                Type::Builtin(BuiltinType::I32) => args[1].ir.clone(),
                Type::Builtin(BuiltinType::I64) => {
                    let tmp = self.new_temp();
                    self.emit(format!("{} = trunc i64 {} to i32", tmp, args[1].ir));
                    tmp
                }
                _ => return self.invariant_err("__gost_sync_waitgroup_add expects i32/i64 delta"),
            };
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i32 @__gost_sync_waitgroup_add(i64 {}, i32 {})",
                tmp, handle, delta_i32
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_net_last_status" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_net_last_status does not take type arguments",
            )?;
            self.require_not_invariant(
                !args.is_empty(),
                "__gost_net_last_status expects 0 arguments",
            )?;
            let tmp = self.new_temp();
            self.emit(format!("{} = call i32 @__gost_net_last_status()", tmp));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_net_last_http_status" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_net_last_http_status does not take type arguments",
            )?;
            self.require_not_invariant(
                !args.is_empty(),
                "__gost_net_last_http_status expects 0 arguments",
            )?;
            let tmp = self.new_temp();
            self.emit(format!("{} = call i32 @__gost_net_last_http_status()", tmp));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_net_last_error" || name == "__gost_net_last_peer" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_net_last_error does not take type arguments",
            )?;
            self.require_not_invariant(
                !args.is_empty(),
                "__gost_net_last_error expects 0 arguments",
            )?;
            let out_ptr = self.new_temp();
            self.emit(format!("{} = alloca %string", out_ptr));
            self.emit(format!("call void @{}(%string* {})", name, out_ptr));
            let tmp = self.new_temp();
            self.emit(format!("{} = load %string, %string* {}", tmp, out_ptr));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::String),
                ir: tmp,
            });
        }
        if name == "__gost_net_tcp_listen"
            || name == "__gost_net_tcp_connect"
            || name == "__gost_net_udp_bind"
            || name == "__gost_net_udp_connect"
            || name == "__gost_net_ws_connect"
        {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_net_* does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 1, "__gost_net_* expects 1 argument")?;
            self.require_not_invariant(
                args[0].ty != Type::Builtin(BuiltinType::String),
                "__gost_net_* expects string address",
            )?;
            let (ptr, len) = self.emit_string_ptr_len_from_value(&args[0])?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i64 @{}(i8* {}, i64 {})",
                tmp, name, ptr, len
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I64),
                ir: tmp,
            });
        }
        if name == "__gost_net_tcp_accept" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_net_tcp_accept does not take type arguments",
            )?;
            self.require_not_invariant(
                args.len() != 1,
                "__gost_net_tcp_accept expects 1 argument",
            )?;
            let handle = self.emit_index_i64(&args[0])?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i64 @__gost_net_tcp_accept(i64 {})",
                tmp, handle
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I64),
                ir: tmp,
            });
        }
        if name == "__gost_net_tcp_close"
            || name == "__gost_net_udp_close"
            || name == "__gost_net_ws_close"
        {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_net_*_close does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 1, "__gost_net_*_close expects 1 argument")?;
            let handle = self.emit_index_i64(&args[0])?;
            let tmp = self.new_temp();
            self.emit(format!("{} = call i32 @{}(i64 {})", tmp, name, handle));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_net_ws_send_text" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_net_ws_send_text does not take type arguments",
            )?;
            self.require_not_invariant(
                args.len() != 2,
                "__gost_net_ws_send_text expects 2 arguments",
            )?;
            self.require_not_invariant(
                args[1].ty != Type::Builtin(BuiltinType::String),
                "__gost_net_ws_send_text expects string payload",
            )?;
            let handle = self.emit_index_i64(&args[0])?;
            let (ptr, len) = self.emit_string_ptr_len_from_value(&args[1])?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i32 @__gost_net_ws_send_text(i64 {}, i8* {}, i64 {})",
                tmp, handle, ptr, len
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I32),
                ir: tmp,
            });
        }
        if name == "__gost_net_tcp_write" || name == "__gost_net_udp_send" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_net_* does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 2, "__gost_net_* expects 2 arguments")?;
            self.require_not_invariant(
                args[1].ty != Type::Builtin(BuiltinType::String),
                "__gost_net_* expects string payload",
            )?;
            let handle = self.emit_index_i64(&args[0])?;
            let (ptr, len) = self.emit_string_ptr_len_from_value(&args[1])?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i64 @{}(i64 {}, i8* {}, i64 {})",
                tmp, name, handle, ptr, len
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I64),
                ir: tmp,
            });
        }
        if name == "__gost_net_udp_send_to" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_net_udp_send_to does not take type arguments",
            )?;
            self.require_not_invariant(
                args.len() != 3,
                "__gost_net_udp_send_to expects 3 arguments",
            )?;
            self.require_not_invariant(
                args[1].ty != Type::Builtin(BuiltinType::String)
                    || args[2].ty != Type::Builtin(BuiltinType::String),
                "__gost_net_udp_send_to expects string address and payload",
            )?;
            let handle = self.emit_index_i64(&args[0])?;
            let (addr_ptr, addr_len) = self.emit_string_ptr_len_from_value(&args[1])?;
            let (data_ptr, data_len) = self.emit_string_ptr_len_from_value(&args[2])?;
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call i64 @__gost_net_udp_send_to(i64 {}, i8* {}, i64 {}, i8* {}, i64 {})",
                tmp, handle, addr_ptr, addr_len, data_ptr, data_len
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::I64),
                ir: tmp,
            });
        }
        if name == "__gost_net_tcp_read"
            || name == "__gost_net_udp_recv"
            || name == "__gost_net_udp_recv_from"
        {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_net_* does not take type arguments",
            )?;
            self.require_not_invariant(args.len() != 2, "__gost_net_* expects 2 arguments")?;
            let handle = self.emit_index_i64(&args[0])?;
            let max_i32 = match args[1].ty {
                Type::Builtin(BuiltinType::I32) => args[1].ir.clone(),
                Type::Builtin(BuiltinType::I64) => {
                    let t = self.new_temp();
                    self.emit(format!("{} = trunc i64 {} to i32", t, args[1].ir));
                    t
                }
                _ => return self.invariant_err("__gost_net_* max must be i32/i64"),
            };
            let out_ptr = self.new_temp();
            self.emit(format!("{} = alloca %string", out_ptr));
            self.emit(format!(
                "call void @{}(%string* {}, i64 {}, i32 {})",
                name, out_ptr, handle, max_i32
            ));
            let tmp = self.new_temp();
            self.emit(format!("{} = load %string, %string* {}", tmp, out_ptr));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::String),
                ir: tmp,
            });
        }
        if name == "__gost_net_ws_recv_text" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_net_ws_recv_text does not take type arguments",
            )?;
            self.require_not_invariant(
                args.len() != 1,
                "__gost_net_ws_recv_text expects 1 argument",
            )?;
            let handle = self.emit_index_i64(&args[0])?;
            let out_ptr = self.new_temp();
            self.emit(format!("{} = alloca %string", out_ptr));
            self.emit(format!(
                "call void @__gost_net_ws_recv_text(%string* {}, i64 {})",
                out_ptr, handle
            ));
            let tmp = self.new_temp();
            self.emit(format!("{} = load %string, %string* {}", tmp, out_ptr));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::String),
                ir: tmp,
            });
        }
        if name == "__gost_net_http_request" || name == "__gost_net_http_request_headers" {
            self.require_not_invariant(
                !type_args.is_empty(),
                "__gost_net_http_request does not take type arguments",
            )?;
            let expected = if name == "__gost_net_http_request_headers" {
                5
            } else {
                4
            };
            let expected_msg = if expected == 5 {
                "__gost_net_http_request_headers expects 5 arguments"
            } else {
                "__gost_net_http_request expects 4 arguments"
            };
            self.require_not_invariant(args.len() != expected, expected_msg)?;
            for arg in args.iter() {
                self.require_not_invariant(
                    arg.ty != Type::Builtin(BuiltinType::String),
                    "__gost_net_http_request expects string arguments",
                )?;
            }
            let (m_ptr, m_len) = self.emit_string_ptr_len_from_value(&args[0])?;
            let (u_ptr, u_len) = self.emit_string_ptr_len_from_value(&args[1])?;
            let (b_ptr, b_len) = self.emit_string_ptr_len_from_value(&args[2])?;
            let (ct_ptr, ct_len) = self.emit_string_ptr_len_from_value(&args[3])?;
            let out_ptr = self.new_temp();
            self.emit(format!("{} = alloca %string", out_ptr));
            if name == "__gost_net_http_request_headers" {
                let (h_ptr, h_len) = self.emit_string_ptr_len_from_value(&args[4])?;
                self.emit(format!(
                    "call void @__gost_net_http_request_headers(%string* {}, i8* {}, i64 {}, i8* {}, i64 {}, i8* {}, i64 {}, i8* {}, i64 {}, i8* {}, i64 {})",
                    out_ptr, m_ptr, m_len, u_ptr, u_len, b_ptr, b_len, ct_ptr, ct_len, h_ptr, h_len
                ));
            } else {
                self.emit(format!(
                    "call void @__gost_net_http_request(%string* {}, i8* {}, i64 {}, i8* {}, i64 {}, i8* {}, i64 {}, i8* {}, i64 {})",
                    out_ptr, m_ptr, m_len, u_ptr, u_len, b_ptr, b_len, ct_ptr, ct_len
                ));
            }
            let tmp = self.new_temp();
            self.emit(format!("{} = load %string, %string* {}", tmp, out_ptr));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::String),
                ir: tmp,
            });
        }
        if name == "slice_ref" || name == "slice_mutref" {
            self.require_not_invariant(
                type_args.len() != 1,
                "slice_ref MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 2, "slice_ref expects two arguments")?;
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
            self.require_not_invariant(
                type_args.len() != 1,
                "slice_push MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 2, "slice_push expects two arguments")?;
            let elem_ty = type_args[0].clone();
            let slice_obj = self.emit_slice_obj_from_value(&args[0])?;
            let storage_ty = llvm_storage_type(&elem_ty)?;
            if matches!(elem_ty, Type::Alias(_) | Type::Shared(_) | Type::Chan(_)) {
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
            self.require_not_invariant(
                type_args.len() != 1,
                "slice_pop MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 1, "slice_pop expects one argument")?;
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
            return Ok(Value {
                ty: tuple_ty,
                ir: tmp1,
            });
        }
        if name == "shared_new" || name == "own_new" {
            let type_err = if name == "own_new" {
                "own_new MIR call must carry exactly one type argument"
            } else {
                "shared_new MIR call must carry exactly one type argument"
            };
            let arg_err = if name == "own_new" {
                "invalid MIR arity for own_new"
            } else {
                "invalid MIR arity for shared_new"
            };
            self.require_not_invariant(type_args.len() != 1, type_err)?;
            self.require_not_invariant(args.len() != 1, arg_err)?;
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
                ty: if name == "own_new" {
                    Type::Own(Box::new(elem_ty))
                } else {
                    Type::Shared(Box::new(elem_ty))
                },
                ir: shared_val,
            });
        }
        if name == "shared_get"
            || name == "shared_get_mut"
            || name == "own_borrow"
            || name == "own_borrow_mut"
            || name == "alias_borrow"
        {
            let is_mut_borrow = name == "shared_get_mut" || name == "own_borrow_mut";
            self.require_not_invariant(
                type_args.len() != 1,
                "handle intrinsic MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 1, "invalid MIR arity for handle intrinsic")?;
            let elem_ty = type_args[0].clone();
            let shared_obj = self.emit_shared_obj_from_value(&args[0])?;
            if is_mut_borrow {
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
            let ref_ty = if is_mut_borrow {
                Type::MutRef(Box::new(elem_ty))
            } else {
                Type::Ref(Box::new(elem_ty))
            };
            return Ok(Value {
                ty: ref_ty,
                ir: cast_ptr,
            });
        }
        if name == "freeze" {
            self.require_not_invariant(
                type_args.len() != 1,
                "freeze MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 1, "invalid MIR arity for freeze")?;
            let elem_ty = type_args[0].clone();
            self.require_not_invariant(
                args[0].ty != Type::Own(Box::new(elem_ty.clone())),
                "invalid MIR operand type for freeze",
            )?;
            return Ok(Value {
                ty: Type::Alias(Box::new(elem_ty)),
                ir: args[0].ir.clone(),
            });
        }
        if name == "own_into_value" {
            self.require_not_invariant(
                type_args.len() != 1,
                "handle intrinsic MIR call must carry exactly one type argument",
            )?;
            self.require_not_invariant(args.len() != 1, "invalid MIR arity for own_into_value")?;
            let elem_ty = type_args[0].clone();
            self.require_not_invariant(
                args[0].ty != Type::Own(Box::new(elem_ty.clone())),
                "invalid MIR operand type for own_into_value",
            )?;
            let shared_obj = self.emit_shared_obj_from_value(&args[0])?;
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
            let size_val = self.emit_size_of(&elem_ty)?;
            let status = self.new_temp();
            self.emit(format!(
                "{} = call i32 @__gost_shared_take_unique(%shared_obj* {}, i8* {}, i64 {})",
                status, shared_obj, cast_ptr, size_val.ir
            ));
            let ok = self.new_temp();
            self.emit(format!("{} = icmp eq i32 {}, 0", ok, status));
            let ok_name = self.new_block("shared_take_ok");
            let err_name = self.new_block("shared_take_err");
            let cont_name = self.new_block("shared_take_cont");
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
            if elem_ty == Type::Builtin(BuiltinType::Unit) {
                return Ok(Value {
                    ty: elem_ty,
                    ir: String::new(),
                });
            }
            let out = self.new_temp();
            self.emit(format!(
                "{} = load {}, {}* {}",
                out,
                llvm_type(&elem_ty)?,
                storage_ty,
                alloca
            ));
            return Ok(Value {
                ty: elem_ty,
                ir: out,
            });
        }
        if name == "make_map" {
            self.require_invariant(
                type_args.len() == 2,
                "make_map MIR call must carry exactly two type arguments",
            )?;
            self.require_invariant(args.len() == 1, "make_map expects one argument")?;
            let key_ty = type_args[0].clone();
            let val_ty = type_args[1].clone();
            let cap_ir = self.emit_index_i64(&args[0])?;
            let key_spec = self
                .types
                .map_key_spec(&key_ty)
                .ok_or_else(|| Self::invariant_violation("missing runtime map key spec"))?;
            let key_kind = key_spec.kind;
            let runtime_key_ty = key_spec.runtime_ty.clone();
            let key_size = self.emit_size_of(&runtime_key_ty)?;
            let val_size = self.emit_size_of(&val_ty)?;
            let (eq_fn, hash_fn, clone_fn, drop_fn) = if key_spec.needs_callbacks {
                let ops = self.ensure_map_key_ops(&runtime_key_ty)?;
                (
                    format!("@{}", ops.eq_fn),
                    format!("@{}", ops.hash_fn),
                    format!("@{}", ops.clone_fn),
                    format!("@{}", ops.drop_fn),
                )
            } else {
                (
                    "null".to_string(),
                    "null".to_string(),
                    "null".to_string(),
                    "null".to_string(),
                )
            };
            let obj = self.new_temp();
            self.emit(format!(
                "{} = call %map_obj* @__gost_map_new(i32 {}, i64 {}, i64 {}, i64 {}, i32 (i8*, i8*)* {}, i64 (i8*)* {}, void (i8*, i8*)* {}, void (i8*)* {})",
                obj, key_kind, key_size.ir, val_size.ir, cap_ir, eq_fn, hash_fn, clone_fn, drop_fn
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
            self.require_invariant(
                type_args.len() == 2,
                "map_len MIR call must carry exactly two type arguments",
            )?;
            self.require_invariant(args.len() == 1, "map_len expects one argument")?;
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
            self.require_invariant(
                type_args.len() == 2,
                "map_get MIR call must carry exactly two type arguments",
            )?;
            self.require_invariant(args.len() == 2, "map_get expects two arguments")?;
            let key_ty = type_args[0].clone();
            let val_ty = type_args[1].clone();
            let map_obj = self.emit_map_obj_from_value(&args[0])?;
            let key_ptr = self.emit_map_key_ptr(&key_ty, args[1].clone())?;
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
            let val = Value {
                ty: val_ty.clone(),
                ir: val_tmp.clone(),
            };
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
            return Ok(Value {
                ty: tuple_ty,
                ir: tmp1,
            });
        }
        if name == "map_set" {
            self.require_invariant(
                type_args.len() == 2,
                "map_set MIR call must carry exactly two type arguments",
            )?;
            self.require_invariant(args.len() == 3, "map_set expects three arguments")?;
            let key_ty = type_args[0].clone();
            let val_ty = type_args[1].clone();
            let map_obj = self.emit_map_obj_from_value(&args[0])?;
            let key_ptr = self.emit_map_key_ptr(&key_ty, args[1].clone())?;
            let val_alloca = self.new_temp();
            let val_storage = llvm_storage_type(&val_ty)?;
            self.emit(format!("{} = alloca {}", val_alloca, val_storage));
            if matches!(val_ty, Type::Alias(_) | Type::Shared(_) | Type::Chan(_)) {
                self.emit_shared_inc_value(&args[2])?;
            }
            self.emit(format!(
                "store {} {}, {}* {}",
                llvm_type(&val_ty)?,
                args[2].ir,
                val_storage,
                val_alloca
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
            self.require_invariant(
                type_args.len() == 2,
                "map_del MIR call must carry exactly two type arguments",
            )?;
            self.require_invariant(args.len() == 2, "map_del expects two arguments")?;
            let key_ty = type_args[0].clone();
            let map_obj = self.emit_map_obj_from_value(&args[0])?;
            let key_ptr = self.emit_map_key_ptr(&key_ty, args[1].clone())?;
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
        let fn_sig = self
            .fn_sigs
            .get(name)
            .cloned()
            .ok_or_else(|| Self::invariant_violation("unknown function signature"))?;
        self.require_invariant(
            type_args.is_empty(),
            "non-generic MIR call carried unexpected type arguments",
        )?;
        self.require_invariant(
            (!fn_sig.is_variadic && args.len() == fn_sig.params.len())
                || (fn_sig.is_variadic && args.len() >= fn_sig.params.len()),
            "argument count mismatch",
        )?;
        let mut args_ir = Vec::new();
        for (idx, val) in args.iter().enumerate() {
            if let Some(ty) = fn_sig.params.get(idx) {
                if matches!(ty, Type::Alias(_) | Type::Shared(_) | Type::Chan(_)) {
                    self.emit_shared_inc_value(val)?;
                }
                args_ir.push(format!("{} {}", llvm_type(ty)?, val.ir));
            } else {
                if matches!(val.ty, Type::Alias(_) | Type::Shared(_) | Type::Chan(_)) {
                    self.emit_shared_inc_value(val)?;
                }
                args_ir.push(format!("{} {}", llvm_type(&val.ty)?, val.ir));
            }
        }
        let ret_ty = llvm_type(&fn_sig.ret)?;
        let cc = llvm_call_conv(fn_sig.extern_abi.as_deref());
        let callee_symbol = self.llvm_function_symbol(name);
        if fn_sig.ret == Type::Builtin(BuiltinType::Unit) {
            self.emit(format!(
                "call {}{} @{}({})",
                cc,
                ret_ty,
                callee_symbol,
                args_ir.join(", ")
            ));
            Ok(Value {
                ty: Type::Builtin(BuiltinType::Unit),
                ir: String::new(),
            })
        } else {
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call {}{} @{}({})",
                tmp,
                cc,
                ret_ty,
                callee_symbol,
                args_ir.join(", ")
            ));
            Ok(Value {
                ty: fn_sig.ret,
                ir: tmp,
            })
        }
    }

    fn emit_call_ptr(&mut self, callee: Value, args: Vec<Value>) -> Result<Value, String> {
        let (params, ret, is_variadic) = match &callee.ty {
            Type::FnPtr {
                params,
                ret,
                is_variadic,
            } => (params.clone(), *ret.clone(), *is_variadic),
            _ => return self.invariant_err("call target must be function pointer"),
        };
        if (!is_variadic && args.len() != params.len())
            || (is_variadic && args.len() < params.len())
        {
            return self.invariant_err("argument count mismatch");
        }
        let mut args_ir = Vec::new();
        for (idx, val) in args.iter().enumerate() {
            if let Some(pty) = params.get(idx) {
                if matches!(pty, Type::Alias(_) | Type::Shared(_) | Type::Chan(_)) {
                    self.emit_shared_inc_value(val)?;
                }
                args_ir.push(format!("{} {}", llvm_type(pty)?, val.ir));
            } else {
                if matches!(val.ty, Type::Alias(_) | Type::Shared(_) | Type::Chan(_)) {
                    self.emit_shared_inc_value(val)?;
                }
                args_ir.push(format!("{} {}", llvm_type(&val.ty)?, val.ir));
            }
        }
        let ret_ty = llvm_type(&ret)?;
        if ret == Type::Builtin(BuiltinType::Unit) {
            self.emit(format!(
                "call {} {}({})",
                ret_ty,
                callee.ir,
                args_ir.join(", ")
            ));
            Ok(Value {
                ty: Type::Builtin(BuiltinType::Unit),
                ir: String::new(),
            })
        } else {
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call {} {}({})",
                tmp,
                ret_ty,
                callee.ir,
                args_ir.join(", ")
            ));
            Ok(Value { ty: ret, ir: tmp })
        }
    }

    fn emit_inline_asm_call(
        &mut self,
        callee_name: &str,
        type_args: &[TypeAst],
        args: &[Expr],
    ) -> Result<Value, String> {
        self.require_not_invariant(
            type_args.len() > 1,
            "asm MIR call carried invalid type argument count",
        )?;
        self.require_not_invariant(args.is_empty(), "asm expects at least 1 argument")?;
        let template = match &args[0].kind {
            ExprKind::String(s) => s.clone(),
            _ => return self.invariant_err("asm template must be string literal"),
        };
        let constraints = if args.len() >= 2 {
            match &args[1].kind {
                ExprKind::String(s) => s.clone(),
                _ => return self.invariant_err("asm constraints must be string literal"),
            }
        } else {
            "=r".to_string()
        };
        let mut operand_values = Vec::new();
        if args.len() >= 3 {
            for arg in args.iter().skip(2) {
                let val = self.emit_expr(arg)?;
                self.require_not_invariant(
                    val.ty == Type::Builtin(BuiltinType::Unit),
                    "asm operands cannot be unit",
                )?;
                operand_values.push(val);
            }
        }
        let spec = parse_asm_constraint_spec(&constraints);
        let explicit_count = spec.outputs.len() + spec.input_count;
        let legacy_count = spec.readwrite_outputs + spec.input_count;
        let explicit_style = operand_values.len() == explicit_count;
        let legacy_style = !explicit_style && operand_values.len() == legacy_count;
        self.require_not_invariant_fmt(!explicit_style && !legacy_style, format!(
                "asm operand count mismatch (expected {} with explicit outputs or {} in legacy style, found {})",
                explicit_count,
                legacy_count,
                operand_values.len()
            ))?;
        let ret_ty = if let Some(ret) = type_args.first() {
            self.resolve_type_ast(ret)?
        } else if spec.outputs.is_empty() {
            Type::Builtin(BuiltinType::I64)
        } else if spec.outputs.len() == 1 {
            if explicit_style {
                operand_values
                    .first()
                    .map(|v| v.ty.clone())
                    .unwrap_or(Type::Builtin(BuiltinType::I64))
            } else if spec.readwrite_outputs > 0 {
                operand_values
                    .first()
                    .map(|v| v.ty.clone())
                    .unwrap_or(Type::Builtin(BuiltinType::I64))
            } else {
                Type::Builtin(BuiltinType::I64)
            }
        } else if explicit_style {
            Type::Tuple(
                operand_values
                    .iter()
                    .take(spec.outputs.len())
                    .map(|v| v.ty.clone())
                    .collect(),
            )
        } else if spec.readwrite_outputs == spec.outputs.len()
            && operand_values.len() >= spec.outputs.len()
        {
            Type::Tuple(
                operand_values
                    .iter()
                    .take(spec.outputs.len())
                    .map(|v| v.ty.clone())
                    .collect(),
            )
        } else {
            self.require_not_invariant(
                true,
                "multiple asm outputs require explicit output operands (colon syntax)",
            )?;
            unreachable!();
        };
        let mut arg_ir = Vec::new();
        if explicit_style {
            for (idx, kind) in spec.outputs.iter().enumerate() {
                if matches!(kind, AsmOutputKind::ReadWrite) {
                    let val = operand_values.get(idx).ok_or_else(|| {
                        Self::invariant_violation("missing asm readwrite output operand")
                    })?;
                    arg_ir.push(format!("{} {}", llvm_type(&val.ty)?, val.ir));
                }
            }
            for val in operand_values.iter().skip(spec.outputs.len()) {
                arg_ir.push(format!("{} {}", llvm_type(&val.ty)?, val.ir));
            }
        } else {
            for val in operand_values.iter().take(spec.readwrite_outputs) {
                arg_ir.push(format!("{} {}", llvm_type(&val.ty)?, val.ir));
            }
            for val in operand_values.iter().skip(spec.readwrite_outputs) {
                arg_ir.push(format!("{} {}", llvm_type(&val.ty)?, val.ir));
            }
        }
        let escaped = escape_llvm_inline_asm(&template);
        let escaped_constraints = escape_llvm_inline_asm(&constraints);
        let sideeffect = if callee_name == "asm_pure" {
            ""
        } else {
            " sideeffect"
        };
        if ret_ty == Type::Builtin(BuiltinType::Unit) {
            self.emit(format!(
                "call void asm{} \"{}\", \"{}\"({})",
                sideeffect,
                escaped,
                escaped_constraints,
                arg_ir.join(", ")
            ));
            Ok(Value {
                ty: Type::Builtin(BuiltinType::Unit),
                ir: String::new(),
            })
        } else {
            let tmp = self.new_temp();
            self.emit(format!(
                "{} = call {} asm{} \"{}\", \"{}\"({})",
                tmp,
                llvm_type(&ret_ty)?,
                sideeffect,
                escaped,
                escaped_constraints,
                arg_ir.join(", ")
            ));
            Ok(Value {
                ty: ret_ty,
                ir: tmp,
            })
        }
    }

    fn emit_inline_asm_label(&mut self, args: &[Expr]) -> Result<Value, String> {
        self.require_not_invariant(args.len() != 1, "asm_label expects exactly 1 argument")?;
        let label = match &args[0].kind {
            ExprKind::String(s) => s.trim(),
            _ => return self.invariant_err("asm_label argument must be a string literal"),
        };
        self.require_not_invariant(label.is_empty(), "asm_label name cannot be empty")?;
        let label_idx = self.ensure_asm_label_block(label);
        let label_block = self
            .blocks
            .get(label_idx)
            .map(|b| b.name.clone())
            .ok_or_else(|| Self::invariant_violation("invalid asm label block"))?;
        if !self.current_block_terminated() {
            self.terminate(format!("br label %{}", label_block));
        }
        self.switch_to(label_idx);
        Ok(Value {
            ty: Type::Builtin(BuiltinType::Unit),
            ir: String::new(),
        })
    }

    fn emit_inline_asm_goto_call(
        &mut self,
        type_args: &[TypeAst],
        args: &[Expr],
    ) -> Result<Value, String> {
        self.require_not_invariant(
            args.len() < 3,
            "asm_goto expects at least 3 arguments (template, constraints, labels)",
        )?;
        let template = match &args[0].kind {
            ExprKind::String(s) => s.clone(),
            _ => return self.invariant_err("asm_goto template must be string literal"),
        };
        let constraints_raw = match &args[1].kind {
            ExprKind::String(s) => s.clone(),
            _ => return self.invariant_err("asm_goto constraints must be string literal"),
        };
        let labels_raw = match &args[2].kind {
            ExprKind::String(s) => s.clone(),
            _ => return self.invariant_err("asm_goto labels must be string literal"),
        };
        let labels: Vec<String> = labels_raw
            .split(',')
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string())
            .collect();
        self.require_not_invariant(labels.is_empty(), "asm_goto requires at least one label")?;

        let mut operand_values = Vec::new();
        for arg in args.iter().skip(3) {
            let val = self.emit_expr(arg)?;
            self.require_not_invariant(
                val.ty == Type::Builtin(BuiltinType::Unit),
                "asm_goto operands cannot be unit",
            )?;
            operand_values.push(val);
        }
        let spec = parse_asm_constraint_spec(&constraints_raw);
        let explicit_count = spec.outputs.len() + spec.input_count;
        let legacy_count = spec.readwrite_outputs + spec.input_count;
        let explicit_style = operand_values.len() == explicit_count;
        let legacy_style = !explicit_style && operand_values.len() == legacy_count;
        self.require_not_invariant_fmt(!explicit_style && !legacy_style, format!(
                "asm_goto operand count mismatch (expected {} with explicit outputs or {} in legacy style, found {})",
                explicit_count,
                legacy_count,
                operand_values.len()
            ))?;
        let ret_ty = if let Some(ret) = type_args.first() {
            self.resolve_type_ast(ret)?
        } else if spec.outputs.is_empty() {
            Type::Builtin(BuiltinType::Unit)
        } else if spec.outputs.len() == 1 {
            if explicit_style {
                operand_values
                    .first()
                    .map(|v| v.ty.clone())
                    .unwrap_or(Type::Builtin(BuiltinType::I64))
            } else if spec.readwrite_outputs > 0 {
                operand_values
                    .first()
                    .map(|v| v.ty.clone())
                    .unwrap_or(Type::Builtin(BuiltinType::I64))
            } else {
                Type::Builtin(BuiltinType::I64)
            }
        } else if explicit_style {
            Type::Tuple(
                operand_values
                    .iter()
                    .take(spec.outputs.len())
                    .map(|v| v.ty.clone())
                    .collect(),
            )
        } else if spec.readwrite_outputs == spec.outputs.len()
            && operand_values.len() >= spec.outputs.len()
        {
            Type::Tuple(
                operand_values
                    .iter()
                    .take(spec.outputs.len())
                    .map(|v| v.ty.clone())
                    .collect(),
            )
        } else {
            self.require_not_invariant(
                true,
                "multiple asm_goto outputs require explicit output operands (colon syntax)",
            )?;
            unreachable!();
        };
        let mut args_ir = Vec::new();
        if explicit_style {
            for (idx, kind) in spec.outputs.iter().enumerate() {
                if matches!(kind, AsmOutputKind::ReadWrite) {
                    let val = operand_values.get(idx).ok_or_else(|| {
                        Self::invariant_violation("missing asm_goto readwrite output operand")
                    })?;
                    args_ir.push(format!("{} {}", llvm_type(&val.ty)?, val.ir));
                }
            }
            for val in operand_values.iter().skip(spec.outputs.len()) {
                args_ir.push(format!("{} {}", llvm_type(&val.ty)?, val.ir));
            }
        } else {
            for val in operand_values.iter().take(spec.readwrite_outputs) {
                args_ir.push(format!("{} {}", llvm_type(&val.ty)?, val.ir));
            }
            for val in operand_values.iter().skip(spec.readwrite_outputs) {
                args_ir.push(format!("{} {}", llvm_type(&val.ty)?, val.ir));
            }
        }

        let raw_constraints: Vec<String> = constraints_raw
            .split(',')
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string())
            .collect();
        let mut non_clobber_constraints = Vec::new();
        let mut clobber_constraints = Vec::new();
        let mut existing_label_constraints = 0usize;
        for c in raw_constraints {
            if c == "!i" {
                existing_label_constraints += 1;
            } else if c.starts_with("~{") {
                clobber_constraints.push(c);
            } else {
                non_clobber_constraints.push(c);
            }
        }
        let label_needed = labels.len().max(existing_label_constraints);
        for _ in 0..label_needed {
            non_clobber_constraints.push("!i".to_string());
        }
        let mut final_constraints = non_clobber_constraints;
        final_constraints.extend(clobber_constraints);
        let constraints = final_constraints.join(",");

        let mut label_block_names = Vec::new();
        for label in &labels {
            let idx = self.ensure_asm_label_block(label);
            let block_name = self
                .blocks
                .get(idx)
                .map(|b| b.name.clone())
                .ok_or_else(|| Self::invariant_violation("invalid asm goto label block"))?;
            label_block_names.push(block_name);
        }
        let cont_name = self.new_block("asmgoto_cont");
        let cont_idx = self.add_block(cont_name.clone());

        let escaped_tmpl = escape_llvm_inline_asm(&template);
        let escaped_constraints = escape_llvm_inline_asm(&constraints);
        let mut dests = Vec::new();
        for name in &label_block_names {
            dests.push(format!("label %{}", name));
        }
        let ret_ir = if ret_ty == Type::Builtin(BuiltinType::Unit) {
            self.terminate(format!(
                "callbr void asm sideeffect \"{}\", \"{}\"({}) to label %{} [{}]",
                escaped_tmpl,
                escaped_constraints,
                args_ir.join(", "),
                cont_name,
                dests.join(", ")
            ));
            String::new()
        } else {
            let tmp = self.new_temp();
            self.terminate(format!(
                "{} = callbr {} asm sideeffect \"{}\", \"{}\"({}) to label %{} [{}]",
                tmp,
                llvm_type(&ret_ty)?,
                escaped_tmpl,
                escaped_constraints,
                args_ir.join(", "),
                cont_name,
                dests.join(", ")
            ));
            tmp
        };
        self.switch_to(cont_idx);
        if ret_ty == Type::Builtin(BuiltinType::Unit) {
            Ok(Value {
                ty: ret_ty,
                ir: String::new(),
            })
        } else {
            Ok(Value {
                ty: ret_ty,
                ir: ret_ir,
            })
        }
    }

    fn ensure_asm_label_block(&mut self, label: &str) -> usize {
        if let Some(idx) = self.asm_labels.get(label) {
            return *idx;
        }
        let mut safe = String::new();
        for ch in label.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                safe.push(ch);
            } else {
                safe.push('_');
            }
        }
        if safe.is_empty() {
            safe.push('L');
        }
        let name = self.new_block(&format!("asm_label_{}_", safe));
        let idx = self.add_block(name);
        self.asm_labels.insert(label.to_string(), idx);
        idx
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
            if matches!(arg.ty, Type::Alias(_) | Type::Shared(_) | Type::Chan(_)) {
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
            "call void @__gost_go_spawn(void (i8*)* @{}, i8* {})",
            thunk_name, ctx_raw
        ));
        Ok(())
    }

    fn emit_for_in_stmt(&mut self, name: &str, iter: &Expr, body: &Block) -> Result<(), String> {
        let iter_val = self.emit_expr(iter)?;
        let (elem_ty, loop_var_ty) = match &iter_val.ty {
            Type::Slice(inner) => {
                let elem = *inner.clone();
                self.require_not_invariant(
                    !matches!(self.types.classify(&elem), Some(TypeClass::Copy)),
                    "for-in by value requires Copy element; use &xs or &mut xs",
                )?;
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
                _ => return self.invariant_err("invalid MIR iterable container type"),
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
                _ => return self.invariant_err("invalid MIR iterable container type"),
            },
            _ => return self.invariant_err("invalid MIR iterable container type"),
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
        let _ = self.emit_block(body)?;
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
        let mut arm_ops: Vec<Option<i32>> = Vec::new();
        for arm in arms {
            match &arm.kind {
                crate::frontend::ast::SelectArmKind::Send { chan, .. } => {
                    let chan_val = self.emit_expr(chan)?;
                    let elem_ty = match &chan_val.ty {
                        Type::Chan(inner) => *inner.clone(),
                        _ => return self.invariant_err("select send expects chan[T]"),
                    };
                    arm_channels.push(Some(chan_val));
                    arm_elem_tys.push(Some(elem_ty));
                    arm_ops.push(Some(1));
                }
                crate::frontend::ast::SelectArmKind::Recv { chan, .. } => {
                    let chan_val = self.emit_expr(chan)?;
                    let elem_ty = match &chan_val.ty {
                        Type::Chan(inner) => *inner.clone(),
                        _ => return self.invariant_err("select recv expects chan[T]"),
                    };
                    arm_channels.push(Some(chan_val));
                    arm_elem_tys.push(Some(elem_ty));
                    arm_ops.push(Some(0));
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
                        return self.invariant_err("after expects i32 milliseconds");
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
                    arm_ops.push(Some(0));
                }
                crate::frontend::ast::SelectArmKind::Default => {
                    arm_channels.push(None);
                    arm_elem_tys.push(None);
                    arm_ops.push(None);
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
        let wait_idx = wait_name.as_ref().map(|name| self.add_block(name.clone()));
        if non_default.is_empty()
            && let Some(default_idx) = default_index
        {
            let default_name = default_name
                .as_ref()
                .ok_or_else(|| Self::invariant_violation("select default missing block"))?;
            let default_block = default_block
                .ok_or_else(|| Self::invariant_violation("select default missing block"))?;
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
                        .ok_or_else(|| Self::invariant_violation("select send missing channel"))?;
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
                        .ok_or_else(|| Self::invariant_violation("select recv missing channel"))?;
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
                        .ok_or_else(|| Self::invariant_violation("select default missing block"))?
                ));
            } else {
                self.terminate(format!(
                    "br i1 {}, label %{}, label %{}",
                    ready,
                    arm_name,
                    wait_name
                        .as_ref()
                        .ok_or_else(|| Self::invariant_violation("select wait missing block"))?
                ));
            }
            self.switch_to(arm_block);
            self.enter_scope();
            match &arm.kind {
                crate::frontend::ast::SelectArmKind::Send { value, .. } => {
                    let chan_val = arm_channels[*arm_idx]
                        .as_ref()
                        .ok_or_else(|| Self::invariant_violation("select send missing channel"))?;
                    let elem_ty = arm_elem_tys[*arm_idx].as_ref().ok_or_else(|| {
                        Self::invariant_violation("select send missing element type")
                    })?;
                    self.emit_send_with_value(chan_val, elem_ty, value)?;
                }
                crate::frontend::ast::SelectArmKind::Recv { .. }
                | crate::frontend::ast::SelectArmKind::After { .. } => {
                    let chan_val = arm_channels[*arm_idx]
                        .as_ref()
                        .ok_or_else(|| Self::invariant_violation("select recv missing channel"))?;
                    let elem_ty = arm_elem_tys[*arm_idx].as_ref().ok_or_else(|| {
                        Self::invariant_violation("select recv missing element type")
                    })?;
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
                        self.locals
                            .insert(name.clone(), (elem_ty.clone(), val_alloca));
                        if let Some(scope) = self.scopes.last_mut() {
                            scope.push(name.clone());
                        }
                        let ok_alloca = self.new_temp();
                        self.emit(format!("{} = alloca i1", ok_alloca));
                        self.emit(format!("store i1 {}, i1* {}", ok_tmp, ok_alloca));
                        self.locals.insert(
                            ok_name.clone(),
                            (Type::Builtin(BuiltinType::Bool), ok_alloca),
                        );
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
                .ok_or_else(|| Self::invariant_violation("select default missing block"))?;
            let default_block = default_block
                .ok_or_else(|| Self::invariant_violation("select default missing block"))?;
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
            let wait_idx =
                wait_idx.ok_or_else(|| Self::invariant_violation("select wait missing block"))?;
            self.switch_to(wait_idx);
            if !non_default.is_empty() {
                let arr_len = non_default.len();
                let arr_ty = format!("[{} x %chan*]", arr_len);
                let arr_ptr = self.new_temp();
                self.emit(format!("{} = alloca {}", arr_ptr, arr_ty));
                for (i, arm_idx) in non_default.iter().enumerate() {
                    let chan_val = arm_channels[*arm_idx]
                        .as_ref()
                        .ok_or_else(|| Self::invariant_violation("select wait missing channel"))?;
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
                let op_arr_ty = format!("[{} x i32]", arr_len);
                let op_arr_ptr = self.new_temp();
                self.emit(format!("{} = alloca {}", op_arr_ptr, op_arr_ty));
                for (i, arm_idx) in non_default.iter().enumerate() {
                    let op = arm_ops[*arm_idx].unwrap_or(0);
                    let slot = self.new_temp();
                    self.emit(format!(
                        "{} = getelementptr {}, {}* {}, i32 0, i32 {}",
                        slot, op_arr_ty, op_arr_ty, op_arr_ptr, i
                    ));
                    self.emit(format!("store i32 {}, i32* {}", op, slot));
                }
                let op_cast_ptr = self.new_temp();
                self.emit(format!(
                    "{} = bitcast {}* {} to i32*",
                    op_cast_ptr, op_arr_ty, op_arr_ptr
                ));
                self.emit(format!(
                    "call i32 @__gost_select_wait(%chan** {}, i32* {}, i32 {})",
                    cast_ptr, op_cast_ptr, arr_len
                ));
            } else {
                self.emit("call i32 @__gost_select_wait(%chan** null, i32* null, i32 0)");
            }
            self.terminate(format!("br label %{}", start_name));
        }
        self.switch_to(end_idx);
        Ok(())
    }

    fn emit_go_stmt(&mut self, expr: &Expr) -> Result<(), String> {
        let (name, type_args, args) = self.extract_direct_call(
            expr,
            "go expects a call expression",
            "go expects a direct call",
        )?;
        self.require_invariant(type_args.is_empty(), "go does not accept type arguments")?;
        self.require_invariant(name != "make_chan", "go does not support intrinsic calls")?;
        let sig = self
            .fn_sigs
            .get(name)
            .ok_or_else(|| Self::invariant_violation("unknown function for go call"))?;
        let mut resolved_args = Vec::new();
        for arg in args {
            resolved_args.push(self.emit_expr(arg)?);
        }
        self.require_invariant(
            sig.params.len() == resolved_args.len(),
            "go call argument count mismatch",
        )?;
        self.emit_go_spawn(name, &sig.params, &resolved_args)?;
        Ok(())
    }

    // Precondition: Input MIR statement has passed lowering/validation invariants.
    // Side effects: Mutates IR buffers, scopes, locals, and block termination state.
    pub(crate) fn emit_mir_stmt(&mut self, stmt: &MirStmt) -> Result<(), String> {
        match stmt {
            MirStmt::EnterScope { .. } => {
                self.enter_scope();
                Ok(())
            }
            MirStmt::ExitScope { .. } => self.exit_scope(),
            MirStmt::MatchBind { pattern, scrutinee } => {
                let scrut = self.emit_expr(scrutinee)?;
                let enum_info = match &scrut.ty {
                    Type::Named(name) => match self.types.get(name) {
                        Some(TypeDefKind::Enum(_)) => {
                            Some(self.build_enum_scrut_info(name, &scrut.ir)?)
                        }
                        _ => None,
                    },
                    _ => None,
                };
                let result_tag = match &scrut.ty {
                    Type::Result(_, _) => {
                        let tag = self.new_temp();
                        self.emit(format!(
                            "{} = extractvalue {} {}, 0",
                            tag,
                            llvm_type(&scrut.ty)?,
                            scrut.ir
                        ));
                        Some(tag)
                    }
                    _ => None,
                };
                self.emit_match_pattern_bindings(
                    pattern,
                    &scrut,
                    enum_info.as_ref(),
                    result_tag.as_ref(),
                    None,
                )
            }
            MirStmt::ForIn { name, iter, body } => self.emit_for_in_stmt(name, iter, body),
            MirStmt::Select { arms } => self.emit_select_stmt(arms),
            MirStmt::Go { expr } => self.emit_go_stmt(expr),
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
                        .ok_or_else(|| Self::invariant_violation("missing mir local"))?;
                    if !self.is_rc_fresh_expr(expr) {
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
                    if let Some(name) = &info.name
                        && self.is_linear_type(&info.ty)
                    {
                        self.mark_assigned(name);
                    }
                    return Ok(());
                }
                let (tuple_items, index_offset) = match &val.ty {
                    Type::Tuple(items) => (items.clone(), 0usize),
                    Type::Result(ok, err) => {
                        (vec![ok.as_ref().clone(), err.as_ref().clone()], 1usize)
                    }
                    _ => return self.invariant_err("mir eval expects tuple for multiple outputs"),
                };
                self.require_not_invariant(
                    tuple_items.len() != out.len(),
                    "mir eval output count mismatch",
                )?;
                for (idx, local_id) in out.iter().enumerate() {
                    let info = self
                        .mir_local_ptrs
                        .get(*local_id)
                        .and_then(|opt| opt.clone())
                        .ok_or_else(|| Self::invariant_violation("missing mir local"))?;
                    let tmp = self.new_temp();
                    self.emit(format!(
                        "{} = extractvalue {} {}, {}",
                        tmp,
                        llvm_type(&val.ty)?,
                        val.ir,
                        idx + index_offset
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
                    if let Some(name) = &info.name
                        && self.is_linear_type(&info.ty)
                    {
                        self.mark_assigned(name);
                    }
                }
                Ok(())
            }
            MirStmt::Assign { op, target, value } => {
                let (target_ty, target_ptr) = self.emit_place_ptr(target)?;
                let target_name = match &target.kind {
                    ExprKind::Ident(name) => Some(name.as_str()),
                    _ => None,
                };
                if *op == crate::frontend::ast::AssignOp::Assign && self.needs_drop(&target_ty) {
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
                let mut val = self.emit_expr(value)?;
                if !self.is_rc_fresh_expr(value) {
                    self.emit_shared_inc_value(&val)?;
                }
                if *op != crate::frontend::ast::AssignOp::Assign {
                    val = self.emit_compound_assign_value(op, &target_ty, &target_ptr, val)?;
                }
                if target_ty != val.ty && self.is_int_type(&target_ty) && self.is_int_type(&val.ty)
                {
                    val = self.cast_int_value(val, &target_ty)?;
                } else if target_ty != val.ty && is_float_type(&target_ty) && is_float_type(&val.ty)
                {
                    val = self.cast_value(val, &target_ty)?;
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
                if let Some(name) = target_name
                    && self.is_linear_type(&target_ty)
                {
                    self.mark_assigned(name);
                }
                Ok(())
            }
            MirStmt::Let { .. } => {
                self.unreachable_internal("MirStmt::Let must be lowered to Eval before codegen")
            }
            MirStmt::Drop { local } => {
                let info = self.mir_local_ptrs.get(*local).and_then(|opt| opt.clone());
                if let Some(info) = info {
                    if let Some(name) = &info.name
                        && self.is_linear_type(&info.ty)
                        && self.linear_states.get(name).copied() == Some(false)
                    {
                        return Ok(());
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
                    return self.invariant_err_fmt(format!("unknown local {} for drop", name));
                }
                Ok(())
            }
        }
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
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
            Terminator::If {
                cond,
                then_bb,
                else_bb,
            } => {
                let cond_val = self.emit_expr(cond)?;
                self.terminate(format!(
                    "br i1 {}, label %{}, label %{}",
                    cond_val.ir, block_names[*then_bb], block_names[*else_bb]
                ));
                Ok(())
            }
            Terminator::Match {
                scrutinee,
                arms,
                default,
            } => {
                let scrut_val = self.emit_expr(scrutinee)?;
                let default_bb =
                    default.ok_or_else(|| Self::invariant_violation("match default missing"))?;
                if arms.is_empty() {
                    self.terminate(format!("br label %{}", block_names[default_bb]));
                    return Ok(());
                }
                let enum_info = match &scrut_val.ty {
                    Type::Named(name) => match self.types.get(name) {
                        Some(TypeDefKind::Enum(_)) => {
                            Some(self.build_enum_scrut_info(name, &scrut_val.ir)?)
                        }
                        _ => None,
                    },
                    _ => None,
                };
                let result_tag = match &scrut_val.ty {
                    Type::Result(_, _) => {
                        let tag = self.new_temp();
                        self.emit(format!(
                            "{} = extractvalue {} {}, 0",
                            tag,
                            llvm_type(&scrut_val.ty)?,
                            scrut_val.ir
                        ));
                        Some(tag)
                    }
                    _ => None,
                };

                let mut switch_supported = true;
                let scrut_llvm_ty = llvm_type(&scrut_val.ty)?;
                let mut cases = Vec::new();
                let mut seen_literals = HashSet::new();
                for (pattern, target) in arms {
                    let mut literals = Vec::new();
                    match self.collect_switch_pattern_literals(pattern, &mut literals) {
                        Ok(has_default) => {
                            if has_default {
                                switch_supported = false;
                                break;
                            }
                            for lit in literals {
                                if !seen_literals.insert(lit.clone()) {
                                    continue;
                                }
                                cases.push(format!(
                                    "{} {}, label %{}",
                                    scrut_llvm_ty, lit, block_names[*target]
                                ));
                            }
                        }
                        Err(_) => {
                            switch_supported = false;
                            break;
                        }
                    }
                }

                if switch_supported && !cases.is_empty() {
                    self.terminate(format!(
                        "switch {} {}, label %{} [ {} ]",
                        scrut_llvm_ty,
                        scrut_val.ir,
                        block_names[default_bb],
                        cases.join(" ")
                    ));
                    return Ok(());
                }

                for (idx, (pattern, target)) in arms.iter().enumerate() {
                    let is_last = idx + 1 == arms.len();
                    let fallback_name = if is_last {
                        block_names[default_bb].clone()
                    } else {
                        self.new_block("mir_match_next")
                    };
                    let cond_ir = self.emit_match_pattern_cond(
                        pattern,
                        &scrut_val,
                        enum_info.as_ref(),
                        result_tag.as_ref(),
                    )?;
                    if cond_ir == "1" {
                        self.terminate(format!("br label %{}", block_names[*target]));
                    } else if cond_ir == "0" {
                        self.terminate(format!("br label %{}", fallback_name));
                    } else {
                        self.terminate(format!(
                            "br i1 {}, label %{}, label %{}",
                            cond_ir, block_names[*target], fallback_name
                        ));
                    }
                    if is_last {
                        return Ok(());
                    }
                    let next_idx = self.add_block(fallback_name);
                    self.switch_to(next_idx);
                }
                Ok(())
            }
            Terminator::Return { value } => {
                if let Some(expr) = value {
                    let val = self.emit_expr(expr)?;
                    self.emit_return_value(Some(val))
                } else {
                    self.require_not_invariant(
                        self.ret_type != Type::Builtin(BuiltinType::Unit),
                        "missing return value",
                    )?;
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
        let mut prefix = String::with_capacity(self.fn_name.len());
        for ch in self.fn_name.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                prefix.push(ch);
            } else {
                prefix.push('_');
            }
        }
        let name = format!("__gost_go_thunk_{}_{}", prefix, self.go_counter);
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
        lines.push(format!("  %ctx_cast = bitcast i8* %ctx to {}*", ctx_ty));
        let mut arg_ir = Vec::new();
        let mut field_slots = Vec::new();
        for (idx, ty) in param_tys.iter().enumerate() {
            let storage_ty = llvm_storage_type(ty)?;
            let slot = format!("%slot{}", idx);
            lines.push(format!(
                "  {} = getelementptr {}, {}* %ctx_cast, i32 0, i32 {}",
                slot, ctx_ty, ctx_ty, idx
            ));
            field_slots.push((slot.clone(), storage_ty.clone(), ty.clone()));
            if *ty == Type::Builtin(BuiltinType::Unit) {
                arg_ir.push(String::new());
                continue;
            }
            let tmp = format!("%arg{}", idx);
            lines.push(format!(
                "  {} = load {}, {}* {}",
                tmp, storage_ty, storage_ty, slot
            ));
            if matches!(ty, Type::Alias(_) | Type::Shared(_)) {
                let obj = format!("%sh_obj{}", idx);
                lines.push(format!("  {} = extractvalue %shared {}, 0", obj, tmp));
                lines.push(format!(
                    "  call void @__gost_shared_inc(%shared_obj* {})",
                    obj
                ));
            } else if matches!(ty, Type::Chan(_)) {
                lines.push(format!("  call void @__gost_chan_retain(%chan* {})", tmp));
            }
            arg_ir.push(tmp);
        }
        let mut args_rendered = Vec::new();
        for (arg, ty) in arg_ir.iter().zip(param_tys.iter()) {
            if *ty == Type::Builtin(BuiltinType::Unit) {
                continue;
            }
            args_rendered.push(format!("{} {}", llvm_type(ty)?, arg));
        }
        let ret_ty = llvm_type(
            &self
                .fn_sigs
                .get(callee_name)
                .ok_or_else(|| Self::invariant_violation("unknown function for go thunk"))?
                .ret,
        )?;
        let callee_symbol = self.llvm_function_symbol(callee_name);
        if ret_ty == "void" {
            lines.push(format!(
                "  call {} @{}({})",
                ret_ty,
                callee_symbol,
                args_rendered.join(", ")
            ));
        } else {
            lines.push(format!(
                "  %_ = call {} @{}({})",
                ret_ty,
                callee_symbol,
                args_rendered.join(", ")
            ));
        }
        for (idx, (slot, storage_ty, ty)) in field_slots.iter().enumerate().rev() {
            let drop_fn = self.emit_drop_fn_ptr(ty)?;
            if drop_fn == "null" {
                continue;
            }
            let cast = format!("%drop_cast{}", idx);
            lines.push(format!(
                "  {} = bitcast {}* {} to i8*",
                cast, storage_ty, slot
            ));
            lines.push(format!("  call void {}(i8* {})", drop_fn, cast));
        }
        lines.push(format!(
            "  %size_ptr = getelementptr {}, {}* null, i32 1",
            ctx_ty, ctx_ty
        ));
        lines.push(format!("  %size = ptrtoint {}* %size_ptr to i64", ctx_ty));
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
        self.emit(format!("{} = ptrtoint {}* {} to i64", size, llvm_ty, ptr));
        Ok(Value {
            ty: Type::Builtin(BuiltinType::I64),
            ir: size,
        })
    }

    fn emit_slice_obj_from_value(&mut self, value: &Value) -> Result<String, String> {
        match &value.ty {
            Type::Slice(_) | Type::Builtin(BuiltinType::Bytes) => {
                let obj = self.new_temp();
                self.emit(format!("{} = extractvalue %slice {}, 0", obj, value.ir));
                Ok(obj)
            }
            Type::Ref(inner) | Type::MutRef(inner) => match &**inner {
                Type::Slice(_) | Type::Builtin(BuiltinType::Bytes) => {
                    let tmp = self.new_temp();
                    self.emit(format!("{} = load %slice, %slice* {}", tmp, value.ir));
                    let obj = self.new_temp();
                    self.emit(format!("{} = extractvalue %slice {}, 0", obj, tmp));
                    Ok(obj)
                }
                _ => self.invariant_err("expected ref to slice"),
            },
            _ => self.invariant_err("expected slice"),
        }
    }

    fn emit_for_in_container_len(&mut self, container: &Value) -> Result<String, String> {
        match &container.ty {
            Type::Map(_, _) => {
                let map_obj = self.emit_map_obj_from_value(container)?;
                let tmp = self.new_temp();
                self.emit(format!(
                    "{} = call i64 @__gost_map_len(%map_obj* {})",
                    tmp, map_obj
                ));
                Ok(tmp)
            }
            Type::Slice(_) | Type::Builtin(BuiltinType::Bytes) => {
                let slice_obj = self.emit_slice_obj_from_value(container)?;
                let tmp = self.new_temp();
                self.emit(format!(
                    "{} = call i64 @__gost_slice_len(%slice_obj* {})",
                    tmp, slice_obj
                ));
                Ok(tmp)
            }
            Type::Builtin(BuiltinType::String) => {
                let (_ptr, len) = self.emit_string_ptr_len_from_value(container)?;
                Ok(len)
            }
            Type::Array(_, len) => Ok((*len as i64).to_string()),
            Type::Ref(inner) | Type::MutRef(inner) => match &**inner {
                Type::Slice(_) | Type::Builtin(BuiltinType::Bytes) => {
                    let slice_obj = self.emit_slice_obj_from_value(container)?;
                    let tmp = self.new_temp();
                    self.emit(format!(
                        "{} = call i64 @__gost_slice_len(%slice_obj* {})",
                        tmp, slice_obj
                    ));
                    Ok(tmp)
                }
                Type::Builtin(BuiltinType::String) => {
                    let (_ptr, len) = self.emit_string_ptr_len_from_value(container)?;
                    Ok(len)
                }
                Type::Map(_, _) => {
                    let map_obj = self.emit_map_obj_from_value(container)?;
                    let tmp = self.new_temp();
                    self.emit(format!(
                        "{} = call i64 @__gost_map_len(%map_obj* {})",
                        tmp, map_obj
                    ));
                    Ok(tmp)
                }
                Type::Array(_, len) => Ok((*len as i64).to_string()),
                _ => self.invariant_err("invalid MIR container type for $for_in_len"),
            },
            _ => self.invariant_err("invalid MIR container type for $for_in_len"),
        }
    }

    fn emit_for_in_elem_ptr(
        &mut self,
        container: &Value,
        elem_ty: &Type,
        idx: &Value,
    ) -> Result<String, String> {
        match &container.ty {
            Type::Slice(inner) => {
                self.require_not_invariant(
                    **inner != *elem_ty,
                    "for_in container element type mismatch",
                )?;
                let slice_obj = self.emit_slice_obj_from_value(container)?;
                self.emit_slice_elem_ptr(&slice_obj, elem_ty, idx, true)
            }
            Type::Builtin(BuiltinType::Bytes) => {
                self.require_not_invariant(
                    *elem_ty != Type::Builtin(BuiltinType::U32),
                    "for_in bytes element type mismatch",
                )?;
                let slice_obj = self.emit_slice_obj_from_value(container)?;
                self.emit_slice_elem_ptr(&slice_obj, elem_ty, idx, true)
            }
            Type::Array(inner, len) => {
                self.require_not_invariant(
                    **inner != *elem_ty,
                    "for_in container element type mismatch",
                )?;
                let array_ptr = self.materialize_value(container)?;
                self.emit_array_elem_ptr(&array_ptr, elem_ty, *len, idx, true)
            }
            Type::Ref(inner) | Type::MutRef(inner) => match &**inner {
                Type::Slice(slice_elem) => {
                    self.require_not_invariant(
                        **slice_elem != *elem_ty,
                        "for_in container element type mismatch",
                    )?;
                    let slice_obj = self.emit_slice_obj_from_value(container)?;
                    self.emit_slice_elem_ptr(&slice_obj, elem_ty, idx, true)
                }
                Type::Builtin(BuiltinType::Bytes) => {
                    self.require_not_invariant(
                        *elem_ty != Type::Builtin(BuiltinType::U32),
                        "for_in bytes element type mismatch",
                    )?;
                    let slice_obj = self.emit_slice_obj_from_value(container)?;
                    self.emit_slice_elem_ptr(&slice_obj, elem_ty, idx, true)
                }
                Type::Array(array_elem, len) => {
                    self.require_not_invariant(
                        **array_elem != *elem_ty,
                        "for_in container element type mismatch",
                    )?;
                    self.emit_array_elem_ptr(&container.ir, elem_ty, *len, idx, true)
                }
                _ => self.invariant_err("invalid MIR container type for $for_in_get"),
            },
            _ => self.invariant_err("invalid MIR container type for $for_in_get"),
        }
    }

    fn emit_string_ptr_len_from_value(
        &mut self,
        value: &Value,
    ) -> Result<(String, String), String> {
        match &value.ty {
            Type::Builtin(BuiltinType::String) => {
                let ptr = self.new_temp();
                let len = self.new_temp();
                self.emit(format!("{} = extractvalue %string {}, 0", ptr, value.ir));
                self.emit(format!("{} = extractvalue %string {}, 1", len, value.ir));
                Ok((ptr, len))
            }
            Type::Ref(inner) | Type::MutRef(inner) => match &**inner {
                Type::Builtin(BuiltinType::String) => {
                    let tmp = self.new_temp();
                    self.emit(format!("{} = load %string, %string* {}", tmp, value.ir));
                    let ptr = self.new_temp();
                    let len = self.new_temp();
                    self.emit(format!("{} = extractvalue %string {}, 0", ptr, tmp));
                    self.emit(format!("{} = extractvalue %string {}, 1", len, tmp));
                    Ok((ptr, len))
                }
                _ => self.invariant_err("expected ref to string"),
            },
            _ => self.invariant_err("expected string"),
        }
    }

    fn emit_map_obj_from_value(&mut self, value: &Value) -> Result<String, String> {
        match &value.ty {
            Type::Map(_, _) => {
                let obj = self.new_temp();
                self.emit(format!("{} = extractvalue %map {}, 0", obj, value.ir));
                Ok(obj)
            }
            Type::Ref(inner) | Type::MutRef(inner) => match &**inner {
                Type::Map(_, _) => {
                    let tmp = self.new_temp();
                    self.emit(format!("{} = load %map, %map* {}", tmp, value.ir));
                    let obj = self.new_temp();
                    self.emit(format!("{} = extractvalue %map {}, 0", obj, tmp));
                    Ok(obj)
                }
                _ => self.unreachable_internal("expected ref to map"),
            },
            _ => self.unreachable_internal("expected map"),
        }
    }

    fn emit_shared_obj_from_value(&mut self, value: &Value) -> Result<String, String> {
        match &value.ty {
            Type::Own(_) | Type::Alias(_) | Type::Shared(_) => {
                let obj = self.new_temp();
                self.emit(format!("{} = extractvalue %shared {}, 0", obj, value.ir));
                Ok(obj)
            }
            Type::Ref(inner) | Type::MutRef(inner) => match &**inner {
                Type::Own(_) | Type::Alias(_) | Type::Shared(_) => {
                    let tmp = self.new_temp();
                    self.emit(format!("{} = load %shared, %shared* {}", tmp, value.ir));
                    let obj = self.new_temp();
                    self.emit(format!("{} = extractvalue %shared {}, 0", obj, tmp));
                    Ok(obj)
                }
                _ => self.invariant_err("expected ref to own/alias/shared"),
            },
            _ => self.invariant_err("expected own/alias/shared"),
        }
    }

    fn emit_shared_inc_value(&mut self, value: &Value) -> Result<(), String> {
        match &value.ty {
            Type::Alias(_) | Type::Shared(_) => {
                let obj = self.emit_shared_obj_from_value(value)?;
                self.emit(format!(
                    "call void @__gost_shared_inc(%shared_obj* {})",
                    obj
                ));
            }
            Type::Chan(_) => {
                self.emit(format!(
                    "call void @__gost_chan_retain(%chan* {})",
                    value.ir
                ));
            }
            _ => {}
        }
        Ok(())
    }

    fn is_rc_fresh_expr(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::After { .. } => true,
            ExprKind::Call { callee, .. } => {
                if let ExprKind::Ident(name) = &callee.kind {
                    name == "shared_new" || name == "own_new" || name == "make_chan"
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn int_info(&self, ty: &Type) -> Option<(bool, u32)> {
        match ty {
            Type::Builtin(BuiltinType::I8) => Some((true, 8)),
            Type::Builtin(BuiltinType::I16) => Some((true, 16)),
            Type::Builtin(BuiltinType::I32) => Some((true, 32)),
            Type::Builtin(BuiltinType::I64) => Some((true, 64)),
            Type::Builtin(BuiltinType::Isize) => Some((true, 64)),
            Type::Builtin(BuiltinType::U8) => Some((false, 8)),
            Type::Builtin(BuiltinType::U16) => Some((false, 16)),
            Type::Builtin(BuiltinType::U32) => Some((false, 32)),
            Type::Builtin(BuiltinType::U64) => Some((false, 64)),
            Type::Builtin(BuiltinType::Usize) => Some((false, 64)),
            Type::Builtin(BuiltinType::Char) => Some((false, 32)),
            _ => None,
        }
    }

    fn is_int_type(&self, ty: &Type) -> bool {
        self.int_info(ty).is_some()
    }

    fn is_signed_int_type(&self, ty: &Type) -> bool {
        self.int_info(ty).map(|(signed, _)| signed).unwrap_or(false)
    }

    fn promote_int_type(&self, a: &Type, b: &Type) -> Option<Type> {
        if a == b {
            return Some(a.clone());
        }
        let (a_signed, a_bits) = self.int_info(a)?;
        let (b_signed, b_bits) = self.int_info(b)?;
        if a_signed != b_signed {
            return None;
        }
        let bits = a_bits.max(b_bits);
        Some(match (a_signed, bits) {
            (true, 8) => Type::Builtin(BuiltinType::I8),
            (true, 16) => Type::Builtin(BuiltinType::I16),
            (true, 32) => Type::Builtin(BuiltinType::I32),
            (true, 64) => Type::Builtin(BuiltinType::I64),
            (false, 8) => Type::Builtin(BuiltinType::U8),
            (false, 16) => Type::Builtin(BuiltinType::U16),
            (false, 32) => Type::Builtin(BuiltinType::U32),
            (false, 64) => Type::Builtin(BuiltinType::U64),
            _ => return None,
        })
    }

    fn map_key_tmp(prefix: &str, counter: &mut usize) -> String {
        let name = format!("%{}_{}", prefix, *counter);
        *counter += 1;
        name
    }

    fn combine_i1_and(
        lines: &mut Vec<String>,
        counter: &mut usize,
        lhs: String,
        rhs: String,
    ) -> String {
        if lhs == "0" || rhs == "0" {
            return "0".to_string();
        }
        if lhs == "1" {
            return rhs;
        }
        if rhs == "1" {
            return lhs;
        }
        let out = Self::map_key_tmp("and", counter);
        lines.push(format!("  {} = and i1 {}, {}", out, lhs, rhs));
        out
    }

    fn mix_hash_ir(
        lines: &mut Vec<String>,
        counter: &mut usize,
        acc: String,
        part: String,
    ) -> String {
        let xor = Self::map_key_tmp("hxor", counter);
        lines.push(format!("  {} = xor i64 {}, {}", xor, acc, part));
        let mul = Self::map_key_tmp("hmul", counter);
        lines.push(format!("  {} = mul i64 {}, 1099511628211", mul, xor));
        mul
    }

    fn map_key_enum_tag_ty(&self, enum_name: &str) -> Result<Type, String> {
        let def = match self.types.get(enum_name) {
            Some(TypeDefKind::Enum(def)) => def,
            _ => return self.invariant_err_fmt(format!("unknown enum {}", enum_name)),
        };
        Ok(match def.layout.repr_int {
            Some(crate::sema::types::ReprInt::I8) => Type::Builtin(BuiltinType::I8),
            Some(crate::sema::types::ReprInt::I16) => Type::Builtin(BuiltinType::I16),
            Some(crate::sema::types::ReprInt::I32) => Type::Builtin(BuiltinType::I32),
            Some(crate::sema::types::ReprInt::I64) => Type::Builtin(BuiltinType::I64),
            Some(crate::sema::types::ReprInt::Isize) => Type::Builtin(BuiltinType::Isize),
            Some(crate::sema::types::ReprInt::U8) => Type::Builtin(BuiltinType::U8),
            Some(crate::sema::types::ReprInt::U16) => Type::Builtin(BuiltinType::U16),
            Some(crate::sema::types::ReprInt::U32) => Type::Builtin(BuiltinType::U32),
            Some(crate::sema::types::ReprInt::U64) => Type::Builtin(BuiltinType::U64),
            Some(crate::sema::types::ReprInt::Usize) => Type::Builtin(BuiltinType::Usize),
            None => Type::Builtin(BuiltinType::I32),
        })
    }

    fn map_key_label(prefix: &str, counter: &mut usize) -> String {
        let name = format!("{}_{}", prefix, *counter);
        *counter += 1;
        name
    }

    fn emit_map_key_enum_eq_from_ptr(
        &self,
        enum_name: &str,
        lhs_ptr: &str,
        rhs_ptr: &str,
        lines: &mut Vec<String>,
        counter: &mut usize,
    ) -> Result<String, String> {
        let def = self.enum_def(enum_name)?;
        let named_ty = format!("%{}", enum_name);
        let tag_llvm = self.enum_tag_llvm_type(enum_name)?;

        let lhs_val = Self::map_key_tmp("lhs_enum", counter);
        lines.push(format!(
            "  {} = load {}, {}* {}",
            lhs_val, named_ty, named_ty, lhs_ptr
        ));
        let rhs_val = Self::map_key_tmp("rhs_enum", counter);
        lines.push(format!(
            "  {} = load {}, {}* {}",
            rhs_val, named_ty, named_ty, rhs_ptr
        ));
        let lhs_tag = Self::map_key_tmp("lhs_tag", counter);
        lines.push(format!(
            "  {} = extractvalue {} {}, 0",
            lhs_tag, named_ty, lhs_val
        ));
        let rhs_tag = Self::map_key_tmp("rhs_tag", counter);
        lines.push(format!(
            "  {} = extractvalue {} {}, 0",
            rhs_tag, named_ty, rhs_val
        ));
        let tag_eq = Self::map_key_tmp("tag_eq", counter);
        lines.push(format!(
            "  {} = icmp eq {} {}, {}",
            tag_eq, tag_llvm, lhs_tag, rhs_tag
        ));

        if def.variants.iter().all(|(_, fields)| fields.is_empty()) {
            return Ok(tag_eq);
        }

        let mismatch_label = Self::map_key_label("mk_eq_enum_mismatch", counter);
        let dispatch_label = Self::map_key_label("mk_eq_enum_dispatch", counter);
        let end_label = Self::map_key_label("mk_eq_enum_end", counter);

        lines.push(format!(
            "  br i1 {}, label %{}, label %{}",
            tag_eq, dispatch_label, mismatch_label
        ));
        lines.push(format!("{}:", mismatch_label));
        lines.push(format!("  br label %{}", end_label));

        lines.push(format!("{}:", dispatch_label));
        let mut switch_cases = Vec::new();
        let mut case_labels = Vec::new();
        for (idx, _) in def.variants.iter().enumerate() {
            let case_label = Self::map_key_label("mk_eq_enum_case", counter);
            switch_cases.push(format!("{} {}, label %{}", tag_llvm, idx, case_label));
            case_labels.push(case_label);
        }
        lines.push(format!(
            "  switch {} {}, label %{} [ {} ]",
            tag_llvm,
            lhs_tag,
            mismatch_label,
            switch_cases.join(" ")
        ));

        let mut phi_incomings = vec![format!("[ 0, %{} ]", mismatch_label)];

        for ((_, fields), case_label) in def.variants.iter().zip(case_labels.iter()) {
            lines.push(format!("{}:", case_label));
            if fields.is_empty() {
                lines.push(format!("  br label %{}", end_label));
                phi_incomings.push(format!("[ 1, %{} ]", case_label));
                continue;
            }

            let lhs_payload = Self::map_key_tmp("lhs_payload", counter);
            lines.push(format!(
                "  {} = extractvalue {} {}, 1",
                lhs_payload, named_ty, lhs_val
            ));
            let rhs_payload = Self::map_key_tmp("rhs_payload", counter);
            lines.push(format!(
                "  {} = extractvalue {} {}, 1",
                rhs_payload, named_ty, rhs_val
            ));
            let lhs_null = Self::map_key_tmp("lhs_payload_null", counter);
            lines.push(format!(
                "  {} = icmp eq i8* {}, null",
                lhs_null, lhs_payload
            ));
            let rhs_null = Self::map_key_tmp("rhs_payload_null", counter);
            lines.push(format!(
                "  {} = icmp eq i8* {}, null",
                rhs_null, rhs_payload
            ));
            let any_null = Self::map_key_tmp("payload_any_null", counter);
            lines.push(format!("  {} = or i1 {}, {}", any_null, lhs_null, rhs_null));

            let null_label = Self::map_key_label("mk_eq_enum_null", counter);
            let work_label = Self::map_key_label("mk_eq_enum_work", counter);
            lines.push(format!(
                "  br i1 {}, label %{}, label %{}",
                any_null, null_label, work_label
            ));

            lines.push(format!("{}:", null_label));
            lines.push(format!("  br label %{}", end_label));
            phi_incomings.push(format!("[ 0, %{} ]", null_label));

            lines.push(format!("{}:", work_label));
            let payload_ty = self.enum_payload_type_name(
                enum_name,
                case_labels
                    .iter()
                    .position(|label| label == case_label)
                    .ok_or_else(|| Self::invariant_violation("enum case index lookup failed"))?,
            );
            let lhs_payload_ptr = Self::map_key_tmp("lhs_payload_ptr", counter);
            lines.push(format!(
                "  {} = bitcast i8* {} to {}*",
                lhs_payload_ptr, lhs_payload, payload_ty
            ));
            let rhs_payload_ptr = Self::map_key_tmp("rhs_payload_ptr", counter);
            lines.push(format!(
                "  {} = bitcast i8* {} to {}*",
                rhs_payload_ptr, rhs_payload, payload_ty
            ));

            let mut acc = "1".to_string();
            for (field_idx, field_ty) in fields.iter().enumerate() {
                let lhs_field_ptr = Self::map_key_tmp("lhs_field_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                    lhs_field_ptr, payload_ty, payload_ty, lhs_payload_ptr, field_idx
                ));
                let rhs_field_ptr = Self::map_key_tmp("rhs_field_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                    rhs_field_ptr, payload_ty, payload_ty, rhs_payload_ptr, field_idx
                ));
                let eq = self.emit_map_key_eq_from_ptr(
                    field_ty,
                    &lhs_field_ptr,
                    &rhs_field_ptr,
                    lines,
                    counter,
                )?;
                acc = Self::combine_i1_and(lines, counter, acc, eq);
            }
            lines.push(format!("  br label %{}", end_label));
            phi_incomings.push(format!("[ {}, %{} ]", acc, work_label));
        }

        lines.push(format!("{}:", end_label));
        let out = Self::map_key_tmp("enum_eq", counter);
        lines.push(format!("  {} = phi i1 {}", out, phi_incomings.join(", ")));
        Ok(out)
    }

    fn emit_map_key_enum_hash_from_ptr(
        &self,
        enum_name: &str,
        ptr: &str,
        lines: &mut Vec<String>,
        counter: &mut usize,
    ) -> Result<String, String> {
        let def = self.enum_def(enum_name)?;
        let named_ty = format!("%{}", enum_name);
        let tag_ty = self.map_key_enum_tag_ty(enum_name)?;
        let tag_llvm = self.enum_tag_llvm_type(enum_name)?;

        let val = Self::map_key_tmp("enum_val", counter);
        lines.push(format!(
            "  {} = load {}, {}* {}",
            val, named_ty, named_ty, ptr
        ));
        let tag = Self::map_key_tmp("enum_tag", counter);
        lines.push(format!("  {} = extractvalue {} {}, 0", tag, named_ty, val));

        let tag_alloca = Self::map_key_tmp("enum_tag_alloca", counter);
        lines.push(format!("  {} = alloca {}", tag_alloca, tag_llvm));
        lines.push(format!(
            "  store {} {}, {}* {}",
            tag_llvm, tag, tag_llvm, tag_alloca
        ));
        let tag_hash = self.emit_map_key_hash_from_ptr(&tag_ty, &tag_alloca, lines, counter)?;

        if def.variants.iter().all(|(_, fields)| fields.is_empty()) {
            return Ok(tag_hash);
        }

        let dispatch_label = Self::map_key_label("mk_hash_enum_dispatch", counter);
        let default_label = Self::map_key_label("mk_hash_enum_default", counter);
        let end_label = Self::map_key_label("mk_hash_enum_end", counter);
        lines.push(format!("  br label %{}", dispatch_label));

        lines.push(format!("{}:", dispatch_label));
        let mut switch_cases = Vec::new();
        let mut case_labels = Vec::new();
        for (idx, _) in def.variants.iter().enumerate() {
            let case_label = Self::map_key_label("mk_hash_enum_case", counter);
            switch_cases.push(format!("{} {}, label %{}", tag_llvm, idx, case_label));
            case_labels.push(case_label);
        }
        lines.push(format!(
            "  switch {} {}, label %{} [ {} ]",
            tag_llvm,
            tag,
            default_label,
            switch_cases.join(" ")
        ));

        lines.push(format!("{}:", default_label));
        lines.push(format!("  br label %{}", end_label));

        let mut phi_incomings = vec![format!("[ {}, %{} ]", tag_hash, default_label)];

        for (variant_idx, ((_, fields), case_label)) in
            def.variants.iter().zip(case_labels.iter()).enumerate()
        {
            lines.push(format!("{}:", case_label));
            if fields.is_empty() {
                lines.push(format!("  br label %{}", end_label));
                phi_incomings.push(format!("[ {}, %{} ]", tag_hash, case_label));
                continue;
            }

            let payload = Self::map_key_tmp("enum_payload", counter);
            lines.push(format!(
                "  {} = extractvalue {} {}, 1",
                payload, named_ty, val
            ));
            let payload_null = Self::map_key_tmp("enum_payload_null", counter);
            lines.push(format!(
                "  {} = icmp eq i8* {}, null",
                payload_null, payload
            ));
            let null_label = Self::map_key_label("mk_hash_enum_null", counter);
            let work_label = Self::map_key_label("mk_hash_enum_work", counter);
            lines.push(format!(
                "  br i1 {}, label %{}, label %{}",
                payload_null, null_label, work_label
            ));

            lines.push(format!("{}:", null_label));
            lines.push(format!("  br label %{}", end_label));
            phi_incomings.push(format!("[ {}, %{} ]", tag_hash, null_label));

            lines.push(format!("{}:", work_label));
            let payload_ty = self.enum_payload_type_name(enum_name, variant_idx);
            let payload_ptr = Self::map_key_tmp("enum_payload_ptr", counter);
            lines.push(format!(
                "  {} = bitcast i8* {} to {}*",
                payload_ptr, payload, payload_ty
            ));

            let mut acc = tag_hash.clone();
            for (field_idx, field_ty) in fields.iter().enumerate() {
                let field_ptr = Self::map_key_tmp("enum_field_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                    field_ptr, payload_ty, payload_ty, payload_ptr, field_idx
                ));
                let part = self.emit_map_key_hash_from_ptr(field_ty, &field_ptr, lines, counter)?;
                acc = Self::mix_hash_ir(lines, counter, acc, part);
            }
            lines.push(format!("  br label %{}", end_label));
            phi_incomings.push(format!("[ {}, %{} ]", acc, work_label));
        }

        lines.push(format!("{}:", end_label));
        let out = Self::map_key_tmp("enum_hash", counter);
        lines.push(format!("  {} = phi i64 {}", out, phi_incomings.join(", ")));
        Ok(out)
    }

    fn emit_map_key_enum_clone_from_ptr(
        &self,
        enum_name: &str,
        dst_ptr: &str,
        src_ptr: &str,
        lines: &mut Vec<String>,
        counter: &mut usize,
    ) -> Result<(), String> {
        let def = self.enum_def(enum_name)?;
        let named_ty = format!("%{}", enum_name);
        let tag_llvm = self.enum_tag_llvm_type(enum_name)?;

        let src_val = Self::map_key_tmp("enum_src", counter);
        lines.push(format!(
            "  {} = load {}, {}* {}",
            src_val, named_ty, named_ty, src_ptr
        ));
        let src_tag = Self::map_key_tmp("enum_src_tag", counter);
        lines.push(format!(
            "  {} = extractvalue {} {}, 0",
            src_tag, named_ty, src_val
        ));

        if def.variants.iter().all(|(_, fields)| fields.is_empty()) {
            lines.push(format!(
                "  store {} {}, {}* {}",
                named_ty, src_val, named_ty, dst_ptr
            ));
            return Ok(());
        }

        let dispatch_label = Self::map_key_label("mk_clone_enum_dispatch", counter);
        let default_label = Self::map_key_label("mk_clone_enum_default", counter);
        let end_label = Self::map_key_label("mk_clone_enum_end", counter);

        lines.push(format!("  br label %{}", dispatch_label));
        lines.push(format!("{}:", dispatch_label));
        let mut switch_cases = Vec::new();
        let mut case_labels = Vec::new();
        for (idx, _) in def.variants.iter().enumerate() {
            let case_label = Self::map_key_label("mk_clone_enum_case", counter);
            switch_cases.push(format!("{} {}, label %{}", tag_llvm, idx, case_label));
            case_labels.push(case_label);
        }
        lines.push(format!(
            "  switch {} {}, label %{} [ {} ]",
            tag_llvm,
            src_tag,
            default_label,
            switch_cases.join(" ")
        ));

        lines.push(format!("{}:", default_label));
        lines.push(format!(
            "  store {} {}, {}* {}",
            named_ty, src_val, named_ty, dst_ptr
        ));
        lines.push(format!("  br label %{}", end_label));

        for (variant_idx, ((_, fields), case_label)) in
            def.variants.iter().zip(case_labels.iter()).enumerate()
        {
            lines.push(format!("{}:", case_label));
            if fields.is_empty() {
                lines.push(format!(
                    "  store {} {}, {}* {}",
                    named_ty, src_val, named_ty, dst_ptr
                ));
                lines.push(format!("  br label %{}", end_label));
                continue;
            }

            let src_payload = Self::map_key_tmp("enum_src_payload", counter);
            lines.push(format!(
                "  {} = extractvalue {} {}, 1",
                src_payload, named_ty, src_val
            ));
            let payload_null = Self::map_key_tmp("enum_payload_null", counter);
            lines.push(format!(
                "  {} = icmp eq i8* {}, null",
                payload_null, src_payload
            ));
            let null_label = Self::map_key_label("mk_clone_enum_null", counter);
            let work_label = Self::map_key_label("mk_clone_enum_work", counter);
            lines.push(format!(
                "  br i1 {}, label %{}, label %{}",
                payload_null, null_label, work_label
            ));

            lines.push(format!("{}:", null_label));
            lines.push(format!(
                "  store {} {}, {}* {}",
                named_ty, src_val, named_ty, dst_ptr
            ));
            lines.push(format!("  br label %{}", end_label));

            lines.push(format!("{}:", work_label));
            let payload_ty = self.enum_payload_type_name(enum_name, variant_idx);
            let size_ptr = Self::map_key_tmp("enum_payload_size_ptr", counter);
            lines.push(format!(
                "  {} = getelementptr {}, {}* null, i32 1",
                size_ptr, payload_ty, payload_ty
            ));
            let size = Self::map_key_tmp("enum_payload_size", counter);
            lines.push(format!(
                "  {} = ptrtoint {}* {} to i64",
                size, payload_ty, size_ptr
            ));
            let raw = Self::map_key_tmp("enum_payload_raw", counter);
            lines.push(format!(
                "  {} = call i8* @__gost_alloc(i64 {}, i64 1)",
                raw, size
            ));
            let dst_payload_ptr = Self::map_key_tmp("enum_dst_payload_ptr", counter);
            lines.push(format!(
                "  {} = bitcast i8* {} to {}*",
                dst_payload_ptr, raw, payload_ty
            ));
            let src_payload_ptr = Self::map_key_tmp("enum_src_payload_ptr", counter);
            lines.push(format!(
                "  {} = bitcast i8* {} to {}*",
                src_payload_ptr, src_payload, payload_ty
            ));

            for (field_idx, field_ty) in fields.iter().enumerate() {
                let dst_field_ptr = Self::map_key_tmp("enum_dst_field_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                    dst_field_ptr, payload_ty, payload_ty, dst_payload_ptr, field_idx
                ));
                let src_field_ptr = Self::map_key_tmp("enum_src_field_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                    src_field_ptr, payload_ty, payload_ty, src_payload_ptr, field_idx
                ));
                self.emit_map_key_clone_from_ptr(
                    field_ty,
                    &dst_field_ptr,
                    &src_field_ptr,
                    lines,
                    counter,
                )?;
            }

            let agg0 = Self::map_key_tmp("enum_clone_agg0", counter);
            lines.push(format!(
                "  {} = insertvalue {} undef, {} {}, 0",
                agg0, named_ty, tag_llvm, src_tag
            ));
            let agg1 = Self::map_key_tmp("enum_clone_agg1", counter);
            lines.push(format!(
                "  {} = insertvalue {} {}, i8* {}, 1",
                agg1, named_ty, agg0, raw
            ));
            lines.push(format!(
                "  store {} {}, {}* {}",
                named_ty, agg1, named_ty, dst_ptr
            ));
            lines.push(format!("  br label %{}", end_label));
        }

        lines.push(format!("{}:", end_label));
        Ok(())
    }

    fn emit_map_key_eq_from_ptr(
        &self,
        ty: &Type,
        lhs_ptr: &str,
        rhs_ptr: &str,
        lines: &mut Vec<String>,
        counter: &mut usize,
    ) -> Result<String, String> {
        match ty {
            Type::Builtin(BuiltinType::Bool)
            | Type::Builtin(BuiltinType::I8)
            | Type::Builtin(BuiltinType::I16)
            | Type::Builtin(BuiltinType::I32)
            | Type::Builtin(BuiltinType::I64)
            | Type::Builtin(BuiltinType::Isize)
            | Type::Builtin(BuiltinType::U8)
            | Type::Builtin(BuiltinType::U16)
            | Type::Builtin(BuiltinType::U32)
            | Type::Builtin(BuiltinType::U64)
            | Type::Builtin(BuiltinType::Usize)
            | Type::Builtin(BuiltinType::Char)
            | Type::Builtin(BuiltinType::Error) => {
                let storage_ty = llvm_storage_type(ty)?;
                let llvm_ty = llvm_type(ty)?;
                let lhs_val = Self::map_key_tmp("lhsv", counter);
                lines.push(format!(
                    "  {} = load {}, {}* {}",
                    lhs_val, llvm_ty, storage_ty, lhs_ptr
                ));
                let rhs_val = Self::map_key_tmp("rhsv", counter);
                lines.push(format!(
                    "  {} = load {}, {}* {}",
                    rhs_val, llvm_ty, storage_ty, rhs_ptr
                ));
                let cmp = Self::map_key_tmp("eq", counter);
                lines.push(format!(
                    "  {} = icmp eq {} {}, {}",
                    cmp, llvm_ty, lhs_val, rhs_val
                ));
                Ok(cmp)
            }
            Type::Builtin(BuiltinType::String) => {
                let storage_ty = llvm_storage_type(ty)?;
                let lhs_val = Self::map_key_tmp("lhsv", counter);
                lines.push(format!(
                    "  {} = load %string, {}* {}",
                    lhs_val, storage_ty, lhs_ptr
                ));
                let rhs_val = Self::map_key_tmp("rhsv", counter);
                lines.push(format!(
                    "  {} = load %string, {}* {}",
                    rhs_val, storage_ty, rhs_ptr
                ));
                let lhs_ptr_v = Self::map_key_tmp("lhs_ptr", counter);
                lines.push(format!(
                    "  {} = extractvalue %string {}, 0",
                    lhs_ptr_v, lhs_val
                ));
                let rhs_ptr_v = Self::map_key_tmp("rhs_ptr", counter);
                lines.push(format!(
                    "  {} = extractvalue %string {}, 0",
                    rhs_ptr_v, rhs_val
                ));
                let lhs_len_v = Self::map_key_tmp("lhs_len", counter);
                lines.push(format!(
                    "  {} = extractvalue %string {}, 1",
                    lhs_len_v, lhs_val
                ));
                let rhs_len_v = Self::map_key_tmp("rhs_len", counter);
                lines.push(format!(
                    "  {} = extractvalue %string {}, 1",
                    rhs_len_v, rhs_val
                ));
                let eq_len = Self::map_key_tmp("eq_len", counter);
                lines.push(format!(
                    "  {} = icmp eq i64 {}, {}",
                    eq_len, lhs_len_v, rhs_len_v
                ));
                let cmp_len = Self::map_key_tmp("cmp_len", counter);
                lines.push(format!(
                    "  {} = select i1 {}, i64 {}, i64 0",
                    cmp_len, eq_len, lhs_len_v
                ));
                let eq_i32 = Self::map_key_tmp("eq_i32", counter);
                lines.push(format!(
                    "  {} = call i32 @__gost_map_eq_bytes(i8* {}, i8* {}, i64 {})",
                    eq_i32, lhs_ptr_v, rhs_ptr_v, cmp_len
                ));
                let eq_bytes = Self::map_key_tmp("eq_bytes", counter);
                lines.push(format!("  {} = icmp ne i32 {}, 0", eq_bytes, eq_i32));
                Ok(Self::combine_i1_and(lines, counter, eq_len, eq_bytes))
            }
            Type::Builtin(BuiltinType::F32) => {
                let storage_ty = llvm_storage_type(ty)?;
                let lhs_val = Self::map_key_tmp("lhsv", counter);
                lines.push(format!(
                    "  {} = load float, {}* {}",
                    lhs_val, storage_ty, lhs_ptr
                ));
                let rhs_val = Self::map_key_tmp("rhsv", counter);
                lines.push(format!(
                    "  {} = load float, {}* {}",
                    rhs_val, storage_ty, rhs_ptr
                ));
                let lhs_bits = Self::map_key_tmp("lhs_bits", counter);
                lines.push(format!("  {} = bitcast float {} to i32", lhs_bits, lhs_val));
                let rhs_bits = Self::map_key_tmp("rhs_bits", counter);
                lines.push(format!("  {} = bitcast float {} to i32", rhs_bits, rhs_val));
                let cmp = Self::map_key_tmp("eq", counter);
                lines.push(format!(
                    "  {} = icmp eq i32 {}, {}",
                    cmp, lhs_bits, rhs_bits
                ));
                Ok(cmp)
            }
            Type::Builtin(BuiltinType::F64) => {
                let storage_ty = llvm_storage_type(ty)?;
                let lhs_val = Self::map_key_tmp("lhsv", counter);
                lines.push(format!(
                    "  {} = load double, {}* {}",
                    lhs_val, storage_ty, lhs_ptr
                ));
                let rhs_val = Self::map_key_tmp("rhsv", counter);
                lines.push(format!(
                    "  {} = load double, {}* {}",
                    rhs_val, storage_ty, rhs_ptr
                ));
                let lhs_bits = Self::map_key_tmp("lhs_bits", counter);
                lines.push(format!(
                    "  {} = bitcast double {} to i64",
                    lhs_bits, lhs_val
                ));
                let rhs_bits = Self::map_key_tmp("rhs_bits", counter);
                lines.push(format!(
                    "  {} = bitcast double {} to i64",
                    rhs_bits, rhs_val
                ));
                let cmp = Self::map_key_tmp("eq", counter);
                lines.push(format!(
                    "  {} = icmp eq i64 {}, {}",
                    cmp, lhs_bits, rhs_bits
                ));
                Ok(cmp)
            }
            Type::FnPtr { .. } => {
                let storage_ty = llvm_storage_type(ty)?;
                let llvm_ty = llvm_type(ty)?;
                let lhs_val = Self::map_key_tmp("lhsv", counter);
                lines.push(format!(
                    "  {} = load {}, {}* {}",
                    lhs_val, llvm_ty, storage_ty, lhs_ptr
                ));
                let rhs_val = Self::map_key_tmp("rhsv", counter);
                lines.push(format!(
                    "  {} = load {}, {}* {}",
                    rhs_val, llvm_ty, storage_ty, rhs_ptr
                ));
                let cmp = Self::map_key_tmp("eq", counter);
                lines.push(format!(
                    "  {} = icmp eq {} {}, {}",
                    cmp, llvm_ty, lhs_val, rhs_val
                ));
                Ok(cmp)
            }
            Type::Alias(_) | Type::Shared(_) => {
                let storage_ty = llvm_storage_type(ty)?;
                let lhs_val = Self::map_key_tmp("lhsv", counter);
                lines.push(format!(
                    "  {} = load %shared, {}* {}",
                    lhs_val, storage_ty, lhs_ptr
                ));
                let rhs_val = Self::map_key_tmp("rhsv", counter);
                lines.push(format!(
                    "  {} = load %shared, {}* {}",
                    rhs_val, storage_ty, rhs_ptr
                ));
                let lhs_obj = Self::map_key_tmp("lhs_obj", counter);
                lines.push(format!(
                    "  {} = extractvalue %shared {}, 0",
                    lhs_obj, lhs_val
                ));
                let rhs_obj = Self::map_key_tmp("rhs_obj", counter);
                lines.push(format!(
                    "  {} = extractvalue %shared {}, 0",
                    rhs_obj, rhs_val
                ));
                let cmp = Self::map_key_tmp("eq", counter);
                lines.push(format!(
                    "  {} = icmp eq %shared_obj* {}, {}",
                    cmp, lhs_obj, rhs_obj
                ));
                Ok(cmp)
            }
            Type::Chan(_) => {
                let storage_ty = llvm_storage_type(ty)?;
                let llvm_ty = llvm_type(ty)?;
                let lhs_val = Self::map_key_tmp("lhsv", counter);
                lines.push(format!(
                    "  {} = load {}, {}* {}",
                    lhs_val, llvm_ty, storage_ty, lhs_ptr
                ));
                let rhs_val = Self::map_key_tmp("rhsv", counter);
                lines.push(format!(
                    "  {} = load {}, {}* {}",
                    rhs_val, llvm_ty, storage_ty, rhs_ptr
                ));
                let cmp = Self::map_key_tmp("eq", counter);
                lines.push(format!(
                    "  {} = icmp eq {} {}, {}",
                    cmp, llvm_ty, lhs_val, rhs_val
                ));
                Ok(cmp)
            }
            Type::Array(inner, len) => {
                let arr_ty = llvm_storage_type(ty)?;
                let mut acc = "1".to_string();
                for idx in 0..*len {
                    let lhs_elem_ptr = Self::map_key_tmp("lhs_elem_ptr", counter);
                    lines.push(format!(
                        "  {} = getelementptr {}, {}* {}, i32 0, i64 {}",
                        lhs_elem_ptr, arr_ty, arr_ty, lhs_ptr, idx
                    ));
                    let rhs_elem_ptr = Self::map_key_tmp("rhs_elem_ptr", counter);
                    lines.push(format!(
                        "  {} = getelementptr {}, {}* {}, i32 0, i64 {}",
                        rhs_elem_ptr, arr_ty, arr_ty, rhs_ptr, idx
                    ));
                    let eq = self.emit_map_key_eq_from_ptr(
                        inner,
                        &lhs_elem_ptr,
                        &rhs_elem_ptr,
                        lines,
                        counter,
                    )?;
                    acc = Self::combine_i1_and(lines, counter, acc, eq);
                }
                Ok(acc)
            }
            Type::Tuple(items) => {
                let tuple_ty = llvm_storage_type(ty)?;
                let mut acc = "1".to_string();
                for (idx, item_ty) in items.iter().enumerate() {
                    let lhs_item_ptr = Self::map_key_tmp("lhs_item_ptr", counter);
                    lines.push(format!(
                        "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                        lhs_item_ptr, tuple_ty, tuple_ty, lhs_ptr, idx
                    ));
                    let rhs_item_ptr = Self::map_key_tmp("rhs_item_ptr", counter);
                    lines.push(format!(
                        "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                        rhs_item_ptr, tuple_ty, tuple_ty, rhs_ptr, idx
                    ));
                    let eq = self.emit_map_key_eq_from_ptr(
                        item_ty,
                        &lhs_item_ptr,
                        &rhs_item_ptr,
                        lines,
                        counter,
                    )?;
                    acc = Self::combine_i1_and(lines, counter, acc, eq);
                }
                Ok(acc)
            }
            Type::Result(ok_ty, err_ty) => {
                let result_ty = llvm_storage_type(ty)?;
                let mut acc = "1".to_string();
                let tag_ty = Type::Builtin(BuiltinType::U8);

                let lhs_tag_ptr = Self::map_key_tmp("lhs_tag_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 0",
                    lhs_tag_ptr, result_ty, result_ty, lhs_ptr
                ));
                let rhs_tag_ptr = Self::map_key_tmp("rhs_tag_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 0",
                    rhs_tag_ptr, result_ty, result_ty, rhs_ptr
                ));
                let tag_eq = self.emit_map_key_eq_from_ptr(
                    &tag_ty,
                    &lhs_tag_ptr,
                    &rhs_tag_ptr,
                    lines,
                    counter,
                )?;
                acc = Self::combine_i1_and(lines, counter, acc, tag_eq);

                let lhs_ok_ptr = Self::map_key_tmp("lhs_ok_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 1",
                    lhs_ok_ptr, result_ty, result_ty, lhs_ptr
                ));
                let rhs_ok_ptr = Self::map_key_tmp("rhs_ok_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 1",
                    rhs_ok_ptr, result_ty, result_ty, rhs_ptr
                ));
                let ok_eq =
                    self.emit_map_key_eq_from_ptr(ok_ty, &lhs_ok_ptr, &rhs_ok_ptr, lines, counter)?;
                acc = Self::combine_i1_and(lines, counter, acc, ok_eq);

                let lhs_err_ptr = Self::map_key_tmp("lhs_err_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 2",
                    lhs_err_ptr, result_ty, result_ty, lhs_ptr
                ));
                let rhs_err_ptr = Self::map_key_tmp("rhs_err_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 2",
                    rhs_err_ptr, result_ty, result_ty, rhs_ptr
                ));
                let err_eq = self.emit_map_key_eq_from_ptr(
                    err_ty,
                    &lhs_err_ptr,
                    &rhs_err_ptr,
                    lines,
                    counter,
                )?;
                acc = Self::combine_i1_and(lines, counter, acc, err_eq);
                Ok(acc)
            }
            Type::Named(name) => {
                if let Some(alias) = self.types.get_alias(name) {
                    return self.emit_map_key_eq_from_ptr(alias, lhs_ptr, rhs_ptr, lines, counter);
                }
                match self.types.get(name) {
                    Some(TypeDefKind::Struct(def)) => {
                        let named_ty = format!("%{}", name);
                        let mut acc = "1".to_string();
                        for (idx, field) in def.fields.iter().enumerate() {
                            let lhs_field_ptr = Self::map_key_tmp("lhs_field_ptr", counter);
                            lines.push(format!(
                                "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                                lhs_field_ptr, named_ty, named_ty, lhs_ptr, idx
                            ));
                            let rhs_field_ptr = Self::map_key_tmp("rhs_field_ptr", counter);
                            lines.push(format!(
                                "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                                rhs_field_ptr, named_ty, named_ty, rhs_ptr, idx
                            ));
                            let eq = self.emit_map_key_eq_from_ptr(
                                &field.ty,
                                &lhs_field_ptr,
                                &rhs_field_ptr,
                                lines,
                                counter,
                            )?;
                            acc = Self::combine_i1_and(lines, counter, acc, eq);
                        }
                        Ok(acc)
                    }
                    Some(TypeDefKind::Enum(def)) => {
                        if def.variants.iter().all(|(_, fields)| fields.is_empty()) {
                            let tag_ty = self.map_key_enum_tag_ty(name)?;
                            let named_ty = format!("%{}", name);
                            let lhs_tag_ptr = Self::map_key_tmp("lhs_tag_ptr", counter);
                            lines.push(format!(
                                "  {} = getelementptr {}, {}* {}, i32 0, i32 0",
                                lhs_tag_ptr, named_ty, named_ty, lhs_ptr
                            ));
                            let rhs_tag_ptr = Self::map_key_tmp("rhs_tag_ptr", counter);
                            lines.push(format!(
                                "  {} = getelementptr {}, {}* {}, i32 0, i32 0",
                                rhs_tag_ptr, named_ty, named_ty, rhs_ptr
                            ));
                            self.emit_map_key_eq_from_ptr(
                                &tag_ty,
                                &lhs_tag_ptr,
                                &rhs_tag_ptr,
                                lines,
                                counter,
                            )
                        } else {
                            self.emit_map_key_enum_eq_from_ptr(
                                name, lhs_ptr, rhs_ptr, lines, counter,
                            )
                        }
                    }
                    _ => self.invariant_err("map key callbacks support aggregate scalar keys only"),
                }
            }
            _ => self.invariant_err("map key callbacks support aggregate scalar keys only"),
        }
    }

    fn emit_map_key_hash_from_ptr(
        &self,
        ty: &Type,
        ptr: &str,
        lines: &mut Vec<String>,
        counter: &mut usize,
    ) -> Result<String, String> {
        match ty {
            Type::Builtin(BuiltinType::Bool)
            | Type::Builtin(BuiltinType::I8)
            | Type::Builtin(BuiltinType::I16)
            | Type::Builtin(BuiltinType::I32)
            | Type::Builtin(BuiltinType::I64)
            | Type::Builtin(BuiltinType::Isize)
            | Type::Builtin(BuiltinType::U8)
            | Type::Builtin(BuiltinType::U16)
            | Type::Builtin(BuiltinType::U32)
            | Type::Builtin(BuiltinType::U64)
            | Type::Builtin(BuiltinType::Usize)
            | Type::Builtin(BuiltinType::Char)
            | Type::Builtin(BuiltinType::Error) => {
                let storage_ty = llvm_storage_type(ty)?;
                let llvm_ty = llvm_type(ty)?;
                let loaded = Self::map_key_tmp("hashv", counter);
                lines.push(format!(
                    "  {} = load {}, {}* {}",
                    loaded, llvm_ty, storage_ty, ptr
                ));
                if matches!(ty, Type::Builtin(BuiltinType::Bool)) {
                    let out = Self::map_key_tmp("hashbool", counter);
                    lines.push(format!("  {} = zext i1 {} to i64", out, loaded));
                    return Ok(out);
                }
                if matches!(ty, Type::Builtin(BuiltinType::Error)) {
                    let out = Self::map_key_tmp("hasherr", counter);
                    lines.push(format!("  {} = sext i32 {} to i64", out, loaded));
                    return Ok(out);
                }
                let Some((signed, bits)) = self.int_info(ty) else {
                    return self
                        .invariant_err("map key callbacks support aggregate scalar keys only");
                };
                if bits == 64 {
                    return Ok(loaded);
                }
                let out = Self::map_key_tmp("hashint", counter);
                if signed {
                    lines.push(format!("  {} = sext {} {} to i64", out, llvm_ty, loaded));
                } else {
                    lines.push(format!("  {} = zext {} {} to i64", out, llvm_ty, loaded));
                }
                Ok(out)
            }
            Type::Builtin(BuiltinType::String) => {
                let storage_ty = llvm_storage_type(ty)?;
                let loaded = Self::map_key_tmp("hashv", counter);
                lines.push(format!(
                    "  {} = load %string, {}* {}",
                    loaded, storage_ty, ptr
                ));
                let ptr_part = Self::map_key_tmp("hash_ptr", counter);
                lines.push(format!(
                    "  {} = extractvalue %string {}, 0",
                    ptr_part, loaded
                ));
                let len_part = Self::map_key_tmp("hash_len", counter);
                lines.push(format!(
                    "  {} = extractvalue %string {}, 1",
                    len_part, loaded
                ));
                let out = Self::map_key_tmp("hash_str", counter);
                lines.push(format!(
                    "  {} = call i64 @__gost_map_hash_bytes(i8* {}, i64 {})",
                    out, ptr_part, len_part
                ));
                Ok(out)
            }
            Type::Builtin(BuiltinType::F32) => {
                let storage_ty = llvm_storage_type(ty)?;
                let loaded = Self::map_key_tmp("hashv", counter);
                lines.push(format!(
                    "  {} = load float, {}* {}",
                    loaded, storage_ty, ptr
                ));
                let bits = Self::map_key_tmp("hashbits", counter);
                lines.push(format!("  {} = bitcast float {} to i32", bits, loaded));
                let out = Self::map_key_tmp("hashint", counter);
                lines.push(format!("  {} = zext i32 {} to i64", out, bits));
                Ok(out)
            }
            Type::Builtin(BuiltinType::F64) => {
                let storage_ty = llvm_storage_type(ty)?;
                let loaded = Self::map_key_tmp("hashv", counter);
                lines.push(format!(
                    "  {} = load double, {}* {}",
                    loaded, storage_ty, ptr
                ));
                let out = Self::map_key_tmp("hashbits", counter);
                lines.push(format!("  {} = bitcast double {} to i64", out, loaded));
                Ok(out)
            }
            Type::FnPtr { .. } => {
                let storage_ty = llvm_storage_type(ty)?;
                let llvm_ty = llvm_type(ty)?;
                let loaded = Self::map_key_tmp("hashv", counter);
                lines.push(format!(
                    "  {} = load {}, {}* {}",
                    loaded, llvm_ty, storage_ty, ptr
                ));
                let out = Self::map_key_tmp("hashptr", counter);
                lines.push(format!(
                    "  {} = ptrtoint {} {} to i64",
                    out, llvm_ty, loaded
                ));
                Ok(out)
            }
            Type::Alias(_) | Type::Shared(_) => {
                let storage_ty = llvm_storage_type(ty)?;
                let loaded = Self::map_key_tmp("hashv", counter);
                lines.push(format!(
                    "  {} = load %shared, {}* {}",
                    loaded, storage_ty, ptr
                ));
                let obj = Self::map_key_tmp("hash_obj", counter);
                lines.push(format!("  {} = extractvalue %shared {}, 0", obj, loaded));
                let out = Self::map_key_tmp("hashptr", counter);
                lines.push(format!("  {} = ptrtoint %shared_obj* {} to i64", out, obj));
                Ok(out)
            }
            Type::Chan(_) => {
                let storage_ty = llvm_storage_type(ty)?;
                let llvm_ty = llvm_type(ty)?;
                let loaded = Self::map_key_tmp("hashv", counter);
                lines.push(format!(
                    "  {} = load {}, {}* {}",
                    loaded, llvm_ty, storage_ty, ptr
                ));
                let out = Self::map_key_tmp("hashptr", counter);
                lines.push(format!(
                    "  {} = ptrtoint {} {} to i64",
                    out, llvm_ty, loaded
                ));
                Ok(out)
            }
            Type::Array(inner, len) => {
                let arr_ty = llvm_storage_type(ty)?;
                let mut acc = "1469598103934665603".to_string();
                for idx in 0..*len {
                    let elem_ptr = Self::map_key_tmp("hash_elem_ptr", counter);
                    lines.push(format!(
                        "  {} = getelementptr {}, {}* {}, i32 0, i64 {}",
                        elem_ptr, arr_ty, arr_ty, ptr, idx
                    ));
                    let part = self.emit_map_key_hash_from_ptr(inner, &elem_ptr, lines, counter)?;
                    acc = Self::mix_hash_ir(lines, counter, acc, part);
                }
                Ok(acc)
            }
            Type::Tuple(items) => {
                let tuple_ty = llvm_storage_type(ty)?;
                let mut acc = "1469598103934665603".to_string();
                for (idx, item_ty) in items.iter().enumerate() {
                    let item_ptr = Self::map_key_tmp("hash_item_ptr", counter);
                    lines.push(format!(
                        "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                        item_ptr, tuple_ty, tuple_ty, ptr, idx
                    ));
                    let part =
                        self.emit_map_key_hash_from_ptr(item_ty, &item_ptr, lines, counter)?;
                    acc = Self::mix_hash_ir(lines, counter, acc, part);
                }
                Ok(acc)
            }
            Type::Result(ok_ty, err_ty) => {
                let result_ty = llvm_storage_type(ty)?;
                let mut acc = "1469598103934665603".to_string();
                let tag_ty = Type::Builtin(BuiltinType::U8);

                let tag_ptr = Self::map_key_tmp("hash_tag_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 0",
                    tag_ptr, result_ty, result_ty, ptr
                ));
                let tag_part =
                    self.emit_map_key_hash_from_ptr(&tag_ty, &tag_ptr, lines, counter)?;
                acc = Self::mix_hash_ir(lines, counter, acc, tag_part);

                let ok_ptr = Self::map_key_tmp("hash_ok_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 1",
                    ok_ptr, result_ty, result_ty, ptr
                ));
                let ok_part = self.emit_map_key_hash_from_ptr(ok_ty, &ok_ptr, lines, counter)?;
                acc = Self::mix_hash_ir(lines, counter, acc, ok_part);

                let err_ptr = Self::map_key_tmp("hash_err_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 2",
                    err_ptr, result_ty, result_ty, ptr
                ));
                let err_part = self.emit_map_key_hash_from_ptr(err_ty, &err_ptr, lines, counter)?;
                acc = Self::mix_hash_ir(lines, counter, acc, err_part);
                Ok(acc)
            }
            Type::Named(name) => {
                if let Some(alias) = self.types.get_alias(name) {
                    return self.emit_map_key_hash_from_ptr(alias, ptr, lines, counter);
                }
                match self.types.get(name) {
                    Some(TypeDefKind::Struct(def)) => {
                        let named_ty = format!("%{}", name);
                        let mut acc = "1469598103934665603".to_string();
                        for (idx, field) in def.fields.iter().enumerate() {
                            let field_ptr = Self::map_key_tmp("hash_field_ptr", counter);
                            lines.push(format!(
                                "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                                field_ptr, named_ty, named_ty, ptr, idx
                            ));
                            let part = self.emit_map_key_hash_from_ptr(
                                &field.ty, &field_ptr, lines, counter,
                            )?;
                            acc = Self::mix_hash_ir(lines, counter, acc, part);
                        }
                        Ok(acc)
                    }
                    Some(TypeDefKind::Enum(def)) => {
                        if def.variants.iter().all(|(_, fields)| fields.is_empty()) {
                            let tag_ty = self.map_key_enum_tag_ty(name)?;
                            let named_ty = format!("%{}", name);
                            let tag_ptr = Self::map_key_tmp("hash_tag_ptr", counter);
                            lines.push(format!(
                                "  {} = getelementptr {}, {}* {}, i32 0, i32 0",
                                tag_ptr, named_ty, named_ty, ptr
                            ));
                            self.emit_map_key_hash_from_ptr(&tag_ty, &tag_ptr, lines, counter)
                        } else {
                            self.emit_map_key_enum_hash_from_ptr(name, ptr, lines, counter)
                        }
                    }
                    _ => self.invariant_err("map key callbacks support aggregate scalar keys only"),
                }
            }
            _ => self.invariant_err("map key callbacks support aggregate scalar keys only"),
        }
    }

    fn emit_map_key_clone_from_ptr(
        &self,
        ty: &Type,
        dst_ptr: &str,
        src_ptr: &str,
        lines: &mut Vec<String>,
        counter: &mut usize,
    ) -> Result<(), String> {
        match ty {
            Type::Builtin(BuiltinType::Bool)
            | Type::Builtin(BuiltinType::I8)
            | Type::Builtin(BuiltinType::I16)
            | Type::Builtin(BuiltinType::I32)
            | Type::Builtin(BuiltinType::I64)
            | Type::Builtin(BuiltinType::Isize)
            | Type::Builtin(BuiltinType::U8)
            | Type::Builtin(BuiltinType::U16)
            | Type::Builtin(BuiltinType::U32)
            | Type::Builtin(BuiltinType::U64)
            | Type::Builtin(BuiltinType::Usize)
            | Type::Builtin(BuiltinType::Char)
            | Type::Builtin(BuiltinType::Error)
            | Type::Builtin(BuiltinType::String)
            | Type::Builtin(BuiltinType::F32)
            | Type::Builtin(BuiltinType::F64)
            | Type::FnPtr { .. } => {
                let storage_ty = llvm_storage_type(ty)?;
                let llvm_ty = llvm_type(ty)?;
                let loaded = Self::map_key_tmp("clonev", counter);
                lines.push(format!(
                    "  {} = load {}, {}* {}",
                    loaded, llvm_ty, storage_ty, src_ptr
                ));
                lines.push(format!(
                    "  store {} {}, {}* {}",
                    llvm_ty, loaded, storage_ty, dst_ptr
                ));
                Ok(())
            }
            Type::Alias(_) | Type::Shared(_) => {
                let storage_ty = llvm_storage_type(ty)?;
                let loaded = Self::map_key_tmp("clonev", counter);
                lines.push(format!(
                    "  {} = load %shared, {}* {}",
                    loaded, storage_ty, src_ptr
                ));
                lines.push(format!(
                    "  store %shared {}, {}* {}",
                    loaded, storage_ty, dst_ptr
                ));
                let obj = Self::map_key_tmp("clone_obj", counter);
                lines.push(format!("  {} = extractvalue %shared {}, 0", obj, loaded));
                lines.push(format!(
                    "  call void @__gost_shared_inc(%shared_obj* {})",
                    obj
                ));
                Ok(())
            }
            Type::Chan(_) => {
                let storage_ty = llvm_storage_type(ty)?;
                let llvm_ty = llvm_type(ty)?;
                let loaded = Self::map_key_tmp("clonev", counter);
                lines.push(format!(
                    "  {} = load {}, {}* {}",
                    loaded, llvm_ty, storage_ty, src_ptr
                ));
                lines.push(format!(
                    "  store {} {}, {}* {}",
                    llvm_ty, loaded, storage_ty, dst_ptr
                ));
                lines.push(format!(
                    "  call void @__gost_chan_retain(%chan* {})",
                    loaded
                ));
                Ok(())
            }
            Type::Array(inner, len) => {
                let arr_ty = llvm_storage_type(ty)?;
                for idx in 0..*len {
                    let dst_elem_ptr = Self::map_key_tmp("clone_dst_elem_ptr", counter);
                    lines.push(format!(
                        "  {} = getelementptr {}, {}* {}, i32 0, i64 {}",
                        dst_elem_ptr, arr_ty, arr_ty, dst_ptr, idx
                    ));
                    let src_elem_ptr = Self::map_key_tmp("clone_src_elem_ptr", counter);
                    lines.push(format!(
                        "  {} = getelementptr {}, {}* {}, i32 0, i64 {}",
                        src_elem_ptr, arr_ty, arr_ty, src_ptr, idx
                    ));
                    self.emit_map_key_clone_from_ptr(
                        inner,
                        &dst_elem_ptr,
                        &src_elem_ptr,
                        lines,
                        counter,
                    )?;
                }
                Ok(())
            }
            Type::Tuple(items) => {
                let tuple_ty = llvm_storage_type(ty)?;
                for (idx, item_ty) in items.iter().enumerate() {
                    let dst_item_ptr = Self::map_key_tmp("clone_dst_item_ptr", counter);
                    lines.push(format!(
                        "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                        dst_item_ptr, tuple_ty, tuple_ty, dst_ptr, idx
                    ));
                    let src_item_ptr = Self::map_key_tmp("clone_src_item_ptr", counter);
                    lines.push(format!(
                        "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                        src_item_ptr, tuple_ty, tuple_ty, src_ptr, idx
                    ));
                    self.emit_map_key_clone_from_ptr(
                        item_ty,
                        &dst_item_ptr,
                        &src_item_ptr,
                        lines,
                        counter,
                    )?;
                }
                Ok(())
            }
            Type::Result(ok_ty, err_ty) => {
                let result_ty = llvm_storage_type(ty)?;
                let tag_ty = Type::Builtin(BuiltinType::U8);

                let dst_tag_ptr = Self::map_key_tmp("clone_dst_tag_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 0",
                    dst_tag_ptr, result_ty, result_ty, dst_ptr
                ));
                let src_tag_ptr = Self::map_key_tmp("clone_src_tag_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 0",
                    src_tag_ptr, result_ty, result_ty, src_ptr
                ));
                self.emit_map_key_clone_from_ptr(
                    &tag_ty,
                    &dst_tag_ptr,
                    &src_tag_ptr,
                    lines,
                    counter,
                )?;

                let dst_ok_ptr = Self::map_key_tmp("clone_dst_ok_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 1",
                    dst_ok_ptr, result_ty, result_ty, dst_ptr
                ));
                let src_ok_ptr = Self::map_key_tmp("clone_src_ok_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 1",
                    src_ok_ptr, result_ty, result_ty, src_ptr
                ));
                self.emit_map_key_clone_from_ptr(ok_ty, &dst_ok_ptr, &src_ok_ptr, lines, counter)?;

                let dst_err_ptr = Self::map_key_tmp("clone_dst_err_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 2",
                    dst_err_ptr, result_ty, result_ty, dst_ptr
                ));
                let src_err_ptr = Self::map_key_tmp("clone_src_err_ptr", counter);
                lines.push(format!(
                    "  {} = getelementptr {}, {}* {}, i32 0, i32 2",
                    src_err_ptr, result_ty, result_ty, src_ptr
                ));
                self.emit_map_key_clone_from_ptr(
                    err_ty,
                    &dst_err_ptr,
                    &src_err_ptr,
                    lines,
                    counter,
                )?;
                Ok(())
            }
            Type::Named(name) => {
                if let Some(alias) = self.types.get_alias(name) {
                    return self
                        .emit_map_key_clone_from_ptr(alias, dst_ptr, src_ptr, lines, counter);
                }
                match self.types.get(name) {
                    Some(TypeDefKind::Struct(def)) => {
                        let named_ty = format!("%{}", name);
                        for (idx, field) in def.fields.iter().enumerate() {
                            let dst_field_ptr = Self::map_key_tmp("clone_dst_field_ptr", counter);
                            lines.push(format!(
                                "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                                dst_field_ptr, named_ty, named_ty, dst_ptr, idx
                            ));
                            let src_field_ptr = Self::map_key_tmp("clone_src_field_ptr", counter);
                            lines.push(format!(
                                "  {} = getelementptr {}, {}* {}, i32 0, i32 {}",
                                src_field_ptr, named_ty, named_ty, src_ptr, idx
                            ));
                            self.emit_map_key_clone_from_ptr(
                                &field.ty,
                                &dst_field_ptr,
                                &src_field_ptr,
                                lines,
                                counter,
                            )?;
                        }
                        Ok(())
                    }
                    Some(TypeDefKind::Enum(def)) => {
                        if def.variants.iter().all(|(_, fields)| fields.is_empty()) {
                            let tag_ty = self.map_key_enum_tag_ty(name)?;
                            let named_ty = format!("%{}", name);
                            let dst_tag_ptr = Self::map_key_tmp("clone_dst_tag_ptr", counter);
                            lines.push(format!(
                                "  {} = getelementptr {}, {}* {}, i32 0, i32 0",
                                dst_tag_ptr, named_ty, named_ty, dst_ptr
                            ));
                            let src_tag_ptr = Self::map_key_tmp("clone_src_tag_ptr", counter);
                            lines.push(format!(
                                "  {} = getelementptr {}, {}* {}, i32 0, i32 0",
                                src_tag_ptr, named_ty, named_ty, src_ptr
                            ));
                            self.emit_map_key_clone_from_ptr(
                                &tag_ty,
                                &dst_tag_ptr,
                                &src_tag_ptr,
                                lines,
                                counter,
                            )
                        } else {
                            self.emit_map_key_enum_clone_from_ptr(
                                name, dst_ptr, src_ptr, lines, counter,
                            )
                        }
                    }
                    _ => self.invariant_err("map key callbacks support aggregate scalar keys only"),
                }
            }
            _ => self.invariant_err("map key callbacks support aggregate scalar keys only"),
        }
    }

    fn next_map_key_ops_prefix(&mut self) -> String {
        let mut sanitized = String::with_capacity(self.fn_name.len());
        for ch in self.fn_name.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                sanitized.push(ch);
            } else {
                sanitized.push('_');
            }
        }
        let prefix = format!(
            "__gost_map_keyop_{}_{}",
            sanitized, self.map_key_ops_counter
        );
        self.map_key_ops_counter += 1;
        prefix
    }

    fn ensure_map_key_ops(&mut self, key_ty: &Type) -> Result<MapKeyOps, String> {
        if let Some(existing) = self.map_key_ops_cache.get(key_ty).cloned() {
            return Ok(existing);
        }
        let prefix = self.next_map_key_ops_prefix();
        let eq_fn = format!("{}_eq", prefix);
        let hash_fn = format!("{}_hash", prefix);
        let clone_fn = format!("{}_clone", prefix);
        let drop_fn = format!("{}_drop", prefix);

        let storage_ty = llvm_storage_type(key_ty)?;

        let mut eq_lines = Vec::new();
        eq_lines.push(format!("define i32 @{}(i8* %lhs, i8* %rhs) {{", eq_fn));
        eq_lines.push("entry:".to_string());
        eq_lines.push(format!("  %lhs_ptr = bitcast i8* %lhs to {}*", storage_ty));
        eq_lines.push(format!("  %rhs_ptr = bitcast i8* %rhs to {}*", storage_ty));
        let mut eq_counter = 0usize;
        let eq_i1 = self.emit_map_key_eq_from_ptr(
            key_ty,
            "%lhs_ptr",
            "%rhs_ptr",
            &mut eq_lines,
            &mut eq_counter,
        )?;
        if eq_i1 == "1" {
            eq_lines.push("  ret i32 1".to_string());
        } else if eq_i1 == "0" {
            eq_lines.push("  ret i32 0".to_string());
        } else {
            let eq_i32 = Self::map_key_tmp("eq_i32", &mut eq_counter);
            eq_lines.push(format!("  {} = zext i1 {} to i32", eq_i32, eq_i1));
            eq_lines.push(format!("  ret i32 {}", eq_i32));
        }
        eq_lines.push("}".to_string());
        self.extra_functions.push(eq_lines.join("\n"));

        let mut hash_lines = Vec::new();
        hash_lines.push(format!("define i64 @{}(i8* %key) {{", hash_fn));
        hash_lines.push("entry:".to_string());
        hash_lines.push(format!("  %key_ptr = bitcast i8* %key to {}*", storage_ty));
        let mut hash_counter = 0usize;
        let hash = self.emit_map_key_hash_from_ptr(
            key_ty,
            "%key_ptr",
            &mut hash_lines,
            &mut hash_counter,
        )?;
        hash_lines.push(format!("  ret i64 {}", hash));
        hash_lines.push("}".to_string());
        self.extra_functions.push(hash_lines.join("\n"));

        let mut clone_lines = Vec::new();
        clone_lines.push(format!("define void @{}(i8* %dst, i8* %src) {{", clone_fn));
        clone_lines.push("entry:".to_string());
        clone_lines.push(format!("  %dst_ptr = bitcast i8* %dst to {}*", storage_ty));
        clone_lines.push(format!("  %src_ptr = bitcast i8* %src to {}*", storage_ty));
        let mut clone_counter = 0usize;
        self.emit_map_key_clone_from_ptr(
            key_ty,
            "%dst_ptr",
            "%src_ptr",
            &mut clone_lines,
            &mut clone_counter,
        )?;
        clone_lines.push("  ret void".to_string());
        clone_lines.push("}".to_string());
        self.extra_functions.push(clone_lines.join("\n"));

        let mut drop_lines = Vec::new();
        let mut drop_counter = 0usize;
        let mut next_drop_tmp = || {
            let name = format!("%kdrop{}", drop_counter);
            drop_counter += 1;
            name
        };
        drop_lines.push(format!("define void @{}(i8* %ptr) {{", drop_fn));
        drop_lines.push("entry:".to_string());
        self.emit_drop_impl(key_ty, "%ptr", &mut drop_lines, &mut next_drop_tmp)?;
        drop_lines.push("  ret void".to_string());
        drop_lines.push("}".to_string());
        self.extra_functions.push(drop_lines.join("\n"));

        let ops = MapKeyOps {
            eq_fn,
            hash_fn,
            clone_fn,
            drop_fn,
        };
        self.map_key_ops_cache.insert(key_ty.clone(), ops.clone());
        Ok(ops)
    }

    fn emit_map_key_enum_tag_value(
        &mut self,
        key_ty: &Type,
        key_val: &Value,
    ) -> Result<Option<Value>, String> {
        let enum_name = match key_ty {
            Type::Named(name) => name,
            _ => return Ok(None),
        };
        if !matches!(self.types.get(enum_name), Some(TypeDefKind::Enum(_))) {
            return Ok(None);
        }
        let tag_ty = self.map_key_enum_tag_ty(enum_name)?;
        let tag_ir = self.new_temp();
        self.emit(format!(
            "{} = extractvalue %{} {}, 0",
            tag_ir, enum_name, key_val.ir
        ));
        Ok(Some(Value {
            ty: tag_ty,
            ir: tag_ir,
        }))
    }

    fn emit_map_key_ptr(&mut self, key_ty: &Type, key_val: Value) -> Result<String, String> {
        let key_spec = self
            .types
            .map_key_spec(key_ty)
            .ok_or_else(|| Self::invariant_violation("missing runtime map key storage type"))?;
        let runtime_key_ty = key_spec.runtime_ty;
        let normalized = if key_val.ty == runtime_key_ty {
            key_val
        } else if key_val.ty == Type::Builtin(BuiltinType::Bool)
            && self.is_int_type(&runtime_key_ty)
        {
            let as_u64 = self.new_temp();
            self.emit(format!("{} = zext i1 {} to i64", as_u64, key_val.ir));
            let widened = Value {
                ty: Type::Builtin(BuiltinType::U64),
                ir: as_u64,
            };
            if widened.ty == runtime_key_ty {
                widened
            } else {
                self.cast_int_value(widened, &runtime_key_ty)?
            }
        } else if let Some(enum_tag) = self.emit_map_key_enum_tag_value(key_ty, &key_val)? {
            if enum_tag.ty == runtime_key_ty {
                enum_tag
            } else {
                self.cast_int_value(enum_tag, &runtime_key_ty)?
            }
        } else {
            self.cast_value(key_val, &runtime_key_ty)?
        };
        let key_alloca = self.new_temp();
        let key_storage = llvm_storage_type(&runtime_key_ty)?;
        self.emit(format!("{} = alloca {}", key_alloca, key_storage));
        self.emit(format!(
            "store {} {}, {}* {}",
            llvm_type(&runtime_key_ty)?,
            normalized.ir,
            key_storage,
            key_alloca
        ));
        let key_ptr = self.new_temp();
        self.emit(format!(
            "{} = bitcast {}* {} to i8*",
            key_ptr, key_storage, key_alloca
        ));
        Ok(key_ptr)
    }

    fn emit_map_key_from_runtime(
        &mut self,
        key_ty: &Type,
        runtime_val: Value,
    ) -> Result<Value, String> {
        if &runtime_val.ty == key_ty {
            return Ok(runtime_val);
        }
        if *key_ty == Type::Builtin(BuiltinType::Bool) && self.is_int_type(&runtime_val.ty) {
            let cmp = self.new_temp();
            self.emit(format!(
                "{} = icmp ne {} {}, 0",
                cmp,
                llvm_type(&runtime_val.ty)?,
                runtime_val.ir
            ));
            return Ok(Value {
                ty: Type::Builtin(BuiltinType::Bool),
                ir: cmp,
            });
        }
        if let Type::Named(enum_name) = key_ty
            && let Some(TypeDefKind::Enum(def)) = self.types.get(enum_name)
            && def.variants.iter().all(|(_, fields)| fields.is_empty())
        {
            let tag_ty = self.map_key_enum_tag_ty(enum_name)?;
            let tag_val = if runtime_val.ty == tag_ty {
                runtime_val
            } else {
                self.cast_int_value(runtime_val, &tag_ty)?
            };
            let enum_ir = self.new_temp();
            self.emit(format!(
                "{} = insertvalue %{} undef, {} {}, 0",
                enum_ir,
                enum_name,
                llvm_type(&tag_ty)?,
                tag_val.ir
            ));
            return Ok(Value {
                ty: key_ty.clone(),
                ir: enum_ir,
            });
        }
        self.cast_value(runtime_val, key_ty)
    }

    fn cast_int_value(&mut self, val: Value, target: &Type) -> Result<Value, String> {
        if &val.ty == target {
            return Ok(val);
        }
        let (src_signed, src_bits) = self
            .int_info(&val.ty)
            .ok_or_else(|| Self::invariant_violation("unsupported integer cast"))?;
        let (_dst_signed, dst_bits) = self
            .int_info(target)
            .ok_or_else(|| Self::invariant_violation("unsupported integer cast"))?;
        if src_bits == dst_bits {
            return Ok(Value {
                ty: target.clone(),
                ir: val.ir,
            });
        }
        let tmp = self.new_temp();
        let src_llvm = llvm_type(&val.ty)?;
        let dst_llvm = llvm_type(target)?;
        if src_bits < dst_bits {
            let op = if src_signed { "sext" } else { "zext" };
            self.emit(format!(
                "{} = {} {} {} to {}",
                tmp, op, src_llvm, val.ir, dst_llvm
            ));
        } else {
            self.emit(format!(
                "{} = trunc {} {} to {}",
                tmp, src_llvm, val.ir, dst_llvm
            ));
        }
        Ok(Value {
            ty: target.clone(),
            ir: tmp,
        })
    }

    fn emit_compound_assign_value(
        &mut self,
        op: &crate::frontend::ast::AssignOp,
        target_ty: &Type,
        target_ptr: &str,
        rhs_value: Value,
    ) -> Result<Value, String> {
        let lhs = if *target_ty == Type::Builtin(BuiltinType::Unit) {
            Value {
                ty: target_ty.clone(),
                ir: String::new(),
            }
        } else {
            let loaded = self.new_temp();
            self.emit(format!(
                "{} = load {}, {}* {}",
                loaded,
                llvm_type(target_ty)?,
                llvm_storage_type(target_ty)?,
                target_ptr
            ));
            Value {
                ty: target_ty.clone(),
                ir: loaded,
            }
        };
        let mut rhs = rhs_value;
        if lhs.ty != rhs.ty && self.is_int_type(&lhs.ty) && self.is_int_type(&rhs.ty) {
            rhs = self.cast_int_value(rhs, &lhs.ty)?;
        } else if lhs.ty != rhs.ty && is_float_type(&lhs.ty) && is_float_type(&rhs.ty) {
            rhs = self.cast_value(rhs, &lhs.ty)?;
        }
        let tmp = self.new_temp();
        let instr = match op {
            crate::frontend::ast::AssignOp::AddAssign => {
                if is_float_type(&lhs.ty) {
                    self.emit(format!(
                        "{} = fadd {} {}, {}",
                        tmp,
                        llvm_type(&lhs.ty)?,
                        lhs.ir,
                        rhs.ir
                    ));
                    None
                } else {
                    Some("add")
                }
            }
            crate::frontend::ast::AssignOp::SubAssign => {
                if is_float_type(&lhs.ty) {
                    self.emit(format!(
                        "{} = fsub {} {}, {}",
                        tmp,
                        llvm_type(&lhs.ty)?,
                        lhs.ir,
                        rhs.ir
                    ));
                    None
                } else {
                    Some("sub")
                }
            }
            crate::frontend::ast::AssignOp::MulAssign => {
                if is_float_type(&lhs.ty) {
                    self.emit(format!(
                        "{} = fmul {} {}, {}",
                        tmp,
                        llvm_type(&lhs.ty)?,
                        lhs.ir,
                        rhs.ir
                    ));
                    None
                } else {
                    Some("mul")
                }
            }
            crate::frontend::ast::AssignOp::DivAssign => {
                if is_float_type(&lhs.ty) {
                    self.emit(format!(
                        "{} = fdiv {} {}, {}",
                        tmp,
                        llvm_type(&lhs.ty)?,
                        lhs.ir,
                        rhs.ir
                    ));
                    None
                } else if self.is_signed_int_type(&lhs.ty) {
                    Some("sdiv")
                } else {
                    Some("udiv")
                }
            }
            crate::frontend::ast::AssignOp::RemAssign => {
                if is_float_type(&lhs.ty) {
                    self.emit(format!(
                        "{} = frem {} {}, {}",
                        tmp,
                        llvm_type(&lhs.ty)?,
                        lhs.ir,
                        rhs.ir
                    ));
                    None
                } else if self.is_signed_int_type(&lhs.ty) {
                    Some("srem")
                } else {
                    Some("urem")
                }
            }
            crate::frontend::ast::AssignOp::BitAndAssign => Some("and"),
            crate::frontend::ast::AssignOp::BitOrAssign => Some("or"),
            crate::frontend::ast::AssignOp::BitXorAssign => Some("xor"),
            crate::frontend::ast::AssignOp::ShlAssign => Some("shl"),
            crate::frontend::ast::AssignOp::ShrAssign => {
                if self.is_signed_int_type(&lhs.ty) {
                    Some("ashr")
                } else {
                    Some("lshr")
                }
            }
            crate::frontend::ast::AssignOp::Assign => {
                return self.invariant_err("internal error: expected compound assignment op");
            }
        };
        if let Some(instr) = instr {
            self.emit(format!(
                "{} = {} {} {}, {}",
                tmp,
                instr,
                llvm_type(&lhs.ty)?,
                lhs.ir,
                rhs.ir
            ));
        }
        Ok(Value {
            ty: lhs.ty,
            ir: tmp,
        })
    }

    fn float_bits(&self, ty: &Type) -> Option<u32> {
        match ty {
            Type::Builtin(BuiltinType::F32) => Some(32),
            Type::Builtin(BuiltinType::F64) => Some(64),
            _ => None,
        }
    }

    fn is_interface_object_type(ty: &Type) -> bool {
        matches!(ty, Type::Interface | Type::Closure { .. })
    }

    fn cast_value(&mut self, val: Value, target: &Type) -> Result<Value, String> {
        if &val.ty == target {
            return Ok(val);
        }
        let target_interface_like = Self::is_interface_object_type(target);
        let value_interface_like = Self::is_interface_object_type(&val.ty);
        if target_interface_like {
            if value_interface_like {
                let mut out = val;
                out.ty = target.clone();
                return Ok(out);
            }
            let mut boxed = self.emit_interface_box_value(val)?;
            boxed.ty = target.clone();
            return Ok(boxed);
        }
        if value_interface_like {
            let iface = if val.ty == Type::Interface {
                val
            } else {
                Value {
                    ty: Type::Interface,
                    ir: val.ir,
                }
            };
            return self.emit_interface_unbox_value(iface, target);
        }
        if self.is_int_type(&val.ty) && self.is_int_type(target) {
            return self.cast_int_value(val, target);
        }
        if self.is_int_type(&val.ty) && is_float_type(target) {
            let tmp = self.new_temp();
            let op = if self.is_signed_int_type(&val.ty) {
                "sitofp"
            } else {
                "uitofp"
            };
            self.emit(format!(
                "{} = {} {} {} to {}",
                tmp,
                op,
                llvm_type(&val.ty)?,
                val.ir,
                llvm_type(target)?
            ));
            return Ok(Value {
                ty: target.clone(),
                ir: tmp,
            });
        }
        if is_float_type(&val.ty) && self.is_int_type(target) {
            let tmp = self.new_temp();
            let op = if self.is_signed_int_type(target) {
                "fptosi"
            } else {
                "fptoui"
            };
            self.emit(format!(
                "{} = {} {} {} to {}",
                tmp,
                op,
                llvm_type(&val.ty)?,
                val.ir,
                llvm_type(target)?
            ));
            return Ok(Value {
                ty: target.clone(),
                ir: tmp,
            });
        }
        if is_float_type(&val.ty) && is_float_type(target) {
            let src_bits = self
                .float_bits(&val.ty)
                .ok_or_else(|| Self::invariant_violation("unsupported float cast"))?;
            let dst_bits = self
                .float_bits(target)
                .ok_or_else(|| Self::invariant_violation("unsupported float cast"))?;
            if src_bits == dst_bits {
                return Ok(Value {
                    ty: target.clone(),
                    ir: val.ir,
                });
            }
            let tmp = self.new_temp();
            if src_bits < dst_bits {
                self.emit(format!(
                    "{} = fpext {} {} to {}",
                    tmp,
                    llvm_type(&val.ty)?,
                    val.ir,
                    llvm_type(target)?
                ));
            } else {
                self.emit(format!(
                    "{} = fptrunc {} {} to {}",
                    tmp,
                    llvm_type(&val.ty)?,
                    val.ir,
                    llvm_type(target)?
                ));
            }
            return Ok(Value {
                ty: target.clone(),
                ir: tmp,
            });
        }
        self.invariant_err("unsupported cast")
    }

    fn emit_interface_method_call(
        &mut self,
        iface: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, String> {
        let mut dispatch = Vec::<(String, FunctionSig)>::new();
        for (symbol, sig) in self.fn_sigs.iter() {
            if logical_method_name(symbol) != method {
                continue;
            }
            if sig.params.is_empty() {
                continue;
            }
            if sig.is_variadic {
                continue;
            }
            if sig.params.len().saturating_sub(1) != args.len() {
                continue;
            }
            let mut compatible = true;
            for (idx, arg) in args.iter().enumerate() {
                if sig.params.get(idx + 1) != Some(&arg.ty) {
                    compatible = false;
                    break;
                }
            }
            if compatible {
                dispatch.push((symbol.clone(), sig.clone()));
            }
        }
        self.require_not_invariant_fmt(
            dispatch.is_empty(),
            format!("unknown method `{}`", method),
        )?;
        dispatch.sort_by(|a, b| {
            let a_iface = a.1.params.first() == Some(&Type::Interface);
            let b_iface = b.1.params.first() == Some(&Type::Interface);
            a_iface.cmp(&b_iface).then_with(|| a.0.cmp(&b.0))
        });
        let iface_fallback_count = dispatch
            .iter()
            .filter(|(_, sig)| sig.params.first() == Some(&Type::Interface))
            .count();
        self.require_not_invariant_fmt(
            iface_fallback_count > 1,
            format!("ambiguous interface fallback for method `{}`", method),
        )?;
        let canonical = dispatch[0].1.clone();
        for (_, sig) in dispatch.iter().skip(1) {
            self.require_not_invariant_fmt(
                sig.params.len() != canonical.params.len() || sig.ret != canonical.ret,
                format!(
                    "incompatible interface method implementations for `{}`",
                    method
                ),
            )?;
            for idx in 1..sig.params.len() {
                self.require_not_invariant_fmt(
                    sig.params[idx] != canonical.params[idx],
                    format!(
                        "incompatible interface method implementations for `{}`",
                        method
                    ),
                )?;
            }
        }
        let ret_ty = canonical.ret.clone();
        let tag_ir = self.new_temp();
        self.emit(format!(
            "{} = extractvalue %interface {}, 0",
            tag_ir, iface.ir
        ));
        let join_name = self.new_block("iface_method_join");
        let join_idx = self.add_block(join_name.clone());
        let mut incoming = Vec::<(String, String)>::new();
        for (idx, (symbol, sig)) in dispatch.iter().enumerate() {
            let recv_ty = sig.params.first().cloned().ok_or_else(|| {
                Self::invariant_violation(&format!(
                    "method `{}` requires a receiver parameter",
                    symbol
                ))
            })?;
            let call_name = self.new_block("iface_method_call");
            let call_idx = self.add_block(call_name.clone());
            let next_name = if idx + 1 == dispatch.len() {
                self.new_block("iface_method_fail")
            } else {
                self.new_block("iface_method_next")
            };
            let next_idx = self.add_block(next_name.clone());

            if recv_ty == Type::Interface {
                self.terminate(format!("br label %{}", call_name));
            } else {
                let expected_tag = self.interface_type_tag(&recv_ty);
                let is_match = self.new_temp();
                self.emit(format!(
                    "{} = icmp eq i64 {}, {}",
                    is_match, tag_ir, expected_tag
                ));
                self.terminate(format!(
                    "br i1 {}, label %{}, label %{}",
                    is_match, call_name, next_name
                ));
            }

            self.switch_to(call_idx);
            let mut all_args = Vec::with_capacity(args.len() + 1);
            if recv_ty == Type::Interface {
                all_args.push(iface.clone());
            } else {
                all_args.push(self.emit_interface_unbox_value_unchecked(&iface, &recv_ty)?);
            }
            all_args.extend(args.iter().cloned());
            let ret = self.emit_call_by_name(symbol, &[], all_args)?;
            let from_block = self.blocks[self.current].name.clone();
            self.terminate(format!("br label %{}", join_name));
            if ret_ty != Type::Builtin(BuiltinType::Unit) {
                incoming.push((ret.ir, from_block));
            }

            self.switch_to(next_idx);
            if idx + 1 == dispatch.len() {
                self.emit_panic_str("interface method dispatch failed")?;
            }
        }

        self.switch_to(join_idx);
        if ret_ty == Type::Builtin(BuiltinType::Unit) {
            return Ok(Value {
                ty: ret_ty,
                ir: String::new(),
            });
        }
        let phi = self.new_temp();
        let mut incoming_ir = Vec::new();
        for (val, block) in incoming {
            incoming_ir.push(format!("[ {}, %{} ]", val, block));
        }
        self.emit(format!(
            "{} = phi {} {}",
            phi,
            llvm_type(&ret_ty)?,
            incoming_ir.join(", ")
        ));
        Ok(Value {
            ty: ret_ty,
            ir: phi,
        })
    }

    fn emit_interface_box_value(&mut self, val: Value) -> Result<Value, String> {
        let tag = self.interface_type_tag(&val.ty);
        let data_ptr = if val.ty == Type::Builtin(BuiltinType::Unit) {
            "null".to_string()
        } else {
            let size = self.emit_size_of(&val.ty)?;
            let raw = self.new_temp();
            self.emit(format!(
                "{} = call i8* @__gost_alloc(i64 {}, i64 1)",
                raw, size.ir
            ));
            let storage_ty = llvm_storage_type(&val.ty)?;
            let cast = self.new_temp();
            self.emit(format!("{} = bitcast i8* {} to {}*", cast, raw, storage_ty));
            if matches!(val.ty, Type::Alias(_) | Type::Shared(_) | Type::Chan(_)) {
                self.emit_shared_inc_value(&val)?;
            }
            self.emit(format!(
                "store {} {}, {}* {}",
                llvm_type(&val.ty)?,
                val.ir,
                storage_ty,
                cast
            ));
            raw
        };
        let i0 = self.new_temp();
        self.emit(format!(
            "{} = insertvalue %interface undef, i64 {}, 0",
            i0, tag
        ));
        let i1 = self.new_temp();
        self.emit(format!(
            "{} = insertvalue %interface {}, i8* {}, 1",
            i1, i0, data_ptr
        ));
        Ok(Value {
            ty: Type::Interface,
            ir: i1,
        })
    }

    fn emit_interface_unbox_value(&mut self, iface: Value, target: &Type) -> Result<Value, String> {
        self.require_not_invariant(
            !Self::is_interface_object_type(&iface.ty),
            "interface cast source must be interface-like",
        )?;
        let tag_ir = self.new_temp();
        self.emit(format!(
            "{} = extractvalue %interface {}, 0",
            tag_ir, iface.ir
        ));
        let expected_tag = self.interface_type_tag(target);
        let ok = self.new_temp();
        self.emit(format!("{} = icmp eq i64 {}, {}", ok, tag_ir, expected_tag));
        let ok_name = self.new_block("iface_cast_ok");
        let fail_name = self.new_block("iface_cast_fail");
        let ok_idx = self.add_block(ok_name.clone());
        let fail_idx = self.add_block(fail_name.clone());
        self.terminate(format!(
            "br i1 {}, label %{}, label %{}",
            ok, ok_name, fail_name
        ));
        self.switch_to(fail_idx);
        self.emit_panic_str("interface type assertion failed")?;
        self.switch_to(ok_idx);
        self.emit_interface_unbox_value_unchecked(&iface, target)
    }

    fn emit_interface_unbox_value_unchecked(
        &mut self,
        iface: &Value,
        target: &Type,
    ) -> Result<Value, String> {
        self.require_not_invariant(
            !Self::is_interface_object_type(&iface.ty),
            "interface cast source must be interface-like",
        )?;
        if *target == Type::Builtin(BuiltinType::Unit) {
            return Ok(Value {
                ty: target.clone(),
                ir: String::new(),
            });
        }
        let data_ir = self.new_temp();
        self.emit(format!(
            "{} = extractvalue %interface {}, 1",
            data_ir, iface.ir
        ));
        let storage_ty = llvm_storage_type(target)?;
        let cast_ptr = self.new_temp();
        self.emit(format!(
            "{} = bitcast i8* {} to {}*",
            cast_ptr, data_ir, storage_ty
        ));
        let loaded = self.new_temp();
        self.emit(format!(
            "{} = load {}, {}* {}",
            loaded,
            llvm_type(target)?,
            storage_ty,
            cast_ptr
        ));
        Ok(Value {
            ty: target.clone(),
            ir: loaded,
        })
    }

    fn interface_type_tag(&self, ty: &Type) -> u64 {
        fn write_type(out: &mut String, ty: &Type) {
            match ty {
                Type::Builtin(b) => {
                    out.push('b');
                    out.push_str(match b {
                        BuiltinType::Bool => "bool",
                        BuiltinType::I8 => "i8",
                        BuiltinType::I16 => "i16",
                        BuiltinType::I32 => "i32",
                        BuiltinType::I64 => "i64",
                        BuiltinType::Isize => "isize",
                        BuiltinType::U8 => "u8",
                        BuiltinType::U16 => "u16",
                        BuiltinType::U32 => "u32",
                        BuiltinType::U64 => "u64",
                        BuiltinType::Usize => "usize",
                        BuiltinType::F32 => "f32",
                        BuiltinType::F64 => "f64",
                        BuiltinType::Char => "char",
                        BuiltinType::Unit => "unit",
                        BuiltinType::String => "string",
                        BuiltinType::Error => "error",
                        BuiltinType::Bytes => "bytes",
                    });
                }
                Type::Named(name) => {
                    out.push('n');
                    out.push_str(name);
                }
                Type::FnPtr {
                    params,
                    ret,
                    is_variadic,
                } => {
                    out.push_str("fn(");
                    for (idx, p) in params.iter().enumerate() {
                        if idx > 0 {
                            out.push(',');
                        }
                        write_type(out, p);
                    }
                    if *is_variadic {
                        if !params.is_empty() {
                            out.push(',');
                        }
                        out.push_str("...");
                    }
                    out.push(')');
                    out.push_str("->");
                    write_type(out, ret);
                }
                Type::Closure {
                    params,
                    ret,
                    is_variadic,
                } => {
                    out.push_str("closure(");
                    for (idx, p) in params.iter().enumerate() {
                        if idx > 0 {
                            out.push(',');
                        }
                        write_type(out, p);
                    }
                    if *is_variadic {
                        if !params.is_empty() {
                            out.push(',');
                        }
                        out.push_str("...");
                    }
                    out.push(')');
                    out.push_str("->");
                    write_type(out, ret);
                }
                Type::Ref(inner) => {
                    out.push('&');
                    write_type(out, inner);
                }
                Type::MutRef(inner) => {
                    out.push_str("&mut");
                    write_type(out, inner);
                }
                Type::Slice(inner) => {
                    out.push_str("[]");
                    write_type(out, inner);
                }
                Type::Array(inner, len) => {
                    out.push('[');
                    out.push_str(&len.to_string());
                    out.push(']');
                    write_type(out, inner);
                }
                Type::Map(k, v) => {
                    out.push_str("map[");
                    write_type(out, k);
                    out.push(']');
                    write_type(out, v);
                }
                Type::Result(ok, err) => {
                    out.push_str("result[");
                    write_type(out, ok);
                    out.push(',');
                    write_type(out, err);
                    out.push(']');
                }
                Type::Iter(inner) => {
                    out.push_str("iter[");
                    write_type(out, inner);
                    out.push(']');
                }
                Type::Chan(inner) => {
                    out.push_str("chan[");
                    write_type(out, inner);
                    out.push(']');
                }
                Type::Own(inner) => {
                    out.push_str("own[");
                    write_type(out, inner);
                    out.push(']');
                }
                Type::Alias(inner) => {
                    out.push_str("alias[");
                    write_type(out, inner);
                    out.push(']');
                }
                Type::Shared(inner) => {
                    out.push_str("shared[");
                    write_type(out, inner);
                    out.push(']');
                }
                Type::Interface => out.push_str("interface"),
                Type::Tuple(items) => {
                    out.push('(');
                    for (idx, item) in items.iter().enumerate() {
                        if idx > 0 {
                            out.push(',');
                        }
                        write_type(out, item);
                    }
                    out.push(')');
                }
            }
        }

        let mut s = String::new();
        write_type(&mut s, ty);
        let mut h = 1469598103934665603u64;
        for b in s.bytes() {
            h ^= b as u64;
            h = h.wrapping_mul(1099511628211);
        }
        if h == 0 { 1 } else { h }
    }

    fn coerce_int_pair(&mut self, lhs: Value, rhs: Value) -> Result<(Value, Value), String> {
        if lhs.ty == rhs.ty {
            return Ok((lhs, rhs));
        }
        let target = match self.promote_int_type(&lhs.ty, &rhs.ty) {
            Some(t) => t,
            None => return Ok((lhs, rhs)),
        };
        let lhs2 = self.cast_int_value(lhs, &target)?;
        let rhs2 = self.cast_int_value(rhs, &target)?;
        Ok((lhs2, rhs2))
    }

    fn emit_index_i64(&mut self, idx_val: &Value) -> Result<String, String> {
        if idx_val.ty == Type::Builtin(BuiltinType::I64) {
            return Ok(idx_val.ir.clone());
        }
        self.require_not_invariant(!self.is_int_type(&idx_val.ty), "index expects integer type")?;
        let casted = self.cast_int_value(
            Value {
                ty: idx_val.ty.clone(),
                ir: idx_val.ir.clone(),
            },
            &Type::Builtin(BuiltinType::I64),
        )?;
        Ok(casted.ir)
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

    fn emit_array_elem_ptr(
        &mut self,
        array_ptr: &str,
        elem_ty: &Type,
        len: usize,
        idx_val: &Value,
        bounds_check: bool,
    ) -> Result<String, String> {
        let idx_ir = self.emit_index_i64(idx_val)?;
        if bounds_check {
            let in_bounds = self.new_temp();
            self.emit(format!("{} = icmp ult i64 {}, {}", in_bounds, idx_ir, len));
            let ok_name = self.new_block("arr_idx_ok");
            let fail_name = self.new_block("arr_idx_fail");
            let ok_idx = self.add_block(ok_name.clone());
            let fail_idx = self.add_block(fail_name.clone());
            self.terminate(format!(
                "br i1 {}, label %{}, label %{}",
                in_bounds, ok_name, fail_name
            ));
            self.switch_to(fail_idx);
            self.emit_panic_str("index out of bounds")?;
            self.switch_to(ok_idx);
        }
        let array_llvm = llvm_type(&Type::Array(Box::new(elem_ty.clone()), len))?;
        let elem_ptr = self.new_temp();
        self.emit(format!(
            "{} = getelementptr {}, {}* {}, i32 0, i64 {}",
            elem_ptr, array_llvm, array_llvm, array_ptr, idx_ir
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
        self.emit(format!("{} = ptrtoint {}* {} to i64", size, llvm_ty, ptr));
        Ok(Value {
            ty: Type::Builtin(BuiltinType::I64),
            ir: size,
        })
    }

    fn needs_drop(&self, ty: &Type) -> bool {
        let mut visiting = HashSet::new();
        self.needs_drop_inner(ty, &mut visiting)
    }

    fn needs_drop_inner(&self, ty: &Type, visiting: &mut HashSet<String>) -> bool {
        match ty {
            Type::Builtin(BuiltinType::Bytes) => true,
            Type::Slice(_)
            | Type::Map(_, _)
            | Type::Chan(_)
            | Type::Own(_)
            | Type::Alias(_)
            | Type::Shared(_)
            | Type::Interface => true,
            Type::Tuple(items) => items
                .iter()
                .any(|item| self.needs_drop_inner(item, visiting)),
            Type::Result(ok, err) => {
                self.needs_drop_inner(ok, visiting) || self.needs_drop_inner(err, visiting)
            }
            Type::Iter(_) => false,
            Type::Named(name) => {
                if visiting.contains(name) {
                    return false;
                }
                visiting.insert(name.clone());
                let needs = match self.types.get(name) {
                    Some(TypeDefKind::Struct(def)) => def
                        .fields
                        .iter()
                        .any(|field| self.needs_drop_inner(&field.ty, visiting)),
                    Some(TypeDefKind::Enum(def)) => def.variants.iter().any(|(_, fields)| {
                        fields
                            .iter()
                            .any(|field_ty| self.needs_drop_inner(field_ty, visiting))
                    }),
                    None => false,
                };
                visiting.remove(name);
                needs
            }
            _ => false,
        }
    }

    fn next_drop_fn_name(&mut self) -> String {
        let name = format!("__gost_drop_fn_{}_{}", self.fn_name, self.drop_counter);
        self.drop_counter += 1;
        name
    }

    fn emit_drop_fn_ptr(&mut self, ty: &Type) -> Result<String, String> {
        if !self.needs_drop(ty) {
            return Ok("null".to_string());
        }
        if let Some(existing) = self.drop_fn_cache.get(ty) {
            return Ok(existing.clone());
        }
        let name = format!("@{}", self.next_drop_fn_name());
        self.drop_fn_cache.insert(ty.clone(), name.clone());
        self.drop_fn_in_progress.insert(ty.clone());
        self.emit_drop_function(name.trim_start_matches('@'), ty)?;
        self.drop_fn_in_progress.remove(ty);
        Ok(name)
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
                lines.push(format!("  {} = bitcast i8* {} to %slice*", cast, ptr));
                let val = next_tmp();
                lines.push(format!("  {} = load %slice, %slice* {}", val, cast));
                let obj = next_tmp();
                lines.push(format!("  {} = extractvalue %slice {}, 0", obj, val));
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
                lines.push(format!("  call void @__gost_map_drop(%map_obj* {})", obj));
            }
            Type::Chan(_) => {
                let cast = next_tmp();
                lines.push(format!("  {} = bitcast i8* {} to %chan**", cast, ptr));
                let val = next_tmp();
                lines.push(format!("  {} = load %chan*, %chan** {}", val, cast));
                lines.push(format!("  call void @__gost_chan_drop(%chan* {})", val));
            }
            Type::Own(_) | Type::Alias(_) | Type::Shared(_) => {
                let cast = next_tmp();
                lines.push(format!("  {} = bitcast i8* {} to %shared*", cast, ptr));
                let val = next_tmp();
                lines.push(format!("  {} = load %shared, %shared* {}", val, cast));
                let obj = next_tmp();
                lines.push(format!("  {} = extractvalue %shared {}, 0", obj, val));
                lines.push(format!(
                    "  call void @__gost_shared_dec(%shared_obj* {})",
                    obj
                ));
            }
            Type::Interface => {
                let cast = next_tmp();
                lines.push(format!("  {} = bitcast i8* {} to %interface*", cast, ptr));
                let val = next_tmp();
                lines.push(format!("  {} = load %interface, %interface* {}", val, cast));
                let data = next_tmp();
                lines.push(format!("  {} = extractvalue %interface {}, 1", data, val));
                let is_null = next_tmp();
                lines.push(format!("  {} = icmp eq i8* {}, null", is_null, data));
                let id = is_null.trim_start_matches('%');
                let free_label = format!("drop_iface_free_{}", id);
                let end_label = format!("drop_iface_end_{}", id);
                lines.push(format!(
                    "  br i1 {}, label %{}, label %{}",
                    is_null, end_label, free_label
                ));
                lines.push(format!("{}:", free_label));
                lines.push(format!(
                    "  call void @__gost_free(i8* {}, i64 0, i64 1)",
                    data
                ));
                lines.push(format!("  br label %{}", end_label));
                lines.push(format!("{}:", end_label));
            }
            Type::Result(ok_ty, err_ty) => {
                let storage_ty = llvm_type(ty)?;
                let cast = next_tmp();
                lines.push(format!(
                    "  {} = bitcast i8* {} to {}*",
                    cast, ptr, storage_ty
                ));
                let val = next_tmp();
                lines.push(format!(
                    "  {} = load {}, {}* {}",
                    val, storage_ty, storage_ty, cast
                ));
                let tag = next_tmp();
                lines.push(format!(
                    "  {} = extractvalue {} {}, 0",
                    tag, storage_ty, val
                ));
                let id = next_tmp();
                let id = id.trim_start_matches('%');
                let ok_label = format!("drop_res_ok_{}", id);
                let err_label = format!("drop_res_err_{}", id);
                let end_label = format!("drop_res_end_{}", id);
                lines.push(format!(
                    "  switch i8 {}, label %{} [ i8 0, label %{} i8 1, label %{} ]",
                    tag, end_label, ok_label, err_label
                ));
                lines.push(format!("{}:", ok_label));
                if self.needs_drop(ok_ty) {
                    let field_ptr = next_tmp();
                    lines.push(format!(
                        "  {} = getelementptr {}, {}* {}, i32 0, i32 1",
                        field_ptr, storage_ty, storage_ty, cast
                    ));
                    let drop_fn = self.emit_drop_fn_ptr(ok_ty)?;
                    if drop_fn != "null" {
                        let cast_ptr = next_tmp();
                        lines.push(format!(
                            "  {} = bitcast {}* {} to i8*",
                            cast_ptr,
                            llvm_type_for_tuple_elem(ok_ty)?,
                            field_ptr
                        ));
                        lines.push(format!("  call void {}(i8* {})", drop_fn, cast_ptr));
                    }
                }
                lines.push(format!("  br label %{}", end_label));
                lines.push(format!("{}:", err_label));
                if self.needs_drop(err_ty) {
                    let field_ptr = next_tmp();
                    lines.push(format!(
                        "  {} = getelementptr {}, {}* {}, i32 0, i32 2",
                        field_ptr, storage_ty, storage_ty, cast
                    ));
                    let drop_fn = self.emit_drop_fn_ptr(err_ty)?;
                    if drop_fn != "null" {
                        let cast_ptr = next_tmp();
                        lines.push(format!(
                            "  {} = bitcast {}* {} to i8*",
                            cast_ptr,
                            llvm_type_for_tuple_elem(err_ty)?,
                            field_ptr
                        ));
                        lines.push(format!("  call void {}(i8* {})", drop_fn, cast_ptr));
                    }
                }
                lines.push(format!("  br label %{}", end_label));
                lines.push(format!("{}:", end_label));
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
                        lines.push(format!("  call void {}(i8* {})", drop_fn, cast_ptr));
                    }
                }
            }
            Type::Named(name) => match self.types.get(name) {
                Some(TypeDefKind::Struct(def)) => {
                    let cast = next_tmp();
                    lines.push(format!("  {} = bitcast i8* {} to %{}*", cast, ptr, name));
                    for (idx, field) in def.fields.iter().enumerate() {
                        if !self.needs_drop(&field.ty) {
                            continue;
                        }
                        let field_ptr = next_tmp();
                        lines.push(format!(
                            "  {} = getelementptr %{}, %{}* {}, i32 0, i32 {}",
                            field_ptr, name, name, cast, idx
                        ));
                        let drop_fn = self.emit_drop_fn_ptr(&field.ty)?;
                        if drop_fn != "null" {
                            let cast_ptr = next_tmp();
                            lines.push(format!(
                                "  {} = bitcast {}* {} to i8*",
                                cast_ptr,
                                llvm_storage_type(&field.ty)?,
                                field_ptr
                            ));
                            lines.push(format!("  call void {}(i8* {})", drop_fn, cast_ptr));
                        }
                    }
                }
                Some(TypeDefKind::Enum(def)) => {
                    let tag_only = def.variants.iter().all(|(_, f)| f.is_empty());
                    if tag_only {
                        // Fieldless enums do not own payload allocations.
                        return Ok(());
                    }
                    let cast = next_tmp();
                    lines.push(format!("  {} = bitcast i8* {} to %{}*", cast, ptr, name));
                    let val = next_tmp();
                    lines.push(format!("  {} = load %{}, %{}* {}", val, name, name, cast));
                    let tag = next_tmp();
                    lines.push(format!("  {} = extractvalue %{} {}, 0", tag, name, val));
                    let payload = next_tmp();
                    lines.push(format!("  {} = extractvalue %{} {}, 1", payload, name, val));
                    let end_label = format!("drop_end_{}", name);
                    let tag_ty = self.enum_tag_llvm_type(name)?;
                    let mut cases = Vec::new();
                    for (idx, _) in def.variants.iter().enumerate() {
                        cases.push(format!(
                            "{} {}, label %drop_case{}_{}",
                            tag_ty, idx, name, idx
                        ));
                    }
                    lines.push(format!(
                        "  switch {} {}, label %{} [ {} ]",
                        tag_ty,
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
                                    lines
                                        .push(format!("  call void {}(i8* {})", drop_fn, cast_ptr));
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
                if let Some((ty, ptr)) = self.locals.get(name).cloned() {
                    Ok((ty, ptr))
                } else if let Some(global) = self.globals.get(name) {
                    Ok((global.ty.clone(), format!("@{}", name)))
                } else if let Some(global) = self.extern_globals.get(name) {
                    Ok((global.ty.clone(), format!("@{}", name)))
                } else {
                    self.invariant_err_fmt(format!("unknown local {}", name))
                }
            }
            ExprKind::Deref { expr: inner } => {
                let inner_val = self.emit_expr(inner)?;
                match inner_val.ty {
                    Type::Ref(inner) | Type::MutRef(inner) => Ok((*inner, inner_val.ir)),
                    _ => self.invariant_err("deref expects ref type"),
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
                let (struct_name, struct_ptr) = match &base_ty {
                    Type::Named(name) => (name.clone(), base_ptr),
                    Type::Ref(inner) | Type::MutRef(inner) => match &**inner {
                        Type::Named(name) => {
                            let ref_ptr = self.new_temp();
                            let ref_storage_ty = llvm_storage_type(&base_ty)?;
                            self.emit(format!(
                                "{} = load {}, {}* {}",
                                ref_ptr, ref_storage_ty, ref_storage_ty, base_ptr
                            ));
                            (name.clone(), ref_ptr)
                        }
                        _ => return self.invariant_err("field access expects struct"),
                    },
                    _ => return self.invariant_err("field access expects struct"),
                };
                let (field_idx, field_ty) = self.resolve_struct_field(&struct_name, name)?;
                let gep = self.new_temp();
                self.emit(format!(
                    "{} = getelementptr %{}, %{}* {}, i32 0, i32 {}",
                    gep, struct_name, struct_name, struct_ptr, field_idx
                ));
                Ok((field_ty, gep))
            }
            ExprKind::Index { base, index } => {
                let idx_val = self.emit_expr(index)?;
                if let Ok((base_ty, base_ptr)) = self.emit_place_ptr(base)
                    && let Type::Array(inner, len) = base_ty
                {
                    let elem_ty = *inner;
                    let elem_ptr =
                        self.emit_array_elem_ptr(&base_ptr, &elem_ty, len, &idx_val, true)?;
                    return Ok((elem_ty, elem_ptr));
                }
                let base_val = self.emit_expr(base)?;
                match &base_val.ty {
                    Type::Slice(inner) => {
                        let elem_ty = *inner.clone();
                        let slice_obj = self.emit_slice_obj_from_value(&base_val)?;
                        let elem_ptr =
                            self.emit_slice_elem_ptr(&slice_obj, &elem_ty, &idx_val, true)?;
                        Ok((elem_ty, elem_ptr))
                    }
                    Type::Builtin(BuiltinType::Bytes) => {
                        let elem_ty = Type::Builtin(BuiltinType::U32);
                        let slice_obj = self.emit_slice_obj_from_value(&base_val)?;
                        let elem_ptr =
                            self.emit_slice_elem_ptr(&slice_obj, &elem_ty, &idx_val, true)?;
                        Ok((elem_ty, elem_ptr))
                    }
                    Type::Array(inner, len) => {
                        let elem_ty = *inner.clone();
                        let array_ptr = self.materialize_value(&base_val)?;
                        let elem_ptr =
                            self.emit_array_elem_ptr(&array_ptr, &elem_ty, *len, &idx_val, true)?;
                        Ok((elem_ty, elem_ptr))
                    }
                    Type::Ref(inner) | Type::MutRef(inner) => match &**inner {
                        Type::Slice(elem) => {
                            let elem_ty = *elem.clone();
                            let slice_obj = self.emit_slice_obj_from_value(&base_val)?;
                            let elem_ptr =
                                self.emit_slice_elem_ptr(&slice_obj, &elem_ty, &idx_val, true)?;
                            Ok((elem_ty, elem_ptr))
                        }
                        Type::Builtin(BuiltinType::Bytes) => {
                            let elem_ty = Type::Builtin(BuiltinType::U32);
                            let slice_obj = self.emit_slice_obj_from_value(&base_val)?;
                            let elem_ptr =
                                self.emit_slice_elem_ptr(&slice_obj, &elem_ty, &idx_val, true)?;
                            Ok((elem_ty, elem_ptr))
                        }
                        Type::Array(elem, len) => {
                            let elem_ty = *elem.clone();
                            let elem_ptr = self.emit_array_elem_ptr(
                                &base_val.ir,
                                &elem_ty,
                                *len,
                                &idx_val,
                                true,
                            )?;
                            Ok((elem_ty, elem_ptr))
                        }
                        _ => self.invariant_err("indexing expects a slice"),
                    },
                    _ => self.invariant_err("indexing expects a slice"),
                }
            }
            _ => self.invariant_err("expression is not addressable"),
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
                .ok_or_else(|| {
                    Self::invariant_violation(&format!(
                        "unknown variant `{}` on {}",
                        variant, enum_name
                    ))
                }),
            _ => self.invariant_err("enum constructor expects enum"),
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
        if self.enum_is_tag_only(enum_name)? {
            if payload_ptr.is_some() {
                return self
                    .invariant_err("tag-only enum representation cannot carry payload pointer");
            }
            let agg = self.new_temp();
            self.emit(format!(
                "{} = insertvalue {} undef, {} {}, 0",
                agg,
                llvm_type(&enum_ty)?,
                self.enum_tag_llvm_type(enum_name)?,
                tag
            ));
            return Ok(Value {
                ty: enum_ty,
                ir: agg,
            });
        }
        let agg0 = self.new_temp();
        let tag_ty = self.enum_tag_llvm_type(enum_name)?;
        self.emit(format!(
            "{} = insertvalue {} undef, {} {}, 0",
            agg0,
            llvm_type(&enum_ty)?,
            tag_ty,
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
        self.require_not_invariant(
            field_tys.len() != args.len(),
            "enum constructor arity mismatch",
        )?;
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
                    .ok_or_else(|| Self::invariant_violation("missing enum argument"))?;
                if matches!(field_ty, Type::Alias(_) | Type::Shared(_) | Type::Chan(_)) {
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

    fn emit_result_ctor(
        &mut self,
        expr: &Expr,
        variant: &str,
        type_args: &[Type],
        args: &[Expr],
    ) -> Result<Value, String> {
        self.require_not_invariant(args.len() != 1, "Result constructors expect one argument")?;
        let arg_val = self.emit_expr(&args[0])?;
        self.emit_shared_inc_value(&arg_val)?;
        let (ok_ty, err_ty) = match type_args.len() {
            2 => (type_args[0].clone(), type_args[1].clone()),
            0 => {
                if let Some(Type::Result(ok, err)) = self.mir_expr_type_override(expr) {
                    (ok.as_ref().clone(), err.as_ref().clone())
                } else {
                    match variant {
                        "Ok" | "ok" => (arg_val.ty.clone(), Type::Builtin(BuiltinType::Error)),
                        "Err" | "err" => (Type::Builtin(BuiltinType::Unit), arg_val.ty.clone()),
                        _ => return self.invariant_err("unknown Result variant"),
                    }
                }
            }
            _ => {
                return self.invariant_err("Result constructors expect two type arguments");
            }
        };
        let result_ty = Type::Result(Box::new(ok_ty.clone()), Box::new(err_ty.clone()));
        let llvm_result = llvm_type(&result_ty)?;
        let tag_val = match variant {
            "Ok" | "ok" => "0",
            "Err" | "err" => "1",
            _ => return self.invariant_err("unknown Result variant"),
        };
        let tag_tmp = self.new_temp();
        self.emit(format!(
            "{} = insertvalue {} undef, i8 {}, 0",
            tag_tmp, llvm_result, tag_val
        ));
        let ok_ir = if matches!(variant, "Ok" | "ok") {
            if arg_val.ty == Type::Builtin(BuiltinType::Unit) {
                "0".to_string()
            } else {
                arg_val.ir.clone()
            }
        } else if ok_ty == Type::Builtin(BuiltinType::Unit) {
            "0".to_string()
        } else {
            zero_value(&ok_ty)?
        };
        let ok_tmp = self.new_temp();
        self.emit(format!(
            "{} = insertvalue {} {}, {} {}, 1",
            ok_tmp,
            llvm_result,
            tag_tmp,
            llvm_type_for_tuple_elem(&ok_ty)?,
            ok_ir
        ));
        let err_ir = if matches!(variant, "Err" | "err") {
            if arg_val.ty == Type::Builtin(BuiltinType::Unit) {
                "0".to_string()
            } else {
                arg_val.ir.clone()
            }
        } else if err_ty == Type::Builtin(BuiltinType::Unit) {
            "0".to_string()
        } else {
            zero_value(&err_ty)?
        };
        let err_tmp = self.new_temp();
        self.emit(format!(
            "{} = insertvalue {} {}, {} {}, 2",
            err_tmp,
            llvm_result,
            ok_tmp,
            llvm_type_for_tuple_elem(&err_ty)?,
            err_ir
        ));
        Ok(Value {
            ty: result_ty,
            ir: err_tmp,
        })
    }

    fn resolve_struct_field(&self, type_name: &str, field: &str) -> Result<(usize, Type), String> {
        match self.types.get(type_name) {
            Some(TypeDefKind::Struct(def)) => def
                .fields
                .iter()
                .enumerate()
                .find(|(_, f)| f.name == field)
                .map(|(idx, f)| (idx, f.ty.clone()))
                .ok_or_else(|| {
                    Self::invariant_violation(&format!(
                        "unknown field `{}` on {}",
                        field, type_name
                    ))
                }),
            _ => self.invariant_err("field access expects struct"),
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
                } else if let Some(alias) = self.types.get_alias(name) {
                    Ok(alias.clone())
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
            crate::frontend::ast::TypeAstKind::Own(inner) => {
                Ok(Type::Own(Box::new(self.resolve_type_ast(inner)?)))
            }
            crate::frontend::ast::TypeAstKind::Alias(inner) => {
                Ok(Type::Alias(Box::new(self.resolve_type_ast(inner)?)))
            }
            crate::frontend::ast::TypeAstKind::Slice(inner) => {
                Ok(Type::Slice(Box::new(self.resolve_type_ast(inner)?)))
            }
            crate::frontend::ast::TypeAstKind::Array(inner, len) => {
                Ok(Type::Array(Box::new(self.resolve_type_ast(inner)?), *len))
            }
            crate::frontend::ast::TypeAstKind::Map(key, value) => Ok(Type::Map(
                Box::new(self.resolve_type_ast(key)?),
                Box::new(self.resolve_type_ast(value)?),
            )),
            crate::frontend::ast::TypeAstKind::Result(ok, err) => Ok(Type::Result(
                Box::new(self.resolve_type_ast(ok)?),
                Box::new(self.resolve_type_ast(err)?),
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
            crate::frontend::ast::TypeAstKind::FnPtr {
                params,
                ret,
                is_variadic,
            } => {
                let mut ptys = Vec::new();
                for p in params {
                    ptys.push(self.resolve_type_ast(p)?);
                }
                let rty = self.resolve_type_ast(ret)?;
                Ok(Type::FnPtr {
                    params: ptys,
                    ret: Box::new(rty),
                    is_variadic: *is_variadic,
                })
            }
            crate::frontend::ast::TypeAstKind::Closure {
                params,
                ret,
                is_variadic,
            } => {
                let mut ptys = Vec::new();
                for p in params {
                    ptys.push(self.resolve_type_ast(p)?);
                }
                let rty = self.resolve_type_ast(ret)?;
                Ok(Type::Closure {
                    params: ptys,
                    ret: Box::new(rty),
                    is_variadic: *is_variadic,
                })
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
        self.emit(format!(
            "{} = insertvalue %string undef, i8* {}, 0",
            tmp, ptr
        ));
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
                args.push(Value {
                    ty: ty.clone(),
                    ir: tmp,
                });
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
        if let Some(defers) = self.deferred.pop()
            && emit_runtime
        {
            self.emit_deferred_list(&defers)?;
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
