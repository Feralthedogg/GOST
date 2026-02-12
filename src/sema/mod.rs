pub mod types;

use std::collections::{HashMap, HashSet};

use crate::frontend::ast::*;
use crate::frontend::diagnostic::{
    Diagnostic, Diagnostics, E_UNDEFINED_NAME, E_UNKNOWN_FIELD, E_UNKNOWN_FUNCTION, E_UNKNOWN_TYPE,
    E_UNKNOWN_METHOD, E_UNKNOWN_VARIANT,
};
use crate::frontend::diagnostic::{E1104, E1105};
use crate::frontend::suggest;
use crate::sema::types::{
    builtin_from_name, builtin_names, BuiltinType, EnumDef, LayoutInfo, RefKind, StructDef, Type,
    TypeClass, TypeDefKind, TypeDefs,
};

#[derive(Clone, Debug)]
pub struct FunctionSig {
    pub params: Vec<Type>,
    pub ret: Type,
    pub is_variadic: bool,
    pub is_extern: bool,
    pub is_unsafe: bool,
    pub extern_abi: Option<String>,
}

#[derive(Clone, Debug)]
pub struct ExternGlobalSig {
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub file: FileAst,
    pub types: TypeDefs,
    pub functions: HashMap<String, FunctionSig>,
    pub extern_globals: HashMap<String, ExternGlobalSig>,
    pub expr_types: HashMap<ExprId, Type>,
}

pub fn analyze(
    file: &FileAst,
    std_funcs: &HashSet<String>,
) -> Result<Program, Diagnostics> {
    let mut diags = Diagnostics::default();
    let mut types = TypeDefs::default();
    let mut type_order = Vec::new();
    let mut expr_types: HashMap<ExprId, Type> = HashMap::new();
    for item in &file.items {
        match item {
            Item::Struct(def) => {
                if def.layout.bitfield && !def.layout.repr_c {
                    diags.push(
                        "bitfield requires repr(C)",
                        Some(def.span.clone()),
                    );
                };
                if def.layout.pack.is_some() && !def.layout.repr_c {
                    diags.push(
                        "pack(N) requires repr(C)",
                        Some(def.span.clone()),
                    );
                };
                types.insert(
                    def.name.clone(),
                    TypeDefKind::Struct(StructDef {
                        fields: Vec::new(),
                        is_copy: def.is_copy,
                        layout: lower_layout_attr(&def.layout),
                    }),
                );
                type_order.push(def.name.clone());
            }
            Item::Enum(def) => {
                if def.layout.pack.is_some() || def.layout.bitfield {
                    diags.push(
                        "pack/bitfield are only supported on struct",
                        Some(def.span.clone()),
                    );
                }
                types.insert(
                    def.name.clone(),
                    TypeDefKind::Enum(EnumDef {
                        variants: Vec::new(),
                        is_copy: def.is_copy,
                        layout: lower_layout_attr(&def.layout),
                    }),
                );
                type_order.push(def.name.clone());
            }
            _ => {}
        }
    }

    for item in &file.items {
        match item {
            Item::Struct(def) => {
                let mut fields = Vec::new();
                for field in &def.fields {
                    let ty = resolve_type(&field.ty, &types, &mut diags);
                    if let Some(ty) = ty {
                        if contains_view(&types, &ty) {
                            diags.push(
                                "view types cannot appear in struct fields",
                                Some(field.span.clone()),
                            );
                        }
                        if let Type::Map(_, ref value) = ty {
                            if !is_copy_type(&types, value) {
                                diags.push(
                                    "map value type must be Copy",
                                    Some(field.span.clone()),
                                );
                            }
                        }
                        if def.layout.bitfield {
                            if !matches!(
                                ty,
                                Type::Builtin(
                                    BuiltinType::Bool
                                        | BuiltinType::I32
                                        | BuiltinType::I64
                                        | BuiltinType::U32
                                        | BuiltinType::U64
                                        | BuiltinType::Char
                                )
                            ) {
                                diags.push(
                                    "bitfield struct fields must be integer-like types",
                                    Some(field.span.clone()),
                                );
                            }
                        }
                        fields.push((field.name.clone(), ty));
                    }
                }
                types.insert(
                    def.name.clone(),
                    TypeDefKind::Struct(StructDef {
                        fields,
                        is_copy: def.is_copy,
                        layout: lower_layout_attr(&def.layout),
                    }),
                );
            }
            Item::Enum(def) => {
                let mut variants = Vec::new();
                for var in &def.variants {
                    let mut field_tys = Vec::new();
                    for ty in &var.fields {
                        let resolved = resolve_type(ty, &types, &mut diags);
                        if let Some(resolved) = resolved {
                            if contains_view(&types, &resolved) {
                                diags.push(
                                    "view types cannot appear in enum variants",
                                    Some(var.span.clone()),
                                );
                            }
                            if let Type::Map(_, ref value) = resolved {
                                if !is_copy_type(&types, value) {
                                    diags.push(
                                        "map value type must be Copy",
                                        Some(var.span.clone()),
                                    );
                                }
                            }
                            field_tys.push(resolved);
                        }
                    }
                    variants.push((var.name.clone(), field_tys));
                }
                if def.layout.repr_c {
                    for (_, fields) in &variants {
                        if !fields.is_empty() {
                            diags.push(
                                "repr(C) enum currently supports only fieldless variants",
                                Some(def.span.clone()),
                            );
                            break;
                        }
                    }
                }
                types.insert(
                    def.name.clone(),
                    TypeDefKind::Enum(EnumDef {
                        variants,
                        is_copy: def.is_copy,
                        layout: lower_layout_attr(&def.layout),
                    }),
                );
            }
            _ => {}
        }
    }

    for name in &type_order {
        if let Some(def) = types.get(name) {
            match def {
                TypeDefKind::Struct(def) => {
                    if def.is_copy {
                        for (_, ty) in &def.fields {
                            if !is_copy_type(&types, ty) {
                                diags.push(
                                    "copy struct fields must be Copy",
                                    None,
                                );
                            }
                        }
                    }
                }
                TypeDefKind::Enum(def) => {
                    if def.is_copy {
                        for (_, fields) in &def.variants {
                            for ty in fields {
                                if !is_copy_type(&types, ty) {
                                    diags.push(
                                        "copy enum variants must be Copy",
                                        None,
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    check_recursive_types(&types, &mut diags);

    let mut functions = HashMap::new();
    let mut extern_globals: HashMap<String, ExternGlobalSig> = HashMap::new();
    for item in &file.items {
        match item {
            Item::Function(func) => {
                if func.is_extern {
                    let abi = func
                        .extern_abi
                        .clone()
                        .unwrap_or_else(|| "C".to_string());
                    if !is_supported_extern_abi(&abi) {
                        diags.push(
                            format!(
                                "unsupported extern ABI `{}` (supported: {})",
                                abi,
                                supported_extern_abis()
                            ),
                            Some(func.span.clone()),
                        );
                    }
                }
                if func.is_variadic && !func.is_extern {
                    diags.push(
                        "variadic functions are only supported for extern declarations",
                        Some(func.span.clone()),
                    );
                }
                let mut params = Vec::new();
                for param in &func.params {
                    if let Some(ty) = resolve_type(&param.ty, &types, &mut diags) {
                        params.push(ty);
                    }
                }
                let ret = if let Some(ret_ty) = &func.ret_type {
                    let ty = resolve_type(ret_ty, &types, &mut diags);
                    match ty {
                        Some(ty) => {
                            if contains_view(&types, &ty) {
                                diags.push(
                                    "view types cannot be function return types",
                                    Some(ret_ty.span.clone()),
                                );
                            }
                            ty
                        }
                        None => Type::Builtin(BuiltinType::Unit),
                    }
                } else {
                    Type::Builtin(BuiltinType::Unit)
                };
                if func.name.starts_with("__gost_") && !std_funcs.contains(&func.name) {
                    diags.push(
                        "reserved internal name",
                        Some(func.span.clone()),
                    );
                }
                functions.insert(
                    func.name.clone(),
                    FunctionSig {
                        params,
                        ret,
                        is_variadic: func.is_variadic,
                        is_extern: func.is_extern,
                        is_unsafe: func.is_unsafe,
                        extern_abi: func.extern_abi.clone(),
                    },
                );
            }
            Item::ExternGlobal(global) => {
                let abi = global
                    .extern_abi
                    .clone()
                    .unwrap_or_else(|| "C".to_string());
                if !is_supported_extern_abi(&abi) {
                    diags.push(
                        format!(
                            "unsupported extern ABI `{}` (supported: {})",
                            abi,
                            supported_extern_abis()
                        ),
                        Some(global.span.clone()),
                    );
                }
                if global.name.starts_with("__gost_") {
                    diags.push("reserved internal name", Some(global.span.clone()));
                }
                if let Some(ty) = resolve_type(&global.ty, &types, &mut diags) {
                    if contains_view(&types, &ty) {
                        diags.push(
                            "view types cannot be stored in extern globals",
                            Some(global.span.clone()),
                        );
                    }
                    extern_globals.insert(global.name.clone(), ExternGlobalSig { ty });
                }
            }
            _ => {}
        }
    }

    for item in &file.items {
        if let Item::Function(func) = item {
            if func.is_extern {
                continue;
            }
            let sig = functions.get(&func.name).cloned();
            if let Some(sig) = sig {
                let mut checker = FunctionChecker::new(
                    &types,
                    &functions,
                    &extern_globals,
                    &sig,
                    std_funcs.contains(&func.name),
                    &mut diags,
                    &mut expr_types,
                );
                checker.check_function(func);
            }
        }
    }

    if diags.is_empty() {
        Ok(Program {
            file: file.clone(),
            types,
            functions,
            extern_globals,
            expr_types,
        })
    } else {
        Err(diags)
    }
}

fn is_supported_extern_abi(abi: &str) -> bool {
    matches!(
        abi.to_ascii_lowercase().as_str(),
        "c"
            | "system"
            | "stdcall"
            | "fastcall"
            | "vectorcall"
            | "thiscall"
            | "win64"
            | "sysv64"
            | "aapcs"
    )
}

fn supported_extern_abis() -> &'static str {
    "\"C\", \"system\", \"stdcall\", \"fastcall\", \"vectorcall\", \"thiscall\", \"win64\", \"sysv64\", \"aapcs\""
}

fn lower_layout_attr(layout: &LayoutAttr) -> LayoutInfo {
    LayoutInfo {
        repr_c: layout.repr_c,
        pack: layout.pack,
        bitfield: layout.bitfield,
    }
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

fn resolve_type(ast: &TypeAst, defs: &TypeDefs, diags: &mut Diagnostics) -> Option<Type> {
    match &ast.kind {
        TypeAstKind::Named(name) => {
            if let Some(ty) = builtin_from_name(name) {
                Some(ty)
            } else {
                if defs.get(name).is_none() {
                    let mut candidates: Vec<String> =
                        builtin_names().iter().map(|s| s.to_string()).collect();
                    candidates.extend(defs.names());
                    let mut d = Diagnostic::new(
                        format!("unknown type `{}`", name),
                        Some(ast.span.clone()),
                    )
                    .code(E_UNKNOWN_TYPE)
                    .label(ast.span.clone(), "unknown type");
                    if let Some(h) = suggest::did_you_mean(name, candidates) {
                        d = d.help(h);
                    }
                    diags.push_diag(d);
                }
                Some(Type::Named(name.clone()))
            }
        }
        TypeAstKind::Ref(inner) => {
            let ty = resolve_type(inner, defs, diags)?;
            Some(Type::Ref(Box::new(ty)))
        }
        TypeAstKind::MutRef(inner) => {
            let ty = resolve_type(inner, defs, diags)?;
            Some(Type::MutRef(Box::new(ty)))
        }
        TypeAstKind::Slice(inner) => {
            let ty = resolve_type(inner, defs, diags)?;
            if contains_view(defs, &ty) {
                diags.push("view types cannot be stored in slices", Some(ast.span.clone()));
            }
            Some(Type::Slice(Box::new(ty)))
        }
        TypeAstKind::Map(key, value) => {
            let key_ty = resolve_type(key, defs, diags)?;
            let value_ty = resolve_type(value, defs, diags)?;
            if !is_map_key_type(&key_ty) {
                diags.push(
                    "map key type must be i64, u64, or string",
                    Some(ast.span.clone()),
                );
            }
            if contains_view(defs, &value_ty) {
                diags.push("view types cannot be stored in maps", Some(ast.span.clone()));
            }
            if !is_copy_type(defs, &value_ty) {
                diags.push("map value type must be Copy", Some(ast.span.clone()));
            }
            Some(Type::Map(Box::new(key_ty), Box::new(value_ty)))
        }
        TypeAstKind::Result(ok, err) => {
            let ok_ty = resolve_type(ok, defs, diags)?;
            let err_ty = resolve_type(err, defs, diags)?;
            if contains_view(defs, &ok_ty) || contains_view(defs, &err_ty) {
                diags.push("view types cannot be stored in Result", Some(ast.span.clone()));
            }
            Some(Type::Result(Box::new(ok_ty), Box::new(err_ty)))
        }
        TypeAstKind::Chan(inner) => {
            let ty = resolve_type(inner, defs, diags)?;
            if contains_view(defs, &ty) {
                diags.push("view types cannot be stored in channels", Some(ast.span.clone()));
            }
            Some(Type::Chan(Box::new(ty)))
        }
        TypeAstKind::Shared(inner) => {
            let ty = resolve_type(inner, defs, diags)?;
            if contains_view(defs, &ty) {
                diags.push("view types cannot be stored in shared", Some(ast.span.clone()));
            }
            Some(Type::Shared(Box::new(ty)))
        }
        TypeAstKind::Interface => Some(Type::Interface),
        TypeAstKind::Tuple(items) => {
            let mut tys = Vec::new();
            for item in items {
                let ty = resolve_type(item, defs, diags)?;
                tys.push(ty);
            }
            if tys.len() == 2 && tys[1] == Type::Builtin(BuiltinType::Error) {
                if contains_view(defs, &tys[0]) {
                    diags.push("view types cannot be stored in Result", Some(ast.span.clone()));
                }
                Some(Type::Result(Box::new(tys[0].clone()), Box::new(tys[1].clone())))
            } else {
                Some(Type::Tuple(tys))
            }
        }
        TypeAstKind::FnPtr {
            params,
            ret,
            is_variadic,
        } => {
            let mut ptys = Vec::new();
            for p in params {
                ptys.push(resolve_type(p, defs, diags)?);
            }
            let rty = resolve_type(ret, defs, diags)?;
            Some(Type::FnPtr {
                params: ptys,
                ret: Box::new(rty),
                is_variadic: *is_variadic,
            })
        }
    }
}

fn contains_view(defs: &TypeDefs, ty: &Type) -> bool {
    let mut visiting = HashSet::new();
    contains_view_inner(defs, ty, &mut visiting)
}

fn contains_view_inner(
    defs: &TypeDefs,
    ty: &Type,
    visiting: &mut HashSet<String>,
) -> bool {
    match ty {
        Type::Ref(_) | Type::MutRef(_) => true,
        Type::Slice(inner)
        | Type::Chan(inner)
        | Type::Shared(inner)
        | Type::Iter(inner) => contains_view_inner(defs, inner, visiting),
        Type::Map(key, value) => {
            contains_view_inner(defs, key, visiting)
                || contains_view_inner(defs, value, visiting)
        }
        Type::Tuple(items) => items.iter().any(|t| contains_view_inner(defs, t, visiting)),
        Type::Result(ok, err) => {
            contains_view_inner(defs, ok, visiting)
                || contains_view_inner(defs, err, visiting)
        }
        Type::Named(name) => {
            if visiting.contains(name) {
                return false;
            }
            visiting.insert(name.clone());
            let res = match defs.get(name) {
                Some(TypeDefKind::Struct(def)) => def
                    .fields
                    .iter()
                    .any(|(_, ty)| contains_view_inner(defs, ty, visiting)),
                Some(TypeDefKind::Enum(def)) => def.variants.iter().any(|(_, tys)| {
                    tys.iter()
                        .any(|ty| contains_view_inner(defs, ty, visiting))
                }),
                None => false,
            };
            visiting.remove(name);
            res
        }
        _ => false,
    }
}

fn view_arg_not_allowed(defs: &TypeDefs, arg_ty: &Type, param_ty: &Type) -> bool {
    contains_view(defs, arg_ty) && !contains_view(defs, param_ty)
}

fn slice_elem_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::Slice(inner) => Some(*inner.clone()),
        Type::Builtin(BuiltinType::Bytes) => Some(Type::Builtin(BuiltinType::U32)),
        _ => None,
    }
}

fn slice_like_elem_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::Ref(inner) | Type::MutRef(inner) => slice_elem_type(inner),
        _ => slice_elem_type(ty),
    }
}

fn is_copy_type(defs: &TypeDefs, ty: &Type) -> bool {
    matches!(defs.classify(ty), Some(TypeClass::Copy))
}

fn is_map_key_type(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Builtin(BuiltinType::I64)
            | Type::Builtin(BuiltinType::U64)
            | Type::Builtin(BuiltinType::String)
    )
}

fn check_recursive_types(defs: &TypeDefs, diags: &mut Diagnostics) {
    let mut visiting = HashSet::new();
    let mut visited = HashSet::new();
    for name in defs.names() {
        if visited.contains(&name) {
            continue;
        }
        if dfs_check_cycle(defs, &name, &mut visiting, &mut visited) {
            diags.push("recursive types are not supported yet", None);
        }
    }
}

fn dfs_check_cycle(
    defs: &TypeDefs,
    name: &str,
    visiting: &mut HashSet<String>,
    visited: &mut HashSet<String>,
) -> bool {
    if visiting.contains(name) {
        return true;
    }
    if visited.contains(name) {
        return false;
    }
    visiting.insert(name.to_string());
    let deps = type_deps(defs, name);
    for dep in deps {
        if dfs_check_cycle(defs, &dep, visiting, visited) {
            return true;
        }
    }
    visiting.remove(name);
    visited.insert(name.to_string());
    false
}

fn type_deps(defs: &TypeDefs, name: &str) -> Vec<String> {
    let mut deps = Vec::new();
    match defs.get(name) {
        Some(TypeDefKind::Struct(def)) => {
            for (_, ty) in &def.fields {
                collect_named_types(ty, &mut deps);
            }
        }
        Some(TypeDefKind::Enum(def)) => {
            for (_, fields) in &def.variants {
                for ty in fields {
                    collect_named_types(ty, &mut deps);
                }
            }
        }
        None => {}
    }
    deps
}

fn collect_named_types(ty: &Type, out: &mut Vec<String>) {
    match ty {
        Type::Named(name) => out.push(name.clone()),
        // Indirect/container types break recursion for cycle checks.
        Type::Ref(_)
        | Type::MutRef(_)
        | Type::Slice(_)
        | Type::Chan(_)
        | Type::Shared(_)
        | Type::Iter(_)
        | Type::Map(_, _) => {}
        Type::Tuple(items) => {
            for item in items {
                collect_named_types(item, out);
            }
        }
        Type::Result(ok, err) => {
            collect_named_types(ok, out);
            collect_named_types(err, out);
        }
        _ => {}
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LinearState {
    Uninit,
    Alive,
    Moved,
    Dropped,
}

#[derive(Clone, Debug)]
struct VarInfo {
    ty: Type,
    class: TypeClass,
    state: LinearState,
    borrowed_shared: usize,
    borrowed_mut: usize,
    view_of: Option<String>,
    view_is_mut: bool,
}

#[derive(Clone, Debug)]
struct Env {
    vars: HashMap<String, VarInfo>,
    scopes: Vec<Vec<String>>,
}

impl Env {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
            scopes: vec![Vec::new()],
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    fn exit_scope(&mut self) -> Vec<String> {
        self.scopes.pop().unwrap_or_default()
    }

    fn declare(&mut self, name: String, info: VarInfo) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.push(name.clone());
        }
        self.vars.insert(name, info);
    }
}

#[derive(Clone, Debug)]
struct BorrowInfo {
    base: String,
    is_mut: bool,
}

#[derive(Clone, Debug)]
struct ExprResult {
    ty: Type,
    borrow: Option<BorrowInfo>,
}

#[derive(Clone, Debug)]
enum AutoadjFailKind {
    NotAddressable { need_mut: bool },
    NotMutable,
}

#[derive(Clone, Debug)]
struct AutoadjFail {
    kind: AutoadjFailKind,
    recv_span: Span,
    method: String,
    recv_base_name: Option<String>,
    recv_ty: Type,
    param0_ty: Type,
}

struct FunctionChecker<'a> {
    defs: &'a TypeDefs,
    funcs: &'a HashMap<String, FunctionSig>,
    globals: &'a HashMap<String, ExternGlobalSig>,
    sig: &'a FunctionSig,
    is_std: bool,
    diags: &'a mut Diagnostics,
    expr_types: &'a mut HashMap<ExprId, Type>,
    env: Env,
    allow_iter_chain: bool,
    unsafe_depth: usize,
    defined_asm_labels: HashSet<String>,
    pending_asm_goto_labels: Vec<(String, Span)>,
}

impl<'a> FunctionChecker<'a> {
    fn new(
        defs: &'a TypeDefs,
        funcs: &'a HashMap<String, FunctionSig>,
        globals: &'a HashMap<String, ExternGlobalSig>,
        sig: &'a FunctionSig,
        is_std: bool,
        diags: &'a mut Diagnostics,
        expr_types: &'a mut HashMap<ExprId, Type>,
    ) -> Self {
        Self {
            defs,
            funcs,
            globals,
            sig,
            is_std,
            diags,
            expr_types,
            env: Env::new(),
            allow_iter_chain: false,
            unsafe_depth: if sig.is_unsafe { 1 } else { 0 },
            defined_asm_labels: HashSet::new(),
            pending_asm_goto_labels: Vec::new(),
        }
    }

    fn require_unsafe_operation(&mut self, span: &Span, what: &str) {
        if self.unsafe_depth == 0 {
            self.diags.push(
                format!("{} requires unsafe context (`unsafe {{ ... }}` or `unsafe fn`)", what),
                Some(span.clone()),
            );
        }
    }

    fn function_candidates(&self) -> Vec<String> {
        let mut out = self.funcs.keys().cloned().collect::<Vec<_>>();
        out.sort();
        out.dedup();
        out
    }

    fn recv_param_compatible(&self, recv_ty: &Type, param0_ty: &Type) -> bool {
        let (rbase, rk, _) = recv_ty.peel_refs();
        let (pbase, pk, _) = param0_ty.peel_refs();
        if !type_eq(rbase, pbase) {
            return false;
        }
        match (rk, pk) {
            (RefKind::None, RefKind::None) => true,
            (RefKind::Shared, RefKind::Shared) => true,
            (RefKind::Mut, RefKind::Mut) => true,
            (RefKind::None, RefKind::Shared) => true,  // autoref
            (RefKind::None, RefKind::Mut) => true,     // autoref mut
            (RefKind::Shared, RefKind::None) => true,  // autoderef
            (RefKind::Mut, RefKind::None) => true,     // autoderef
            (RefKind::Mut, RefKind::Shared) => true,   // reborrow
            (RefKind::Shared, RefKind::Mut) => false,
        }
    }

    fn function_ptr_type(sig: &FunctionSig) -> Type {
        Type::FnPtr {
            params: sig.params.clone(),
            ret: Box::new(sig.ret.clone()),
            is_variadic: sig.is_variadic,
        }
    }

    fn diag_autoadj_fail(&mut self, f: AutoadjFail) {
        match f.kind {
            AutoadjFailKind::NotAddressable { need_mut } => {
                let mut d = Diagnostic::new(
                    "cannot borrow temporary value as reference",
                    Some(f.recv_span.clone()),
                )
                .code(E1104)
                .label(f.recv_span.clone(), "temporary value is not addressable")
                .note(format!("this call requires receiver type: {}", f.param0_ty.pretty()))
                .note("but the receiver expression is a temporary value");
                d = d.help("bind the receiver to a local first:");
                if need_mut {
                    d = d.with_help_snippet(
                        f.recv_span.clone(),
                        "let mut tmp = {snippet};",
                    );
                } else {
                    d = d.with_help_snippet(
                        f.recv_span.clone(),
                        "let tmp = {snippet};",
                    );
                }
                d = d.help(format!("tmp.{}(...)", f.method));
                self.diags.push_diag(d);
            }
            AutoadjFailKind::NotMutable => {
                let mut d = Diagnostic::new(
                    "cannot borrow receiver as mutable",
                    Some(f.recv_span.clone()),
                )
                .code(E1105)
                .label(f.recv_span.clone(), "receiver is not mutable")
                .note(format!("this call requires receiver type: {}", f.param0_ty.pretty()))
                .note(format!("receiver type here is: {}", f.recv_ty.pretty()));

                if let Some(name) = f.recv_base_name.clone() {
                    d = d.help("make it mutable:")
                        .help(format!("let mut {0} = {0};", name))
                        .help(format!("{}.{}(...)", name, f.method));
                } else {
                    d = d
                        .help("bind it to a mutable local first:")
                        .with_help_snippet(f.recv_span.clone(), "let mut tmp = {snippet};")
                        .help(format!("tmp.{}(...)", f.method));
                }
                d = d.note("mutable borrows require a mutable receiver");
                self.diags.push_diag(d);
            }
        }
    }

    fn method_candidates_for_recv(&self, recv_ty: &Type) -> Vec<String> {
        let mut out = Vec::new();
        for (name, sig) in self.funcs.iter() {
            if let Some(p0) = sig.params.first() {
                if self.recv_param_compatible(recv_ty, p0) {
                    out.push(name.clone());
                }
            }
        }
        out.sort();
        out.dedup();
        out
    }

    fn diag_unknown_function(&mut self, span: Span, name: &str) {
        let mut d = Diagnostic::new(format!("unknown function `{}`", name), Some(span.clone()))
            .code(E_UNKNOWN_FUNCTION)
            .label(span, "unknown function")
            .note("function names are case-sensitive");
        let candidates = self.function_candidates();
        if let Some(h) = suggest::did_you_mean(name, candidates) {
            d = d.help(h);
        }
        self.diags.push_diag(d);
    }

    fn diag_unknown_method(&mut self, span: Span, recv_ty: Option<Type>, name: &str) {
        let type_name = match recv_ty.as_ref() {
            Some(Type::Named(n)) => n.clone(),
            Some(_) => "<value>".to_string(),
            None => "<value>".to_string(),
        };
        let mut d = Diagnostic::new(
            format!("unknown method `{}` on `{}`", name, type_name),
            Some(span.clone()),
        )
        .code(E_UNKNOWN_METHOD)
        .label(span, format!("unknown method `{}`", name))
        .note("methods are resolved by receiver type");
        let candidates = match recv_ty.as_ref() {
            Some(ty) => self.method_candidates_for_recv(ty),
            None => Vec::new(),
        };
        if let Some(h) = suggest::did_you_mean(name, candidates) {
            d = d.help(h);
        }
        d = d.note("consider calling a free function instead");
        self.diags.push_diag(d);
    }

    fn check_function(&mut self, func: &Function) {
        self.collect_asm_labels_in_block(&func.body);
        self.env.enter_scope();
        for (idx, param) in func.params.iter().enumerate() {
            let ty = self.sig.params.get(idx).cloned().unwrap_or(Type::Builtin(BuiltinType::Unit));
            let class = self.defs.classify(&ty).unwrap_or(TypeClass::Copy);
            let info = VarInfo {
                ty,
                class,
                state: if class == TypeClass::Linear {
                    LinearState::Alive
                } else {
                    LinearState::Alive
                },
                borrowed_shared: 0,
                borrowed_mut: 0,
                view_of: None,
                view_is_mut: false,
            };
            self.env.declare(param.name.clone(), info);
        }
        let block_ty = self.check_block(&func.body);
        if let Some(ty) = block_ty {
            if ty != Type::Builtin(BuiltinType::Unit) && !type_eq(&ty, &self.sig.ret) {
                self.diags.push(
                    "function body type does not match return type",
                    Some(func.span.clone()),
                );
            }
            if contains_view(self.defs, &ty) {
                self.diags.push(
                    "view types cannot be returned",
                    Some(func.span.clone()),
                );
            }
        }
        for (label, span) in self.pending_asm_goto_labels.drain(..) {
            if !self.defined_asm_labels.contains(&label) {
                self.diags.push(
                    format!("asm goto references unknown label `{}`", label),
                    Some(span),
                );
            }
        }
        self.drop_scope();
    }

    fn collect_asm_labels_in_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            match stmt {
                Stmt::Expr { expr, .. } => {
                    if let Some(label) = self.extract_asm_label_name(expr) {
                        if label.is_empty() {
                            self.diags
                                .push("asm_label name cannot be empty", Some(expr.span.clone()));
                        } else if !self.defined_asm_labels.insert(label.clone()) {
                            self.diags.push(
                                format!("duplicate asm label `{}`", label),
                                Some(expr.span.clone()),
                            );
                        }
                    }
                }
                Stmt::While { body, .. }
                | Stmt::Loop { body, .. }
                | Stmt::ForIn { body, .. } => self.collect_asm_labels_in_block(body),
                Stmt::Select { arms, .. } => {
                    for arm in arms {
                        if let BlockOrExpr::Block(block) = &arm.body {
                            self.collect_asm_labels_in_block(block);
                        }
                    }
                }
                Stmt::Let { .. }
                | Stmt::Assign { .. }
                | Stmt::Return { .. }
                | Stmt::Break { .. }
                | Stmt::Continue { .. }
                | Stmt::Go { .. }
                | Stmt::Defer { .. } => {}
            }
        }
    }

    fn extract_asm_label_name(&self, expr: &Expr) -> Option<String> {
        if let ExprKind::Call { callee, args, .. } = &expr.kind {
            if let ExprKind::Ident(name) = &callee.kind {
                if name == "asm_label" {
                    if let Some(Expr {
                        kind: ExprKind::String(s),
                        ..
                    }) = args.first()
                    {
                        return Some(s.trim().to_string());
                    }
                }
            }
        }
        None
    }

    fn check_block(&mut self, block: &Block) -> Option<Type> {
        self.env.enter_scope();
        let mut terminated = false;
        for stmt in &block.stmts {
            self.check_stmt(stmt);
            // Stop after a terminating statement to avoid cascading linear errors.
            if matches!(stmt, Stmt::Return { .. } | Stmt::Break { .. } | Stmt::Continue { .. }) {
                terminated = true;
                break;
            }
        }
        let result = if !terminated {
            if let Some(expr) = &block.tail {
            self.check_expr(expr).map(|r| r.ty)
            } else {
            Some(Type::Builtin(BuiltinType::Unit))
            }
        } else {
            Some(Type::Builtin(BuiltinType::Unit))
        };
        self.drop_scope();
        result
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let { name, ty, init, span } => {
                let init_res = self.check_expr(init);
                let init_ty = match init_res {
                    Some(res) => res,
                    None => return,
                };
                let final_ty = if let Some(annot) = ty {
                    let resolved = resolve_type(annot, self.defs, self.diags);
                    if let Some(resolved) = resolved {
                        let can_promote = matches!(init.kind, ExprKind::Int(_))
                            && promote_int_types(&resolved, &init_ty.ty).is_some();
                        if !type_eq(&resolved, &init_ty.ty) && !can_promote {
                            let d = Diagnostic::new(
                                "let initializer type mismatch",
                                Some(init.span.clone()),
                            )
                            .label(init.span.clone(), "type mismatch here")
                            .note(format!("expected type: {}", resolved.pretty()))
                            .note(format!("found type: {}", init_ty.ty.pretty()));
                            self.diags.push_diag(d);
                        } else if can_promote {
                            self.expr_types.insert(init.id, resolved.clone());
                        }
                        resolved
                    } else {
                        init_ty.ty.clone()
                    }
                } else {
                    init_ty.ty.clone()
                };
                let class = self.defs.classify(&final_ty).unwrap_or(TypeClass::Copy);
                let mut info = VarInfo {
                    ty: final_ty,
                    class,
                    state: if class == TypeClass::Linear {
                        LinearState::Alive
                    } else {
                        LinearState::Alive
                    },
                    borrowed_shared: 0,
                    borrowed_mut: 0,
                    view_of: None,
                    view_is_mut: false,
                };
                if class == TypeClass::View {
                    if let Some(borrow) = init_ty.borrow {
                        if let Some(base) = self.env.vars.get_mut(&borrow.base) {
                            if borrow.is_mut {
                                if base.borrowed_shared > 0 || base.borrowed_mut > 0 {
                                    self.diags.push(
                                        "mutable borrow conflicts with existing borrows",
                                        Some(span.clone()),
                                    );
                                } else {
                                    base.borrowed_mut += 1;
                                }
                            } else {
                                if base.borrowed_mut > 0 {
                                    self.diags.push(
                                        "shared borrow conflicts with mutable borrow",
                                        Some(span.clone()),
                                    );
                                } else {
                                    base.borrowed_shared += 1;
                                }
                            }
                        }
                        info.view_of = Some(borrow.base);
                        info.view_is_mut = borrow.is_mut;
                    } else {
                        self.diags.push(
                            "view values must originate from a borrow",
                            Some(span.clone()),
                        );
                    }
                }
                self.env.declare(name.clone(), info);
            }
            Stmt::Assign { target, value, span } => {
                let target_name = match &target.kind {
                    ExprKind::Ident(name) => Some(name.clone()),
                    _ => None,
                };
                let target_ty = match self.check_assign_target(target) {
                    Some(ty) => ty,
                    None => return,
                };
                let value_ty = match self.check_expr(value) {
                    Some(res) => res.ty,
                    None => return,
                };
                if contains_view(self.defs, &value_ty) {
                    self.diags.push(
                        "view values cannot be assigned",
                        Some(span.clone()),
                    );
                }
                if !type_eq(&target_ty, &value_ty) {
                    self.diags.push(
                        "assignment type mismatch",
                        Some(span.clone()),
                    );
                }
                if let Some(name) = target_name {
                    if let Some(var) = self.env.vars.get_mut(&name) {
                        if var.class == TypeClass::Linear {
                            var.state = LinearState::Alive;
                        }
                    }
                }
            }
            Stmt::Expr { expr, .. } => {
                self.check_expr(expr);
            }
            Stmt::Return { expr, span } => {
                let ret_ty = if let Some(expr) = expr {
                    match self.check_expr(expr) {
                        Some(res) => res.ty,
                        None => return,
                    }
                } else {
                    Type::Builtin(BuiltinType::Unit)
                };
                if contains_view(self.defs, &ret_ty) {
                    self.diags.push("view types cannot be returned", Some(span.clone()));
                }
                if !type_eq(&ret_ty, &self.sig.ret) {
                    self.diags.push("return type mismatch", Some(span.clone()));
                }
            }
            Stmt::Break { .. } => {}
            Stmt::Continue { .. } => {}
            Stmt::While { cond, body, span } => {
                let cond_res = match self.check_expr(cond) {
                    Some(res) => res.ty,
                    None => return,
                };
                if !type_eq(&cond_res, &Type::Builtin(BuiltinType::Bool)) {
                    self.diags.push("while condition must be bool", Some(span.clone()));
                }
                let _ = self.check_block(body);
            }
            Stmt::Loop { body, .. } => {
                let _ = self.check_block(body);
            }
            Stmt::ForIn { name, iter, body, span } => {
                let prev_allow = self.allow_iter_chain;
                self.allow_iter_chain = true;
                let iter_ty = match self.check_expr(iter) {
                    Some(res) => res.ty,
                    None => {
                        self.allow_iter_chain = prev_allow;
                        return;
                    }
                };
                self.allow_iter_chain = prev_allow;
                let (_elem_ty, view_ty, view_is_mut) = match iter_ty {
                    Type::Iter(inner) => {
                        let inner_ty = *inner;
                        let view_is_mut = matches!(inner_ty, Type::MutRef(_));
                        (inner_ty.clone(), inner_ty, view_is_mut)
                    }
                    Type::Slice(inner) => {
                        let elem = *inner;
                        if !is_copy_type(self.defs, &elem) {
                            self.diags.push(
                                "for-in by value requires Copy element; use &xs or &mut xs",
                                Some(span.clone()),
                            );
                        }
                        (elem.clone(), elem, false)
                    }
                    Type::Builtin(BuiltinType::Bytes) => {
                        let elem = Type::Builtin(BuiltinType::U32);
                        (elem.clone(), elem, false)
                    }
                    Type::Ref(inner) => match *inner {
                        Type::Slice(elem) => {
                            let elem = *elem;
                            (
                                elem.clone(),
                                Type::Ref(Box::new(elem)),
                                false,
                            )
                        }
                        Type::Builtin(BuiltinType::Bytes) => {
                            let elem = Type::Builtin(BuiltinType::U32);
                            (
                                elem.clone(),
                                Type::Ref(Box::new(elem)),
                                false,
                            )
                        }
                        _ => {
                            self.diags.push("for-in expects a slice", Some(span.clone()));
                            return;
                        }
                    },
                    Type::MutRef(inner) => match *inner {
                        Type::Slice(elem) => {
                            let elem = *elem;
                            (
                                elem.clone(),
                                Type::MutRef(Box::new(elem)),
                                true,
                            )
                        }
                        Type::Builtin(BuiltinType::Bytes) => {
                            let elem = Type::Builtin(BuiltinType::U32);
                            (
                                elem.clone(),
                                Type::MutRef(Box::new(elem)),
                                true,
                            )
                        }
                        _ => {
                            self.diags.push("for-in expects a slice", Some(span.clone()));
                            return;
                        }
                    },
                    _ => {
                        self.diags.push("for-in expects a slice", Some(span.clone()));
                        return;
                    }
                };
                self.env.enter_scope();
                let class = self.defs.classify(&view_ty).unwrap_or(TypeClass::Copy);
                self.env.declare(
                    name.clone(),
                    VarInfo {
                        ty: view_ty,
                        class,
                        state: LinearState::Alive,
                        borrowed_shared: 0,
                        borrowed_mut: 0,
                        view_of: None,
                        view_is_mut,
                    },
                );
                self.check_block(body);
                self.drop_scope();
            }
            Stmt::Select { arms, span } => {
                let base_env = self.env.clone();
                let mut joined_env = base_env.clone();
                for arm in arms {
                    self.env = base_env.clone();
                    self.env.enter_scope();
                    match &arm.kind {
                        SelectArmKind::Send { chan, value } => {
                            let _ = self.check_expr_no_move(chan);
                            let _ = self.check_expr(value);
                        }
                        SelectArmKind::Recv { chan, bind } => {
                            let chan_res = match self.check_expr_no_move(chan) {
                                Some(res) => res.ty,
                                None => continue,
                            };
                            let elem_ty = match chan_res {
                                Type::Chan(inner) => *inner,
                                _ => {
                                    self.diags.push("recv expects chan[T]", Some(arm.span.clone()));
                                    continue;
                                }
                            };
                            if let Some((name, ok_name)) = bind {
                                let class = self.defs.classify(&elem_ty).unwrap_or(TypeClass::Copy);
                                self.env.declare(
                                    name.clone(),
                                    VarInfo {
                                        ty: elem_ty,
                                        class,
                                        state: LinearState::Alive,
                                        borrowed_shared: 0,
                                        borrowed_mut: 0,
                                        view_of: None,
                                        view_is_mut: false,
                                    },
                                );
                                self.env.declare(
                                    ok_name.clone(),
                                    VarInfo {
                                        ty: Type::Builtin(BuiltinType::Bool),
                                        class: TypeClass::Copy,
                                        state: LinearState::Alive,
                                        borrowed_shared: 0,
                                        borrowed_mut: 0,
                                        view_of: None,
                                        view_is_mut: false,
                                    },
                                );
                            }
                        }
                        SelectArmKind::After { ms } => {
                            let _ = self.check_expr(ms);
                        }
                        SelectArmKind::Default => {}
                    }
                    match &arm.body {
                        BlockOrExpr::Block(block) => {
                            let _ = self.check_block(block);
                        }
                        BlockOrExpr::Expr(expr) => {
                            let _ = self.check_expr(expr);
                        }
                    }
                    self.drop_scope();
                    let arm_env = self.env.clone();
                    joined_env = self.join_env(&joined_env, &arm_env, span);
                    self.env = base_env.clone();
                }
                self.env = joined_env;
            }
            Stmt::Go { expr, span } => {
                match &expr.kind {
                    ExprKind::Call { callee, type_args, .. } => {
                        if !type_args.is_empty() {
                            self.diags.push(
                                "go does not accept type arguments",
                                Some(span.clone()),
                            );
                        }
                        if let ExprKind::Ident(name) = &callee.kind {
                            if name == "make_chan" {
                                self.diags.push(
                                    "go does not support intrinsic calls",
                                    Some(span.clone()),
                                );
                            }
                        } else {
                            self.diags
                                .push("go expects a direct call", Some(span.clone()));
                        }
                    }
                    _ => {
                        self.diags
                            .push("go expects a call expression", Some(span.clone()));
                    }
                }
                let _ = self.check_expr(expr);
            }
            Stmt::Defer { expr, span } => {
                let is_call = matches!(expr.kind, ExprKind::Call { .. });
                if !is_call {
                    self.diags
                        .push("defer expects a call expression", Some(span.clone()));
                    return;
                }
                let _ = self.check_expr(expr);
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> Option<ExprResult> {
        self.check_expr_with_mode(expr, true)
    }

    fn check_expr_no_move(&mut self, expr: &Expr) -> Option<ExprResult> {
        self.check_expr_with_mode(expr, false)
    }

    fn record_expr(&mut self, expr: &Expr, res: ExprResult) -> Option<ExprResult> {
        self.expr_types.insert(expr.id, res.ty.clone());
        Some(res)
    }

    fn declare_pattern_binding(&mut self, name: &str, ty: Type) {
        let class = self.defs.classify(&ty).unwrap_or(TypeClass::Copy);
        let info = VarInfo {
            ty,
            class,
            state: LinearState::Alive,
            borrowed_shared: 0,
            borrowed_mut: 0,
            view_of: None,
            view_is_mut: false,
        };
        self.env.declare(name.to_string(), info);
    }

    fn check_match_pattern(&mut self, pattern: &Pattern, scrut_ty: &Type, span: &Span) {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Bool(_) => {
                if !type_eq(scrut_ty, &Type::Builtin(BuiltinType::Bool)) {
                    self.diags.push("match pattern expects bool", Some(span.clone()));
                }
            }
            Pattern::Int(_) => {
                let ok = matches!(
                    scrut_ty,
                    Type::Builtin(
                        BuiltinType::I32
                            | BuiltinType::I64
                            | BuiltinType::U32
                            | BuiltinType::U64
                            | BuiltinType::Char
                    )
                );
                if !ok {
                    self.diags.push("match pattern expects integer", Some(span.clone()));
                }
            }
            Pattern::Ident(name) => {
                self.declare_pattern_binding(name, scrut_ty.clone());
            }
            Pattern::Variant {
                enum_name,
                variant,
                binds,
            } => {
                if let Type::Result(ok_ty, err_ty) = scrut_ty {
                    if enum_name != "Result" && enum_name != "result" {
                        self.diags.push(
                            "enum pattern does not match scrutinee type",
                            Some(span.clone()),
                        );
                        return;
                    }
                    let field_ty = match variant.as_str() {
                        "Ok" | "ok" => ok_ty.as_ref().clone(),
                        "Err" | "err" => err_ty.as_ref().clone(),
                        _ => {
                            let mut d = Diagnostic::new("unknown enum variant", Some(span.clone()))
                                .code(E_UNKNOWN_VARIANT)
                                .label(span.clone(), "unknown variant");
                            let candidates = vec!["Ok".to_string(), "Err".to_string()];
                            if let Some(h) = suggest::did_you_mean(variant, candidates) {
                                d = d.help(h);
                            }
                            self.diags.push_diag(d);
                            return;
                        }
                    };
                    if binds.len() != 1 {
                        self.diags.push("enum pattern arity mismatch", Some(span.clone()));
                    }
                    if let Some(bind) = binds.get(0) {
                        if bind != "_" {
                            self.declare_pattern_binding(bind, field_ty);
                        }
                    }
                    return;
                }
                let scrut_name = match scrut_ty {
                    Type::Named(name) => name.clone(),
                    _ => {
                        self.diags.push("enum pattern expects enum type", Some(span.clone()));
                        return;
                    }
                };
                if scrut_name != *enum_name {
                    self.diags
                        .push("enum pattern does not match scrutinee type", Some(span.clone()));
                    return;
                }
                let def = match self.defs.get(&scrut_name) {
                    Some(TypeDefKind::Enum(def)) => def,
                    _ => {
                        self.diags.push("enum pattern expects enum type", Some(span.clone()));
                        return;
                    }
                };
                let fields = match def.variants.iter().find(|(name, _)| name == variant) {
                    Some((_, fields)) => fields,
                    None => {
                        let mut d = Diagnostic::new("unknown enum variant", Some(span.clone()))
                            .code(E_UNKNOWN_VARIANT)
                            .label(span.clone(), "unknown variant");
                        let candidates = def
                            .variants
                            .iter()
                            .map(|(name, _)| name.clone())
                            .collect::<Vec<_>>();
                        if let Some(h) = suggest::did_you_mean(variant, candidates) {
                            d = d.help(h);
                        }
                        self.diags.push_diag(d);
                        return;
                    }
                };
                if binds.len() != fields.len() {
                    self.diags.push("enum pattern arity mismatch", Some(span.clone()));
                }
                for (idx, field_ty) in fields.iter().enumerate() {
                    if let Some(bind) = binds.get(idx) {
                        if bind != "_" {
                            self.declare_pattern_binding(bind, field_ty.clone());
                        }
                    }
                }
            }
        }
    }

    fn check_result_ctor(
        &mut self,
        variant: &str,
        type_args: &[TypeAst],
        args: &[Expr],
        span: &Span,
    ) -> Option<ExprResult> {
        if type_args.len() != 2 {
            self.diags.push(
                "Result constructors require two type arguments",
                Some(span.clone()),
            );
            return None;
        }
        let ok_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
        let err_ty = resolve_type(&type_args[1], self.defs, self.diags)?;
        if contains_view(self.defs, &ok_ty) || contains_view(self.defs, &err_ty) {
            self.diags
                .push("view types cannot be stored in Result", Some(span.clone()));
        }
        let result_ty = Type::Result(Box::new(ok_ty.clone()), Box::new(err_ty.clone()));
        match variant {
            "Ok" | "ok" => {
                if args.len() != 1 {
                    self.diags
                        .push("Result.Ok expects 1 argument", Some(span.clone()));
                }
                if let Some(arg) = args.get(0) {
                    let res = self.check_expr(arg)?;
                    if view_arg_not_allowed(self.defs, &res.ty, &ok_ty) {
                        self.diags.push(
                            "view arguments can only be passed to intrinsics",
                            Some(arg.span.clone()),
                        );
                    }
                    if !type_eq(&res.ty, &ok_ty) {
                        self.diags
                            .push("argument type mismatch", Some(arg.span.clone()));
                    }
                }
            }
            "Err" | "err" => {
                if args.len() != 1 {
                    self.diags
                        .push("Result.Err expects 1 argument", Some(span.clone()));
                }
                if let Some(arg) = args.get(0) {
                    let res = self.check_expr(arg)?;
                    if view_arg_not_allowed(self.defs, &res.ty, &err_ty) {
                        self.diags.push(
                            "view arguments can only be passed to intrinsics",
                            Some(arg.span.clone()),
                        );
                    }
                    if !type_eq(&res.ty, &err_ty) {
                        self.diags
                            .push("argument type mismatch", Some(arg.span.clone()));
                    }
                }
            }
            _ => {
                let mut d = Diagnostic::new("unknown enum variant", Some(span.clone()))
                    .code(E_UNKNOWN_VARIANT)
                    .label(span.clone(), "unknown variant");
                let candidates = vec!["Ok".to_string(), "Err".to_string()];
                if let Some(h) = suggest::did_you_mean(variant, candidates) {
                    d = d.help(h);
                }
                self.diags.push_diag(d);
                return None;
            }
        }
        Some(ExprResult {
            ty: result_ty,
            borrow: None,
        })
    }

    fn match_is_exhaustive(&self, scrut_ty: &Type, arms: &[MatchArm]) -> bool {
        if arms.iter().any(|arm| matches!(arm.pattern, Pattern::Wildcard | Pattern::Ident(_))) {
            return true;
        }
        match scrut_ty {
            Type::Builtin(BuiltinType::Bool) => {
                let mut saw_true = false;
                let mut saw_false = false;
                for arm in arms {
                    match arm.pattern {
                        Pattern::Bool(true) => saw_true = true,
                        Pattern::Bool(false) => saw_false = true,
                        _ => {}
                    }
                }
                saw_true && saw_false
            }
            Type::Result(_, _) => {
                let mut saw_ok = false;
                let mut saw_err = false;
                for arm in arms {
                    if let Pattern::Variant { ref enum_name, ref variant, .. } = arm.pattern {
                        if enum_name != "Result" && enum_name != "result" {
                            continue;
                        }
                        match variant.as_str() {
                            "Ok" | "ok" => saw_ok = true,
                            "Err" | "err" => saw_err = true,
                            _ => {}
                        }
                    }
                }
                saw_ok && saw_err
            }
            Type::Named(name) => match self.defs.get(name) {
                Some(TypeDefKind::Enum(def)) => {
                    let mut seen = HashSet::new();
                    for arm in arms {
                        if let Pattern::Variant { enum_name, variant, .. } = &arm.pattern {
                            if enum_name == name {
                                seen.insert(variant.clone());
                            }
                        }
                    }
                    def.variants
                        .iter()
                        .all(|(variant, _)| seen.contains(variant))
                }
                _ => false,
            },
            _ => false,
        }
    }

    fn check_intrinsic_call(
        &mut self,
        callee_name: &str,
        type_args: &[TypeAst],
        args: &[Expr],
        span: &Span,
    ) -> Option<ExprResult> {
        match callee_name {
            "__gost_println" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_println is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags
                        .push("__gost_println does not take type arguments", Some(span.clone()));
                }
                for arg in args {
                    let res = self.check_expr(arg)?;
                    if !type_eq(&res.ty, &Type::Builtin(BuiltinType::String)) {
                        self.diags
                            .push("__gost_println expects string", Some(arg.span.clone()));
                    }
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::Unit),
                    borrow: None,
                });
            }
            "string_len" => {
                if !type_args.is_empty() {
                    self.diags
                        .push("string_len does not take type arguments", Some(span.clone()));
                }
                if args.len() != 1 {
                    self.diags
                        .push("string_len expects 1 argument", Some(span.clone()));
                    return None;
                }
                let res = self.check_expr(&args[0])?;
                if !type_eq(&res.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags
                        .push("string_len expects string", Some(args[0].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I64),
                    borrow: None,
                });
            }
            "string_get" => {
                if !type_args.is_empty() {
                    self.diags
                        .push("string_get does not take type arguments", Some(span.clone()));
                }
                if args.len() != 2 {
                    self.diags
                        .push("string_get expects 2 arguments", Some(span.clone()));
                    return None;
                }
                let s = self.check_expr(&args[0])?;
                if !type_eq(&s.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags
                        .push("string_get expects string as first argument", Some(args[0].span.clone()));
                }
                let idx = self.check_expr(&args[1])?;
                if !matches!(idx.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags
                        .push("string_get expects i64 index", Some(args[1].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I32),
                    borrow: None,
                });
            }
            "string_slice" => {
                if !type_args.is_empty() {
                    self.diags
                        .push("string_slice does not take type arguments", Some(span.clone()));
                }
                if args.len() != 3 {
                    self.diags
                        .push("string_slice expects 3 arguments", Some(span.clone()));
                    return None;
                }
                let s = self.check_expr(&args[0])?;
                if !type_eq(&s.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags
                        .push("string_slice expects string as first argument", Some(args[0].span.clone()));
                }
                let start = self.check_expr(&args[1])?;
                let len = self.check_expr(&args[2])?;
                if !matches!(start.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags
                        .push("string_slice expects i64 start", Some(args[1].span.clone()));
                }
                if !matches!(len.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags
                        .push("string_slice expects i64 len", Some(args[2].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::String),
                    borrow: None,
                });
            }
            "string_concat" => {
                if !type_args.is_empty() {
                    self.diags
                        .push("string_concat does not take type arguments", Some(span.clone()));
                }
                if args.len() != 2 {
                    self.diags
                        .push("string_concat expects 2 arguments", Some(span.clone()));
                    return None;
                }
                let a = self.check_expr(&args[0])?;
                let b = self.check_expr(&args[1])?;
                if !type_eq(&a.ty, &Type::Builtin(BuiltinType::String))
                    || !type_eq(&b.ty, &Type::Builtin(BuiltinType::String))
                {
                    self.diags
                        .push("string_concat expects string arguments", Some(span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::String),
                    borrow: None,
                });
            }
            "string_from_byte" => {
                if !type_args.is_empty() {
                    self.diags
                        .push("string_from_byte does not take type arguments", Some(span.clone()));
                }
                if args.len() != 1 {
                    self.diags
                        .push("string_from_byte expects 1 argument", Some(span.clone()));
                    return None;
                }
                let b = self.check_expr(&args[0])?;
                if !matches!(b.ty, Type::Builtin(BuiltinType::I32 | BuiltinType::I64)) {
                    self.diags
                        .push("string_from_byte expects i32 byte value", Some(args[0].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::String),
                    borrow: None,
                });
            }
            "asm" | "asm_pure" | "asm_volatile" => {
                self.require_unsafe_operation(span, "inline asm");
                if args.is_empty() {
                    self.diags.push(
                        "asm expects at least 1 argument (template string literal)",
                        Some(span.clone()),
                    );
                    return None;
                }
                let template = self.check_expr(&args[0])?;
                if !type_eq(&template.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags
                        .push("asm template must be string", Some(args[0].span.clone()));
                }
                if !matches!(args[0].kind, ExprKind::String(_)) {
                    self.diags
                        .push("asm template must be string literal", Some(args[0].span.clone()));
                }

                let mut constraint_lit: Option<String> = None;
                if args.len() >= 2 {
                    let constraints = self.check_expr(&args[1])?;
                    if !type_eq(&constraints.ty, &Type::Builtin(BuiltinType::String)) {
                        self.diags
                            .push("asm constraints must be string", Some(args[1].span.clone()));
                    }
                    if let ExprKind::String(s) = &args[1].kind {
                        constraint_lit = Some(s.clone());
                    } else {
                        self.diags.push(
                            "asm constraints must be string literal",
                            Some(args[1].span.clone()),
                        );
                    }
                }

                let operand_start = if args.len() >= 2 { 2 } else { 1 };
                let mut operand_results = Vec::new();
                for arg in args.iter().skip(operand_start) {
                    let res = self.check_expr(arg)?;
                    if res.ty == Type::Builtin(BuiltinType::Unit) {
                        self.diags.push("asm operands cannot be unit", Some(arg.span.clone()));
                    }
                    operand_results.push(res.ty);
                }

                let ret_ty = if type_args.is_empty() {
                    if let Some(constraints) = constraint_lit {
                        let spec = parse_asm_constraint_spec(&constraints);
                        let explicit_count = spec.outputs.len() + spec.input_count;
                        let legacy_count = spec.readwrite_outputs + spec.input_count;
                        let operand_n = operand_results.len();
                        let explicit_style = operand_n == explicit_count;
                        let legacy_style = !explicit_style && operand_n == legacy_count;
                        if !explicit_style && !legacy_style {
                            self.diags.push(
                                format!(
                                    "asm operand count mismatch (expected {} with explicit outputs or {} in legacy style, found {})",
                                    explicit_count, legacy_count, operand_n
                                ),
                                Some(span.clone()),
                            );
                        }
                        if spec.outputs.is_empty() {
                            Type::Builtin(BuiltinType::I64)
                        } else if spec.outputs.len() == 1 {
                            if explicit_style {
                                operand_results
                                    .get(0)
                                    .cloned()
                                    .unwrap_or(Type::Builtin(BuiltinType::I64))
                            } else if spec.readwrite_outputs > 0 {
                                operand_results
                                    .get(0)
                                    .cloned()
                                    .unwrap_or(Type::Builtin(BuiltinType::I64))
                            } else {
                                Type::Builtin(BuiltinType::I64)
                            }
                        } else if explicit_style {
                            Type::Tuple(
                                operand_results
                                    .iter()
                                    .take(spec.outputs.len())
                                    .cloned()
                                    .collect(),
                            )
                        } else if spec.readwrite_outputs == spec.outputs.len()
                            && operand_results.len() >= spec.outputs.len()
                        {
                            Type::Tuple(
                                operand_results
                                    .iter()
                                    .take(spec.outputs.len())
                                    .cloned()
                                    .collect(),
                            )
                        } else {
                            self.diags.push(
                                "multiple asm outputs require explicit output operands (colon syntax)",
                                Some(span.clone()),
                            );
                            Type::Tuple(
                                (0..spec.outputs.len())
                                    .map(|_| Type::Builtin(BuiltinType::I64))
                                    .collect(),
                            )
                        }
                    } else {
                        Type::Builtin(BuiltinType::I64)
                    }
                } else if type_args.len() == 1 {
                    resolve_type(&type_args[0], self.defs, self.diags)
                        .unwrap_or(Type::Builtin(BuiltinType::I64))
                } else {
                    self.diags.push(
                        "asm expects at most 1 type argument (return type)",
                        Some(span.clone()),
                    );
                    return None;
                };

                return Some(ExprResult {
                    ty: ret_ty,
                    borrow: None,
                });
            }
            "asm_label" => {
                if !type_args.is_empty() {
                    self.diags
                        .push("asm_label does not take type arguments", Some(span.clone()));
                }
                if args.len() != 1 {
                    self.diags
                        .push("asm_label expects 1 argument", Some(span.clone()));
                    return None;
                }
                let res = self.check_expr(&args[0])?;
                if !type_eq(&res.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags
                        .push("asm_label expects string argument", Some(args[0].span.clone()));
                }
                let label = match &args[0].kind {
                    ExprKind::String(s) => s.trim().to_string(),
                    _ => {
                        self.diags.push(
                            "asm_label argument must be string literal",
                            Some(args[0].span.clone()),
                        );
                        return Some(ExprResult {
                            ty: Type::Builtin(BuiltinType::Unit),
                            borrow: None,
                        });
                    }
                };
                if label.is_empty() {
                    self.diags
                        .push("asm_label name cannot be empty", Some(args[0].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::Unit),
                    borrow: None,
                });
            }
            "asm_goto" => {
                self.require_unsafe_operation(span, "inline asm goto");
                if type_args.len() > 1 {
                    self.diags
                        .push("asm_goto expects at most 1 type argument", Some(span.clone()));
                    return None;
                }
                if args.len() < 3 {
                    self.diags.push(
                        "asm_goto expects at least 3 arguments (template, constraints, labels)",
                        Some(span.clone()),
                    );
                    return None;
                }
                for (idx, arg_name) in ["template", "constraints", "labels"]
                    .into_iter()
                    .enumerate()
                {
                    let res = self.check_expr(&args[idx])?;
                    if !type_eq(&res.ty, &Type::Builtin(BuiltinType::String)) {
                        self.diags.push(
                            format!("asm_goto {} must be string", arg_name),
                            Some(args[idx].span.clone()),
                        );
                    }
                    if !matches!(args[idx].kind, ExprKind::String(_)) {
                        self.diags.push(
                            format!("asm_goto {} must be string literal", arg_name),
                            Some(args[idx].span.clone()),
                        );
                    }
                }
                let constraint_text = match &args[1].kind {
                    ExprKind::String(s) => s.clone(),
                    _ => String::new(),
                };
                let spec = parse_asm_constraint_spec(&constraint_text);
                let mut parsed_label_count = 0usize;
                if let ExprKind::String(raw_labels) = &args[2].kind {
                    let labels = raw_labels
                        .split(',')
                        .map(|s| s.trim())
                        .filter(|s| !s.is_empty());
                    for l in labels {
                        self.pending_asm_goto_labels
                            .push((l.to_string(), args[2].span.clone()));
                        parsed_label_count += 1;
                    }
                    if parsed_label_count == 0 {
                        self.diags.push(
                            "asm_goto labels list cannot be empty",
                            Some(args[2].span.clone()),
                        );
                    }
                }
                if spec.label_count != 0 && spec.label_count != parsed_label_count {
                    self.diags.push(
                        format!(
                            "asm_goto label constraint count mismatch (constraints: {}, labels: {})",
                            spec.label_count, parsed_label_count
                        ),
                        Some(args[1].span.clone()),
                    );
                }
                let mut operand_results = Vec::new();
                for arg in args.iter().skip(3) {
                    let res = self.check_expr(arg)?;
                    if res.ty == Type::Builtin(BuiltinType::Unit) {
                        self.diags.push(
                            "asm_goto operands cannot be unit",
                            Some(arg.span.clone()),
                        );
                    }
                    operand_results.push(res.ty);
                }
                let explicit_count = spec.outputs.len() + spec.input_count;
                let legacy_count = spec.readwrite_outputs + spec.input_count;
                let operand_n = operand_results.len();
                let explicit_style = operand_n == explicit_count;
                let legacy_style = !explicit_style && operand_n == legacy_count;
                if !explicit_style && !legacy_style {
                    self.diags.push(
                        format!(
                            "asm_goto operand count mismatch (expected {} with explicit outputs or {} in legacy style, found {})",
                            explicit_count, legacy_count, operand_n
                        ),
                        Some(span.clone()),
                    );
                }
                let ret_ty = if let Some(ret) = type_args.first() {
                    resolve_type(ret, self.defs, self.diags)
                        .unwrap_or(Type::Builtin(BuiltinType::Unit))
                } else if spec.outputs.is_empty() {
                    Type::Builtin(BuiltinType::Unit)
                } else if spec.outputs.len() == 1 {
                    if explicit_style {
                        operand_results
                            .get(0)
                            .cloned()
                            .unwrap_or(Type::Builtin(BuiltinType::I64))
                    } else if spec.readwrite_outputs > 0 {
                        operand_results
                            .get(0)
                            .cloned()
                            .unwrap_or(Type::Builtin(BuiltinType::I64))
                    } else {
                        Type::Builtin(BuiltinType::I64)
                    }
                } else if explicit_style {
                    Type::Tuple(
                        operand_results
                            .iter()
                            .take(spec.outputs.len())
                            .cloned()
                            .collect(),
                    )
                } else if spec.readwrite_outputs == spec.outputs.len()
                    && operand_results.len() >= spec.outputs.len()
                {
                    Type::Tuple(
                        operand_results
                            .iter()
                            .take(spec.outputs.len())
                            .cloned()
                            .collect(),
                    )
                } else {
                    self.diags.push(
                        "multiple asm_goto outputs require explicit output operands (colon syntax)",
                        Some(span.clone()),
                    );
                    Type::Tuple(
                        (0..spec.outputs.len())
                            .map(|_| Type::Builtin(BuiltinType::I64))
                            .collect(),
                    )
                };
                return Some(ExprResult {
                    ty: ret_ty,
                    borrow: None,
                });
            }
            "__gost_error_new" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_error_new is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags
                        .push("__gost_error_new does not take type arguments", Some(span.clone()));
                }
                if args.len() != 1 {
                    self.diags
                        .push("__gost_error_new expects 1 argument", Some(span.clone()));
                }
                if let Some(arg) = args.get(0) {
                    let res = self.check_expr(arg)?;
                    if !type_eq(&res.ty, &Type::Builtin(BuiltinType::String)) {
                        self.diags
                            .push("__gost_error_new expects string", Some(arg.span.clone()));
                    }
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::Error),
                    borrow: None,
                });
            }
            "__gost_singleton_acquire" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_singleton_acquire is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags
                        .push("__gost_singleton_acquire does not take type arguments", Some(span.clone()));
                }
                if args.len() != 1 {
                    self.diags
                        .push("__gost_singleton_acquire expects 1 argument", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                if !type_eq(&a0.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags
                        .push("__gost_singleton_acquire expects string name", Some(args[0].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I32),
                    borrow: None,
                });
            }
            "__gost_net_last_status" | "__gost_net_last_http_status" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_net_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags
                        .push("__gost_net_last_status does not take type arguments", Some(span.clone()));
                }
                if !args.is_empty() {
                    self.diags
                        .push("__gost_net_last_status expects 0 arguments", Some(span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I32),
                    borrow: None,
                });
            }
            "__gost_net_last_error" | "__gost_net_last_peer" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_net_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags
                        .push("__gost_net_last_error does not take type arguments", Some(span.clone()));
                }
                if !args.is_empty() {
                    self.diags
                        .push("__gost_net_last_error expects 0 arguments", Some(span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::String),
                    borrow: None,
                });
            }
            "__gost_net_tcp_listen" | "__gost_net_tcp_connect" | "__gost_net_udp_bind" | "__gost_net_udp_connect" | "__gost_net_ws_connect" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_net_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags
                        .push("__gost_net_* does not take type arguments", Some(span.clone()));
                }
                if args.len() != 1 {
                    self.diags
                        .push("__gost_net_* expects 1 argument", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                if !type_eq(&a0.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags
                        .push("__gost_net_* expects string address", Some(args[0].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I64),
                    borrow: None,
                });
            }
            "__gost_net_tcp_accept" | "__gost_net_tcp_close" | "__gost_net_udp_close" | "__gost_net_ws_close" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_net_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags
                        .push("__gost_net_* does not take type arguments", Some(span.clone()));
                }
                if args.len() != 1 {
                    self.diags
                        .push("__gost_net_* expects 1 argument", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                if !matches!(a0.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags
                        .push("__gost_net_* expects handle i64", Some(args[0].span.clone()));
                }
                let ret = if callee_name == "__gost_net_tcp_accept" {
                    Type::Builtin(BuiltinType::I64)
                } else {
                    Type::Builtin(BuiltinType::I32)
                };
                return Some(ExprResult {
                    ty: ret,
                    borrow: None,
                });
            }
            "__gost_net_ws_send_text" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_net_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags
                        .push("__gost_net_ws_send_text does not take type arguments", Some(span.clone()));
                }
                if args.len() != 2 {
                    self.diags
                        .push("__gost_net_ws_send_text expects 2 arguments", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                let a1 = self.check_expr(&args[1])?;
                if !matches!(a0.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags
                        .push("__gost_net_ws_send_text expects handle i64", Some(args[0].span.clone()));
                }
                if !type_eq(&a1.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags
                        .push("__gost_net_ws_send_text expects string payload", Some(args[1].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I32),
                    borrow: None,
                });
            }
            "__gost_net_tcp_write" | "__gost_net_udp_send" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_net_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags
                        .push("__gost_net_* does not take type arguments", Some(span.clone()));
                }
                if args.len() != 2 {
                    self.diags
                        .push("__gost_net_* expects 2 arguments", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                let a1 = self.check_expr(&args[1])?;
                if !matches!(a0.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags
                        .push("__gost_net_* expects handle i64", Some(args[0].span.clone()));
                }
                if !type_eq(&a1.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags
                        .push("__gost_net_* expects string payload", Some(args[1].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I64),
                    borrow: None,
                });
            }
            "__gost_net_udp_send_to" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_net_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags
                        .push("__gost_net_udp_send_to does not take type arguments", Some(span.clone()));
                }
                if args.len() != 3 {
                    self.diags
                        .push("__gost_net_udp_send_to expects 3 arguments", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                let a1 = self.check_expr(&args[1])?;
                let a2 = self.check_expr(&args[2])?;
                if !matches!(a0.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags
                        .push("__gost_net_udp_send_to expects handle i64", Some(args[0].span.clone()));
                }
                if !type_eq(&a1.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags
                        .push("__gost_net_udp_send_to expects string address", Some(args[1].span.clone()));
                }
                if !type_eq(&a2.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags
                        .push("__gost_net_udp_send_to expects string payload", Some(args[2].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I64),
                    borrow: None,
                });
            }
            "__gost_net_tcp_read" | "__gost_net_udp_recv" | "__gost_net_udp_recv_from" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_net_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags
                        .push("__gost_net_* does not take type arguments", Some(span.clone()));
                }
                if args.len() != 2 {
                    self.diags
                        .push("__gost_net_* expects 2 arguments", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                let a1 = self.check_expr(&args[1])?;
                if !matches!(a0.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags
                        .push("__gost_net_* expects handle i64", Some(args[0].span.clone()));
                }
                if !matches!(a1.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags
                        .push("__gost_net_* expects i32 max length", Some(args[1].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::String),
                    borrow: None,
                });
            }
            "__gost_net_ws_recv_text" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_net_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags
                        .push("__gost_net_ws_recv_text does not take type arguments", Some(span.clone()));
                }
                if args.len() != 1 {
                    self.diags
                        .push("__gost_net_ws_recv_text expects 1 argument", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                if !matches!(a0.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags
                        .push("__gost_net_ws_recv_text expects handle i64", Some(args[0].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::String),
                    borrow: None,
                });
            }
            "__gost_net_http_request" | "__gost_net_http_request_headers" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_net_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags
                        .push("__gost_net_http_request does not take type arguments", Some(span.clone()));
                }
                let expected = if callee_name == "__gost_net_http_request_headers" {
                    5
                } else {
                    4
                };
                if args.len() != expected {
                    self.diags.push(
                        if expected == 5 {
                            "__gost_net_http_request_headers expects 5 arguments"
                        } else {
                            "__gost_net_http_request expects 4 arguments"
                        },
                        Some(span.clone()),
                    );
                    return None;
                }
                for (idx, arg) in args.iter().enumerate() {
                    let res = self.check_expr(arg)?;
                    if !type_eq(&res.ty, &Type::Builtin(BuiltinType::String)) {
                        self.diags.push(
                            "__gost_net_http_request expects string arguments",
                            Some(args[idx].span.clone()),
                        );
                    }
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::String),
                    borrow: None,
                });
            }
            "iter" | "iter_mut" | "filter" | "map" => {
                if !self.allow_iter_chain {
                    self.diags.push(
                        "iterator value cannot escape for-loop",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags
                        .push("iterator helpers do not take type arguments", Some(span.clone()));
                }
                match callee_name {
                    "iter" | "iter_mut" => {
                        if args.len() != 1 {
                            self.diags
                                .push("iter expects one argument", Some(span.clone()));
                            return None;
                        }
                        let arg_ty = self.check_expr_no_move(&args[0])?.ty;
                        let (elem_ty, is_mut) = match arg_ty {
                            Type::Ref(inner) => match *inner {
                                Type::Slice(elem) => (*elem, false),
                                Type::Builtin(BuiltinType::Bytes) => {
                                    (Type::Builtin(BuiltinType::U32), false)
                                }
                                _ => {
                                    self.diags.push(
                                        "iter expects &slice",
                                        Some(args[0].span.clone()),
                                    );
                                    return None;
                                }
                            },
                            Type::MutRef(inner) => match *inner {
                                Type::Slice(elem) => (*elem, true),
                                Type::Builtin(BuiltinType::Bytes) => {
                                    (Type::Builtin(BuiltinType::U32), true)
                                }
                                _ => {
                                    self.diags.push(
                                        "iter_mut expects &mut slice",
                                        Some(args[0].span.clone()),
                                    );
                                    return None;
                                }
                            },
                            _ => {
                                self.diags.push(
                                    "iter expects &slice",
                                    Some(args[0].span.clone()),
                                );
                                return None;
                            }
                        };
                        if callee_name == "iter" && is_mut {
                            self.diags.push("iter expects &slice", Some(args[0].span.clone()));
                        }
                        if callee_name == "iter_mut" && !is_mut {
                            self.diags
                                .push("iter_mut expects &mut slice", Some(args[0].span.clone()));
                        }
                        let item_ty = if is_mut {
                            Type::MutRef(Box::new(elem_ty))
                        } else {
                            Type::Ref(Box::new(elem_ty))
                        };
                        return Some(ExprResult {
                            ty: Type::Iter(Box::new(item_ty)),
                            borrow: None,
                        });
                    }
                    "filter" => {
                        if args.len() != 2 {
                            self.diags
                                .push("filter expects two arguments", Some(span.clone()));
                            return None;
                        }
                        let iter_ty = self.check_expr_no_move(&args[0])?.ty;
                        let item_ty = match iter_ty {
                            Type::Iter(inner) => *inner,
                            _ => {
                                self.diags.push(
                                    "filter expects iterator",
                                    Some(args[0].span.clone()),
                                );
                                return None;
                            }
                        };
                        let pred_name = match &args[1].kind {
                            ExprKind::Ident(name) => name.clone(),
                            _ => {
                                self.diags.push(
                                    "filter expects function symbol",
                                    Some(args[1].span.clone()),
                                );
                                return None;
                            }
                        };
                            let sig = match self.funcs.get(&pred_name) {
                                Some(sig) => sig,
                                None => {
                                    self.diag_unknown_function(args[1].span.clone(), &pred_name);
                                    return None;
                                }
                            };
                        if sig.params.len() != 1 {
                            self.diags.push(
                                "filter predicate must take one argument",
                                Some(args[1].span.clone()),
                            );
                        } else if !type_eq(&sig.params[0], &item_ty) {
                            self.diags.push(
                                "filter predicate type mismatch",
                                Some(args[1].span.clone()),
                            );
                        }
                        if !type_eq(&sig.ret, &Type::Builtin(BuiltinType::Bool)) {
                            self.diags.push(
                                "filter predicate must return bool",
                                Some(args[1].span.clone()),
                            );
                        }
                        return Some(ExprResult {
                            ty: Type::Iter(Box::new(item_ty)),
                            borrow: None,
                        });
                    }
                    "map" => {
                        if args.len() != 2 {
                            self.diags.push("map expects two arguments", Some(span.clone()));
                            return None;
                        }
                        let iter_ty = self.check_expr_no_move(&args[0])?.ty;
                        let item_ty = match iter_ty {
                            Type::Iter(inner) => *inner,
                            _ => {
                                self.diags.push(
                                    "map expects iterator",
                                    Some(args[0].span.clone()),
                                );
                                return None;
                            }
                        };
                        let map_name = match &args[1].kind {
                            ExprKind::Ident(name) => name.clone(),
                            _ => {
                                self.diags.push(
                                    "map expects function symbol",
                                    Some(args[1].span.clone()),
                                );
                                return None;
                            }
                        };
                        let sig = match self.funcs.get(&map_name) {
                            Some(sig) => sig,
                            None => {
                                self.diag_unknown_function(args[1].span.clone(), &map_name);
                                return None;
                            }
                        };
                        if sig.params.len() != 1 {
                            self.diags.push(
                                "map function must take one argument",
                                Some(args[1].span.clone()),
                            );
                        } else if !type_eq(&sig.params[0], &item_ty) {
                            self.diags.push(
                                "map function type mismatch",
                                Some(args[1].span.clone()),
                            );
                        }
                        return Some(ExprResult {
                            ty: Type::Iter(Box::new(sig.ret.clone())),
                            borrow: None,
                        });
                    }
                    _ => {}
                }
                return None;
            }
            "make_chan" => {
                if type_args.len() != 1 {
                    self.diags
                        .push("make_chan expects one type argument", Some(span.clone()));
                    return None;
                }
                if args.len() != 1 {
                    self.diags
                        .push("make_chan expects one argument", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let cap_ty = self.check_expr(&args[0])?.ty;
                if !type_eq(&cap_ty, &Type::Builtin(BuiltinType::I64))
                    && !type_eq(&cap_ty, &Type::Builtin(BuiltinType::I32))
                {
                    self.diags
                        .push("make_chan expects i64 cap", Some(args[0].span.clone()));
                }
                if contains_view(self.defs, &elem_ty) {
                    self.diags
                        .push("view types cannot be stored in channels", Some(span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Chan(Box::new(elem_ty)),
                    borrow: None,
                });
            }
            "make_slice" => {
                if type_args.len() != 1 {
                    self.diags
                        .push("make_slice expects one type argument", Some(span.clone()));
                    return None;
                }
                if args.len() != 2 {
                    self.diags
                        .push("make_slice expects two arguments", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let len_ty = self.check_expr(&args[0])?.ty;
                let cap_ty = self.check_expr(&args[1])?.ty;
                if !matches!(
                    len_ty,
                    Type::Builtin(BuiltinType::I64 | BuiltinType::I32)
                ) {
                    self.diags
                        .push("make_slice expects i64 len", Some(args[0].span.clone()));
                }
                if !matches!(
                    cap_ty,
                    Type::Builtin(BuiltinType::I64 | BuiltinType::I32)
                ) {
                    self.diags
                        .push("make_slice expects i64 cap", Some(args[1].span.clone()));
                }
                if contains_view(self.defs, &elem_ty) {
                    self.diags
                        .push("view types cannot be stored in slices", Some(span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Slice(Box::new(elem_ty)),
                    borrow: None,
                });
            }
            "slice_len" => {
                if type_args.len() != 1 {
                    self.diags
                        .push("slice_len expects one type argument", Some(span.clone()));
                    return None;
                }
                if args.len() != 1 {
                    self.diags
                        .push("slice_len expects one argument", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let arg_ty = self.check_expr(&args[0])?.ty;
                if !matches!(
                    arg_ty,
                    Type::Ref(inner) | Type::MutRef(inner) if *inner == Type::Slice(Box::new(elem_ty.clone()))
                ) {
                    self.diags
                        .push("slice_len expects ref []T", Some(args[0].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I64),
                    borrow: None,
                });
            }
            "slice_get_copy" => {
                if type_args.len() != 1 {
                    self.diags
                        .push("slice_get_copy expects one type argument", Some(span.clone()));
                    return None;
                }
                if args.len() != 2 {
                    self.diags
                        .push("slice_get_copy expects two arguments", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let arg_ty = self.check_expr(&args[0])?.ty;
                if !matches!(
                    arg_ty,
                    Type::Ref(inner) | Type::MutRef(inner) if *inner == Type::Slice(Box::new(elem_ty.clone()))
                ) {
                    self.diags
                        .push("slice_get_copy expects ref []T", Some(args[0].span.clone()));
                }
                let idx_ty = self.check_expr(&args[1])?.ty;
                if !matches!(
                    idx_ty,
                    Type::Builtin(BuiltinType::I64 | BuiltinType::I32)
                ) {
                    self.diags
                        .push("slice_get_copy expects i64 index", Some(args[1].span.clone()));
                }
                if !is_copy_type(self.defs, &elem_ty) {
                    self.diags
                        .push("slice_get_copy requires Copy element", Some(span.clone()));
                }
                return Some(ExprResult { ty: elem_ty, borrow: None });
            }
            "slice_set" => {
                if type_args.len() != 1 {
                    self.diags
                        .push("slice_set expects one type argument", Some(span.clone()));
                    return None;
                }
                if args.len() != 3 {
                    self.diags
                        .push("slice_set expects three arguments", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let arg_ty = self.check_expr(&args[0])?.ty;
                if !matches!(
                    arg_ty,
                    Type::MutRef(inner) if *inner == Type::Slice(Box::new(elem_ty.clone()))
                ) {
                    self.diags
                        .push("slice_set expects mutref []T", Some(args[0].span.clone()));
                }
                let idx_ty = self.check_expr(&args[1])?.ty;
                if !matches!(
                    idx_ty,
                    Type::Builtin(BuiltinType::I64 | BuiltinType::I32)
                ) {
                    self.diags
                        .push("slice_set expects i64 index", Some(args[1].span.clone()));
                }
                let val_ty = self.check_expr(&args[2])?.ty;
                if !type_eq(&val_ty, &elem_ty) {
                    self.diags
                        .push("slice_set expects element type", Some(args[2].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::Unit),
                    borrow: None,
                });
            }
            "slice_ref" | "slice_mutref" => {
                if type_args.len() != 1 {
                    self.diags
                        .push("slice_ref expects one type argument", Some(span.clone()));
                    return None;
                }
                if args.len() != 2 {
                    self.diags
                        .push("slice_ref expects two arguments", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let arg_res = self.check_expr(&args[0])?;
                match arg_res.ty {
                    Type::Ref(inner) | Type::MutRef(inner)
                        if *inner == Type::Slice(Box::new(elem_ty.clone())) => {}
                    _ => {
                        self.diags
                            .push("slice_ref expects ref []T", Some(args[0].span.clone()));
                    }
                }
                let idx_ty = self.check_expr(&args[1])?.ty;
                if !matches!(
                    idx_ty,
                    Type::Builtin(BuiltinType::I64 | BuiltinType::I32)
                ) {
                    self.diags
                        .push("slice_ref expects i64 index", Some(args[1].span.clone()));
                }
                let borrow = arg_res.borrow.map(|b| BorrowInfo {
                    base: b.base,
                    is_mut: callee_name == "slice_mutref",
                });
                let ret_ty = if callee_name == "slice_mutref" {
                    Type::MutRef(Box::new(elem_ty))
                } else {
                    Type::Ref(Box::new(elem_ty))
                };
                return Some(ExprResult { ty: ret_ty, borrow });
            }
            "slice_push" => {
                if type_args.len() != 1 {
                    self.diags
                        .push("slice_push expects one type argument", Some(span.clone()));
                    return None;
                }
                if args.len() != 2 {
                    self.diags
                        .push("slice_push expects two arguments", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let arg_ty = self.check_expr(&args[0])?.ty;
                if !matches!(
                    arg_ty,
                    Type::MutRef(inner) if *inner == Type::Slice(Box::new(elem_ty.clone()))
                ) {
                    self.diags
                        .push("slice_push expects mutref []T", Some(args[0].span.clone()));
                }
                let val_ty = self.check_expr(&args[1])?.ty;
                if !type_eq(&val_ty, &elem_ty) {
                    self.diags
                        .push("slice_push expects element type", Some(args[1].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::Unit),
                    borrow: None,
                });
            }
            "slice_pop" => {
                if type_args.len() != 1 {
                    self.diags
                        .push("slice_pop expects one type argument", Some(span.clone()));
                    return None;
                }
                if args.len() != 1 {
                    self.diags
                        .push("slice_pop expects one argument", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let arg_ty = self.check_expr(&args[0])?.ty;
                if !matches!(
                    arg_ty,
                    Type::MutRef(inner) if *inner == Type::Slice(Box::new(elem_ty.clone()))
                ) {
                    self.diags
                        .push("slice_pop expects mutref []T", Some(args[0].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Tuple(vec![elem_ty, Type::Builtin(BuiltinType::Bool)]),
                    borrow: None,
                });
            }
            "shared_new" => {
                if type_args.len() != 1 {
                    self.diags
                        .push("shared_new expects one type argument", Some(span.clone()));
                    return None;
                }
                if args.len() != 1 {
                    self.diags
                        .push("shared_new expects one argument", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let val_ty = self.check_expr(&args[0])?.ty;
                if !type_eq(&val_ty, &elem_ty) {
                    self.diags
                        .push("shared_new expects element type", Some(args[0].span.clone()));
                }
                if contains_view(self.defs, &elem_ty) {
                    self.diags
                        .push("view types cannot be stored in shared", Some(span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Shared(Box::new(elem_ty)),
                    borrow: None,
                });
            }
            "shared_get" | "shared_get_mut" => {
                if type_args.len() != 1 {
                    self.diags
                        .push("shared_get expects one type argument", Some(span.clone()));
                    return None;
                }
                if args.len() != 1 {
                    self.diags
                        .push("shared_get expects one argument", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let arg_res = self.check_expr(&args[0])?;
                match arg_res.ty {
                    Type::Ref(inner) | Type::MutRef(inner)
                        if *inner == Type::Shared(Box::new(elem_ty.clone())) => {}
                    _ => {
                        self.diags
                            .push("shared_get expects ref shared[T]", Some(args[0].span.clone()));
                    }
                }
                let borrow = arg_res.borrow.map(|b| BorrowInfo {
                    base: b.base,
                    is_mut: callee_name == "shared_get_mut",
                });
                let ret_ty = if callee_name == "shared_get_mut" {
                    Type::MutRef(Box::new(elem_ty))
                } else {
                    Type::Ref(Box::new(elem_ty))
                };
                return Some(ExprResult { ty: ret_ty, borrow });
            }
            "make_map" => {
                if type_args.len() != 2 {
                    self.diags
                        .push("make_map expects two type arguments", Some(span.clone()));
                    return None;
                }
                if args.len() != 1 {
                    self.diags
                        .push("make_map expects one argument", Some(span.clone()));
                    return None;
                }
                let key_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let val_ty = resolve_type(&type_args[1], self.defs, self.diags)?;
                if !is_map_key_type(&key_ty) {
                    self.diags
                        .push("map key type must be i64, u64, or string", Some(span.clone()));
                }
                if !is_copy_type(self.defs, &val_ty) {
                    self.diags
                        .push("map value type must be Copy", Some(span.clone()));
                }
                let cap_ty = self.check_expr(&args[0])?.ty;
                if !matches!(
                    cap_ty,
                    Type::Builtin(BuiltinType::I64 | BuiltinType::I32)
                ) {
                    self.diags
                        .push("make_map expects i64 cap", Some(args[0].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Map(Box::new(key_ty), Box::new(val_ty)),
                    borrow: None,
                });
            }
            "map_get" => {
                if type_args.len() != 2 {
                    self.diags
                        .push("map_get expects two type arguments", Some(span.clone()));
                    return None;
                }
                if args.len() != 2 {
                    self.diags
                        .push("map_get expects two arguments", Some(span.clone()));
                    return None;
                }
                let key_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let val_ty = resolve_type(&type_args[1], self.defs, self.diags)?;
                let map_ty = self.check_expr(&args[0])?.ty;
                match map_ty {
                    Type::Ref(inner) | Type::MutRef(inner)
                        if *inner == Type::Map(Box::new(key_ty.clone()), Box::new(val_ty.clone())) => {}
                    _ => {
                        self.diags
                            .push("map_get expects ref map[K,V]", Some(args[0].span.clone()));
                    }
                }
                let arg_ty = self.check_expr(&args[1])?.ty;
                if !type_eq(&arg_ty, &key_ty) {
                    self.diags
                        .push("map_get expects key type", Some(args[1].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Tuple(vec![val_ty, Type::Builtin(BuiltinType::Bool)]),
                    borrow: None,
                });
            }
            "map_set" => {
                if type_args.len() != 2 {
                    self.diags
                        .push("map_set expects two type arguments", Some(span.clone()));
                    return None;
                }
                if args.len() != 3 {
                    self.diags
                        .push("map_set expects three arguments", Some(span.clone()));
                    return None;
                }
                let key_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let val_ty = resolve_type(&type_args[1], self.defs, self.diags)?;
                let map_ty = self.check_expr(&args[0])?.ty;
                match map_ty {
                    Type::MutRef(inner)
                        if *inner == Type::Map(Box::new(key_ty.clone()), Box::new(val_ty.clone())) => {}
                    _ => {
                        self.diags
                            .push("map_set expects mutref map[K,V]", Some(args[0].span.clone()));
                    }
                }
                let arg_ty = self.check_expr(&args[1])?.ty;
                if !type_eq(&arg_ty, &key_ty) {
                    self.diags
                        .push("map_set expects key type", Some(args[1].span.clone()));
                }
                let val_arg_ty = self.check_expr(&args[2])?.ty;
                if !type_eq(&val_arg_ty, &val_ty) {
                    self.diags
                        .push("map_set expects value type", Some(args[2].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::Unit),
                    borrow: None,
                });
            }
            "map_del" => {
                if type_args.len() != 2 {
                    self.diags
                        .push("map_del expects two type arguments", Some(span.clone()));
                    return None;
                }
                if args.len() != 2 {
                    self.diags
                        .push("map_del expects two arguments", Some(span.clone()));
                    return None;
                }
                let key_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let val_ty = resolve_type(&type_args[1], self.defs, self.diags)?;
                let map_ty = self.check_expr(&args[0])?.ty;
                match map_ty {
                    Type::MutRef(inner)
                        if *inner == Type::Map(Box::new(key_ty.clone()), Box::new(val_ty.clone())) => {}
                    _ => {
                        self.diags
                            .push("map_del expects mutref map[K,V]", Some(args[0].span.clone()));
                    }
                }
                let arg_ty = self.check_expr(&args[1])?.ty;
                if !type_eq(&arg_ty, &key_ty) {
                    self.diags
                        .push("map_del expects key type", Some(args[1].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::Bool),
                    borrow: None,
                });
            }
            "map_len" => {
                if type_args.len() != 2 {
                    self.diags
                        .push("map_len expects two type arguments", Some(span.clone()));
                    return None;
                }
                if args.len() != 1 {
                    self.diags
                        .push("map_len expects one argument", Some(span.clone()));
                    return None;
                }
                let key_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let val_ty = resolve_type(&type_args[1], self.defs, self.diags)?;
                let map_ty = self.check_expr(&args[0])?.ty;
                match map_ty {
                    Type::Ref(inner) | Type::MutRef(inner)
                        if *inner
                            == Type::Map(Box::new(key_ty.clone()), Box::new(val_ty.clone())) => {}
                    _ => {
                        self.diags
                            .push("map_len expects ref map[K,V]", Some(args[0].span.clone()));
                    }
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I64),
                    borrow: None,
                });
            }
            _ => {}
        }
        None
    }

    fn borrow_base_ident(&self, expr: &Expr) -> Option<String> {
        match &expr.kind {
            ExprKind::Ident(name) => Some(name.clone()),
            ExprKind::Index { base, .. } => self.borrow_base_ident(base),
            ExprKind::Field { base, .. } => self.borrow_base_ident(base),
            _ => None,
        }
    }

    fn is_addressable_expr(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Ident(_) => true,
            ExprKind::Field { base, .. } => self.is_addressable_expr(base),
            ExprKind::Index { base, .. } => self.is_addressable_expr(base),
            ExprKind::Deref { .. } => true,
            _ => false,
        }
    }

    fn can_borrow_mut(&self, expr: &Expr) -> bool {
        if let Some(base) = self.borrow_base_ident(expr) {
            if let Some(info) = self.env.vars.get(&base) {
                // if the base is a shared view, don't allow &mut
                if info.view_of.is_some() && !info.view_is_mut {
                    return false;
                }
            }
        }
        true
    }

    fn check_borrow_target(&mut self, expr: &Expr) -> Option<(Type, Option<String>)> {
        match &expr.kind {
            ExprKind::Ident(_) => {
                let res = self.check_expr_no_move(expr)?;
                let base = self.borrow_base_ident(expr);
                Some((res.ty, base))
            }
            ExprKind::Field { base, name } => {
                let base_res = self.check_expr_no_move(base)?;
                if let Type::Named(type_name) = base_res.ty {
                    if let Some(TypeDefKind::Struct(def)) = self.defs.get(&type_name) {
                        if let Some((_, ty)) =
                            def.fields.iter().find(|(field, _)| field == name)
                        {
                            let base_name = self.borrow_base_ident(base);
                            return Some((ty.clone(), base_name));
                        }
                        let mut d = Diagnostic::new(
                            format!("unknown field `{}` on `{}`", name, type_name),
                            Some(expr.span.clone()),
                        )
                        .code(E_UNKNOWN_FIELD)
                        .label(expr.span.clone(), format!("unknown field `{}`", name))
                        .note("field names are case-sensitive");
                        let candidates = def
                            .fields
                            .iter()
                            .map(|(field, _)| field.clone())
                            .collect::<Vec<_>>();
                        if let Some(h) = suggest::did_you_mean(name, candidates) {
                            d = d.help(h);
                        }
                        self.diags.push_diag(d);
                        return None;
                    }
                }
                None
            }
            ExprKind::Index { base, index } => {
                let base_res = self.check_expr_no_move(base)?;
                let _ = self.check_expr(index)?;
                match slice_like_elem_type(&base_res.ty) {
                    Some(elem) => {
                        let base_name = self.borrow_base_ident(base);
                        Some((elem, base_name))
                    }
                    None => {
                        self.diags.push("indexing expects a slice", Some(expr.span.clone()));
                        None
                    }
                }
            }
            ExprKind::Deref { expr: inner } => {
                let inner_res = self.check_expr_no_move(inner)?;
                let base_name = self.borrow_base_ident(inner);
                match inner_res.ty {
                    Type::Ref(inner_ty) | Type::MutRef(inner_ty) => {
                        Some((*inner_ty, base_name))
                    }
                    _ => {
                        self.diags.push("deref expects ref type", Some(expr.span.clone()));
                        None
                    }
                }
            }
            _ => {
                self.diags.push(
                    "borrow target must be an addressable expression",
                    Some(expr.span.clone()),
                );
                None
            }
        }
    }

    fn check_assign_target(&mut self, expr: &Expr) -> Option<Type> {
        match &expr.kind {
            ExprKind::Ident(name) => {
                // Assignment target: allow re-initializing moved linear values.
                // Still check borrow conflicts.
                if let Some(var) = self.env.vars.get(name) {
                    if var.borrowed_mut > 0 {
                        self.diags.push(
                            "cannot assign while mutable borrow is active",
                            Some(expr.span.clone()),
                        );
                    }
                    if var.borrowed_shared > 0 {
                        self.diags.push(
                            "cannot assign while shared borrow is active",
                            Some(expr.span.clone()),
                        );
                    }
                    Some(var.ty.clone())
                } else if let Some(global) = self.globals.get(name) {
                    self.require_unsafe_operation(
                        &expr.span,
                        &format!("access to extern global `{}`", name),
                    );
                    Some(global.ty.clone())
                } else {
                    self.diags.push(
                        format!("undefined name `{}`", name),
                        Some(expr.span.clone()),
                    );
                    None
                }
            }
            ExprKind::Field { base, name } => {
                let base_res = self.check_expr_no_move(base)?;
                if let Type::Named(type_name) = base_res.ty {
                    if let Some(TypeDefKind::Struct(def)) = self.defs.get(&type_name) {
                        if let Some((_, ty)) =
                            def.fields.iter().find(|(field, _)| field == name)
                        {
                            return Some(ty.clone());
                        }
                        let mut d = Diagnostic::new(
                            format!("unknown field `{}` on `{}`", name, type_name),
                            Some(expr.span.clone()),
                        )
                        .code(E_UNKNOWN_FIELD)
                        .label(expr.span.clone(), format!("unknown field `{}`", name))
                        .note("field names are case-sensitive");
                        let candidates = def
                            .fields
                            .iter()
                            .map(|(field, _)| field.clone())
                            .collect::<Vec<_>>();
                        if let Some(h) = suggest::did_you_mean(name, candidates) {
                            d = d.help(h);
                        }
                        self.diags.push_diag(d);
                        return None;
                    }
                }
                None
            }
            ExprKind::Index { base, index } => {
                let base_res = self.check_expr_no_move(base)?;
                let _ = self.check_expr(index)?;
                match slice_like_elem_type(&base_res.ty) {
                    Some(elem) => Some(elem),
                    None => {
                        self.diags.push("indexing expects a slice", Some(expr.span.clone()));
                        None
                    }
                }
            }
            ExprKind::Deref { expr: inner } => {
                let inner_res = self.check_expr_no_move(inner)?;
                match inner_res.ty {
                    Type::Ref(inner) | Type::MutRef(inner) => Some(*inner),
                    _ => {
                        self.diags.push("deref expects ref type", Some(expr.span.clone()));
                        None
                    }
                }
            }
            _ => {
                self.diags
                    .push("assignment target must be addressable", Some(expr.span.clone()));
                None
            }
        }
    }

    fn check_expr_with_mode(&mut self, expr: &Expr, consume: bool) -> Option<ExprResult> {
        let result = match &expr.kind {
            ExprKind::Bool(_) => Some(ExprResult {
                ty: Type::Builtin(BuiltinType::Bool),
                borrow: None,
            }),
            ExprKind::Int(_) => Some(ExprResult {
                ty: Type::Builtin(BuiltinType::I32),
                borrow: None,
            }),
            ExprKind::Float(_) => Some(ExprResult {
                ty: Type::Builtin(BuiltinType::F64),
                borrow: None,
            }),
            ExprKind::Char(_) => Some(ExprResult {
                ty: Type::Builtin(BuiltinType::Char),
                borrow: None,
            }),
            ExprKind::String(_) => Some(ExprResult {
                ty: Type::Builtin(BuiltinType::String),
                borrow: None,
            }),
            ExprKind::Nil => Some(ExprResult {
                ty: Type::Builtin(BuiltinType::Error),
                borrow: None,
            }),
            ExprKind::Ident(name) => {
                if let Some(var) = self.env.vars.get_mut(name) {
                    if var.borrowed_mut > 0 {
                        self.diags.push(
                            "cannot access value while mutable borrow is active",
                            Some(expr.span.clone()),
                        );
                    }
                    if var.class == TypeClass::Linear {
                        if consume {
                            match var.state {
                                LinearState::Alive => {
                                    var.state = LinearState::Moved;
                                }
                                _ => {
                                    self.diags.push(
                                        format!("use after move of `{}`", name),
                                        Some(expr.span.clone()),
                                    );
                                }
                            }
                        } else if var.state != LinearState::Alive {
                            self.diags
                                .push(format!("use after move of `{}`", name), Some(expr.span.clone()));
                        }
                    }
                    Some(ExprResult {
                        ty: var.ty.clone(),
                        borrow: None,
                    })
                } else {
                    if let Some(sig) = self.funcs.get(name) {
                        return self.record_expr(
                            expr,
                            ExprResult {
                                ty: Self::function_ptr_type(sig),
                                borrow: None,
                            },
                        );
                    }
                    if let Some(global) = self.globals.get(name) {
                        self.require_unsafe_operation(
                            &expr.span,
                            &format!("access to extern global `{}`", name),
                        );
                        return self.record_expr(
                            expr,
                            ExprResult {
                                ty: global.ty.clone(),
                                borrow: None,
                            },
                        );
                    }
                    let mut candidates: Vec<String> =
                        self.env.vars.keys().cloned().collect();
                    candidates.extend(self.funcs.keys().cloned());
                    candidates.extend(self.globals.keys().cloned());
                    let mut d = Diagnostic::new(
                        format!("undefined name `{}`", name),
                        Some(expr.span.clone()),
                    )
                    .code(E_UNDEFINED_NAME)
                    .label(expr.span.clone(), "unknown name")
                    .note("names are case-sensitive");
                    if let Some(h) = suggest::did_you_mean(name, candidates) {
                        d = d.help(h);
                    }
                    self.diags.push_diag(d);
                    None
                }
            }
            ExprKind::StructLit { name, fields } => {
                let def = match self.defs.get(name) {
                    Some(TypeDefKind::Struct(def)) => def,
                    _ => {
                        let mut d = Diagnostic::new(
                            format!("unknown struct `{}`", name),
                            Some(expr.span.clone()),
                        )
                        .code(E_UNKNOWN_TYPE)
                        .label(expr.span.clone(), "unknown struct");
                        self.diags.push_diag(d);
                        return None;
                    }
                };
                let mut seen = std::collections::HashSet::<String>::new();
                let mut field_map: std::collections::HashMap<String, &Expr> =
                    std::collections::HashMap::new();
                for (fname, fexpr) in fields.iter() {
                    if !seen.insert(fname.clone()) {
                        self.diags.push(
                            format!("duplicate field `{}` in struct literal", fname),
                            Some(fexpr.span.clone()),
                        );
                        continue;
                    }
                    field_map.insert(fname.clone(), fexpr);
                }

                // check fields
                for (field_name, field_ty) in &def.fields {
                    match field_map.get(field_name) {
                        Some(fexpr) => {
                            let res = self.check_expr(fexpr)?;
                            if !type_eq(&res.ty, field_ty) {
                                self.diags.push(
                                    "field initializer type mismatch",
                                    Some(fexpr.span.clone()),
                                );
                            }
                        }
                        None => {
                            self.diags.push(
                                format!("missing field `{}` in struct literal", field_name),
                                Some(expr.span.clone()),
                            );
                        }
                    }
                }

                // unknown fields
                for (fname, fexpr) in fields.iter() {
                    if !def.fields.iter().any(|(n, _)| n == fname) {
                        let mut d = Diagnostic::new(
                            format!("unknown field `{}` on `{}`", fname, name),
                            Some(fexpr.span.clone()),
                        )
                        .code(E_UNKNOWN_FIELD)
                        .label(fexpr.span.clone(), format!("unknown field `{}`", fname))
                        .note("field names are case-sensitive");
                        let candidates = def
                            .fields
                            .iter()
                            .map(|(field, _)| field.clone())
                            .collect::<Vec<_>>();
                        if let Some(h) = suggest::did_you_mean(fname, candidates) {
                            d = d.help(h);
                        }
                        self.diags.push_diag(d);
                    }
                }

                Some(ExprResult {
                    ty: Type::Named(name.clone()),
                    borrow: None,
                })
            }
            ExprKind::Tuple(items) => {
                let mut tys = Vec::new();
                for item in items {
                    let res = self.check_expr(item)?;
                    tys.push(res.ty);
                }
                if tys.len() == 2 && tys[1] == Type::Builtin(BuiltinType::Error) {
                    Some(ExprResult {
                        ty: Type::Result(Box::new(tys[0].clone()), Box::new(tys[1].clone())),
                        borrow: None,
                    })
                } else {
                    Some(ExprResult {
                        ty: Type::Tuple(tys),
                        borrow: None,
                    })
                }
            }
            ExprKind::Block(block) => {
                let ty = self.check_block(block)?;
                Some(ExprResult { ty, borrow: None })
            }
            ExprKind::UnsafeBlock(block) => {
                self.unsafe_depth += 1;
                let ty = self.check_block(block);
                self.unsafe_depth = self.unsafe_depth.saturating_sub(1);
                ty.map(|ty| ExprResult { ty, borrow: None })
            }
            ExprKind::If { cond, then_block, else_block } => {
                let cond_ty = self.check_expr(cond)?;
                if !type_eq(&cond_ty.ty, &Type::Builtin(BuiltinType::Bool)) {
                    self.diags.push("if condition must be bool", Some(cond.span.clone()));
                }
                let base_env = self.env.clone();
                self.env = base_env.clone();
                let then_ty = self.check_block(then_block)?;
                let then_env = self.env.clone();
                self.env = base_env.clone();
                let else_ty = if let Some(block) = else_block {
                    self.check_block(block)?
                } else {
                    Type::Builtin(BuiltinType::Unit)
                };
                let else_env = self.env.clone();
                self.env = self.join_env(&then_env, &else_env, &expr.span);
                if !type_eq(&then_ty, &else_ty) {
                    self.diags.push("if branches must have same type", Some(expr.span.clone()));
                }
                Some(ExprResult { ty: then_ty, borrow: None })
            }
            ExprKind::Match { scrutinee, arms } => {
                // Do not consume the scrutinee here; treat as borrow-like use so linear
                // values can be matched without false move errors.
                let scrut = self.check_expr_no_move(scrutinee)?;
                let scrut_ty = scrut.ty;
                let base_env = self.env.clone();
                let mut arm_ty: Option<Type> = None;
                let mut joined_env = base_env.clone();
                for arm in arms {
                    self.env = base_env.clone();
                    self.env.enter_scope();
                    self.check_match_pattern(&arm.pattern, &scrut_ty, &arm.span);
                    let ty = match &arm.body {
                        BlockOrExpr::Block(block) => self.check_block(block),
                        BlockOrExpr::Expr(expr) => self.check_expr(expr).map(|r| r.ty),
                    };
                    self.drop_scope();
                    let arm_env = self.env.clone();
                    joined_env = self.join_env(&joined_env, &arm_env, &arm.span);
                    if let Some(ty) = ty {
                        if let Some(existing) = &arm_ty {
                            if !type_eq(existing, &ty) {
                                self.diags.push(
                                    format!(
                                        "match arms must have same type (expected {}, found {})",
                                        existing.pretty(),
                                        ty.pretty()
                                    ),
                                    Some(arm.span.clone()),
                                );
                            }
                        } else {
                            arm_ty = Some(ty);
                        }
                    }
                }
                self.env = joined_env;
                let result_ty = arm_ty.unwrap_or(Type::Builtin(BuiltinType::Unit));
                if result_ty != Type::Builtin(BuiltinType::Unit)
                    && !self.match_is_exhaustive(&scrut_ty, arms)
                {
                    self.diags.push(
                        "match expression must be exhaustive",
                        Some(expr.span.clone()),
                    );
                }
                Some(ExprResult {
                    ty: result_ty,
                    borrow: None,
                })
            }
            ExprKind::Call { callee, type_args, args } => {
                if let ExprKind::Field { base, name: variant } = &callee.kind {
                    if let ExprKind::Ident(enum_name) = &base.kind {
                        if enum_name == "Result" || enum_name == "result" {
                            if let Some(res) =
                                self.check_result_ctor(variant, type_args, args, &expr.span)
                            {
                                return self.record_expr(expr, res);
                            }
                            return None;
                        }
                        if !type_args.is_empty() {
                            self.diags.push(
                                "type arguments not supported for enum constructors",
                                Some(expr.span.clone()),
                            );
                        }
                        if let Some(TypeDefKind::Enum(def)) = self.defs.get(enum_name) {
                            let fields = match def.variants.iter().find(|(name, _)| name == variant)
                            {
                                Some((_, fields)) => fields,
                                None => {
                                    let mut d = Diagnostic::new(
                                        "unknown enum variant",
                                        Some(expr.span.clone()),
                                    )
                                    .code(E_UNKNOWN_VARIANT)
                                    .label(expr.span.clone(), "unknown variant");
                                    let candidates = def
                                        .variants
                                        .iter()
                                        .map(|(name, _)| name.clone())
                                        .collect::<Vec<_>>();
                                    if let Some(h) = suggest::did_you_mean(variant, candidates) {
                                        d = d.help(h);
                                    }
                                    self.diags.push_diag(d);
                                    return None;
                                }
                            };
                            if fields.len() != args.len() {
                                self.diags
                                    .push("enum constructor arity mismatch", Some(expr.span.clone()));
                            }
                            for (idx, arg) in args.iter().enumerate() {
                                let res = self.check_expr(arg)?;
                                let param_ty = fields.get(idx).unwrap_or(&Type::Builtin(BuiltinType::Unit));
                                if view_arg_not_allowed(self.defs, &res.ty, param_ty) {
                                    self.diags.push(
                                        "view arguments can only be passed to intrinsics",
                                        Some(arg.span.clone()),
                                    );
                                }
                                if !type_eq(&res.ty, param_ty) {
                                    self.diags.push("argument type mismatch", Some(arg.span.clone()));
                                }
                            }
                            return self.record_expr(
                                expr,
                                ExprResult {
                                    ty: Type::Named(enum_name.clone()),
                                    borrow: None,
                                },
                            );
                        }
                    }
                    // Not an enum ctor: treat as method-like call (recv-first free function).
                    let recv_res = match self.check_expr_no_move(base) {
                        Some(res) => res,
                        None => return None,
                    };
                    let recv_ty = recv_res.ty.clone();

                    if let Some(sig) = self.funcs.get(variant) {
                        if let Some(param0) = sig.params.first() {
                            if self.recv_param_compatible(&recv_ty, param0) {
                                if sig.is_extern || sig.is_unsafe {
                                    self.require_unsafe_operation(
                                        &expr.span,
                                        &format!("call to `{}`", variant),
                                    );
                                }
                                let fixed_args = sig.params.len().saturating_sub(1);
                                if (!sig.is_variadic && args.len() != fixed_args)
                                    || (sig.is_variadic && args.len() < fixed_args)
                                {
                                    self.diags.push(
                                        "argument count mismatch",
                                        Some(expr.span.clone()),
                                    );
                                }
                                for (idx, arg) in args.iter().enumerate() {
                                    let res = self.check_expr(arg)?;
                                    if let Some(param_ty) = sig.params.get(idx + 1) {
                                        if view_arg_not_allowed(self.defs, &res.ty, param_ty) {
                                            self.diags.push(
                                                "view arguments can only be passed to intrinsics",
                                                Some(arg.span.clone()),
                                            );
                                        }
                                        if !type_eq(&res.ty, param_ty) {
                                            self.diags.push(
                                                "argument type mismatch",
                                                Some(arg.span.clone()),
                                            );
                                        }
                                    } else if contains_view(self.defs, &res.ty) {
                                        self.diags.push(
                                            "view arguments are not allowed in variadic positions",
                                            Some(arg.span.clone()),
                                        );
                                    }
                                }
                                return self.record_expr(
                                    expr,
                                    ExprResult {
                                        ty: sig.ret.clone(),
                                        borrow: None,
                                    },
                                );
                            }

                            let need_mut = matches!(param0, Type::MutRef(_));
                            if matches!(param0, Type::Ref(_) | Type::MutRef(_))
                                && !self.is_addressable_expr(base)
                            {
                                let fail = AutoadjFail {
                                    kind: AutoadjFailKind::NotAddressable { need_mut },
                                    recv_span: base.span.clone(),
                                    method: variant.clone(),
                                    recv_base_name: self.borrow_base_ident(base),
                                    recv_ty,
                                    param0_ty: param0.clone(),
                                };
                                self.diag_autoadj_fail(fail);
                                return None;
                            }
                            if matches!(param0, Type::MutRef(_)) && !self.can_borrow_mut(base) {
                                let fail = AutoadjFail {
                                    kind: AutoadjFailKind::NotMutable,
                                    recv_span: base.span.clone(),
                                    method: variant.clone(),
                                    recv_base_name: self.borrow_base_ident(base),
                                    recv_ty,
                                    param0_ty: param0.clone(),
                                };
                                self.diag_autoadj_fail(fail);
                                return None;
                            }
                        }
                    }

                    self.diag_unknown_method(callee.span.clone(), Some(recv_ty), variant);
                    return None;
                }
                if let ExprKind::Ident(callee_name) = &callee.kind {
                    if let Some(res) =
                        self.check_intrinsic_call(callee_name, type_args, args, &expr.span)
                    {
                        return self.record_expr(expr, res);
                    }
                    if !type_args.is_empty() {
                        self.diags.push("type arguments not supported for this call", Some(expr.span.clone()));
                    }
                    if let Some(sig) = self.funcs.get(callee_name) {
                        if sig.is_extern || sig.is_unsafe {
                            self.require_unsafe_operation(
                                &expr.span,
                                &format!("call to `{}`", callee_name),
                            );
                        }
                        if (!sig.is_variadic && sig.params.len() != args.len())
                            || (sig.is_variadic && args.len() < sig.params.len())
                        {
                            self.diags.push("argument count mismatch", Some(expr.span.clone()));
                        }
                        for (idx, arg) in args.iter().enumerate() {
                            let res = self.check_expr(arg)?;
                            if let Some(param_ty) = sig.params.get(idx) {
                                if view_arg_not_allowed(self.defs, &res.ty, param_ty) {
                                    self.diags.push("view arguments can only be passed to intrinsics", Some(arg.span.clone()));
                                }
                                if !type_eq(&res.ty, param_ty) {
                                    self.diags.push("argument type mismatch", Some(arg.span.clone()));
                                }
                            } else if contains_view(self.defs, &res.ty) {
                                self.diags.push(
                                    "view arguments are not allowed in variadic positions",
                                    Some(arg.span.clone()),
                                );
                            }
                        }
                        return self.record_expr(
                            expr,
                            ExprResult {
                                ty: sig.ret.clone(),
                                borrow: None,
                            },
                        );
                    }
                }

                if !type_args.is_empty() {
                    self.diags.push("type arguments not supported for this call", Some(expr.span.clone()));
                }
                let callee_ty = self.check_expr_no_move(callee)?.ty;
                match callee_ty {
                    Type::FnPtr {
                        params,
                        ret,
                        is_variadic,
                    } => {
                        if (!is_variadic && params.len() != args.len())
                            || (is_variadic && args.len() < params.len())
                        {
                            self.diags.push("argument count mismatch", Some(expr.span.clone()));
                        }
                        for (idx, arg) in args.iter().enumerate() {
                            let res = self.check_expr(arg)?;
                            if let Some(param_ty) = params.get(idx) {
                                if view_arg_not_allowed(self.defs, &res.ty, param_ty) {
                                    self.diags.push(
                                        "view arguments can only be passed to intrinsics",
                                        Some(arg.span.clone()),
                                    );
                                }
                                if !type_eq(&res.ty, param_ty) {
                                    self.diags.push("argument type mismatch", Some(arg.span.clone()));
                                }
                            } else if contains_view(self.defs, &res.ty) {
                                self.diags.push(
                                    "view arguments are not allowed in variadic positions",
                                    Some(arg.span.clone()),
                                );
                            }
                        }
                        Some(ExprResult {
                            ty: *ret,
                            borrow: None,
                        })
                    }
                    _ => {
                        self.diags.push(
                            "call target must be a function or function pointer",
                            Some(callee.span.clone()),
                        );
                        None
                    }
                }
            }
            ExprKind::Field { base, name: field_name } => {
                if let ExprKind::Ident(enum_name) = &base.kind {
                    if !self.env.vars.contains_key(enum_name) {
                        if let Some(TypeDefKind::Enum(def)) = self.defs.get(enum_name) {
                            let fields = match def.variants.iter().find(|(name, _)| name == field_name) {
                                Some((_, fields)) => fields,
                                None => {
                                    let mut d = Diagnostic::new(
                                        "unknown enum variant",
                                        Some(expr.span.clone()),
                                    )
                                    .code(E_UNKNOWN_VARIANT)
                                    .label(expr.span.clone(), "unknown variant");
                                    let candidates = def
                                        .variants
                                        .iter()
                                        .map(|(name, _)| name.clone())
                                        .collect::<Vec<_>>();
                                    if let Some(h) = suggest::did_you_mean(field_name, candidates) {
                                        d = d.help(h);
                                    }
                                    self.diags.push_diag(d);
                                    return None;
                                }
                            };
                            if !fields.is_empty() {
                                self.diags.push(
                                    "enum variant requires arguments",
                                    Some(expr.span.clone()),
                                );
                                return None;
                            }
                            return self.record_expr(
                                expr,
                                ExprResult {
                                    ty: Type::Named(enum_name.clone()),
                                    borrow: None,
                                },
                            );
                        }
                    }
                }
                let base_res = self.check_expr_no_move(base)?;
                if let Type::Named(type_name) = base_res.ty {
                    if let Some(TypeDefKind::Struct(def)) = self.defs.get(&type_name) {
                        if let Some((_, ty)) =
                            def.fields.iter().find(|(field, _)| field == field_name)
                        {
                            return self.record_expr(
                                expr,
                                ExprResult {
                                    ty: ty.clone(),
                                    borrow: None,
                                },
                            );
                        }
                        let mut d = Diagnostic::new(
                            format!("unknown field `{}` on `{}`", field_name, type_name),
                            Some(expr.span.clone()),
                        )
                        .code(E_UNKNOWN_FIELD)
                        .label(expr.span.clone(), format!("unknown field `{}`", field_name))
                        .note("field names are case-sensitive");
                        let candidates = def
                            .fields
                            .iter()
                            .map(|(field, _)| field.clone())
                            .collect::<Vec<_>>();
                        if let Some(h) = suggest::did_you_mean(field_name, candidates) {
                            d = d.help(h);
                        }
                        self.diags.push_diag(d);
                        return None;
                    }
                }
                None
            }
            ExprKind::Index { base, index } => {
                let base_res = self.check_expr_no_move(base)?;
                let _ = self.check_expr(index)?;
                if let Type::Tuple(items) = &base_res.ty {
                    let idx = match &index.kind {
                        ExprKind::Int(v) => v.parse::<usize>().ok(),
                        _ => None,
                    };
                    let Some(idx) = idx else {
                        self.diags.push(
                            "tuple index must be integer literal",
                            Some(index.span.clone()),
                        );
                        return None;
                    };
                    if let Some(ty) = items.get(idx) {
                        return Some(ExprResult {
                            ty: ty.clone(),
                            borrow: None,
                        });
                    }
                    self.diags.push(
                        format!(
                            "tuple index out of bounds (len={}, idx={})",
                            items.len(),
                            idx
                        ),
                        Some(index.span.clone()),
                    );
                    return None;
                }
                match slice_like_elem_type(&base_res.ty) {
                    Some(elem) => {
                        if !is_copy_type(self.defs, &elem) {
                            self.diags.push(
                                "cannot index linear element by value; use & or &mut",
                                Some(expr.span.clone()),
                            );
                        }
                        Some(ExprResult { ty: elem, borrow: None })
                    }
                    None => {
                        self.diags.push("indexing expects a slice", Some(expr.span.clone()));
                        None
                    }
                }
            }
            ExprKind::Unary { op, expr: inner } => {
                let inner_res = self.check_expr(inner)?;
                match op {
                    UnaryOp::Neg => Some(ExprResult { ty: inner_res.ty, borrow: None }),
                    UnaryOp::Not => Some(ExprResult { ty: Type::Builtin(BuiltinType::Bool), borrow: None }),
                }
            }
            ExprKind::Binary { op, left, right } => {
                let left_res = self.check_expr(left)?;
                let right_res = self.check_expr(right)?;
                let promoted = promote_int_types(&left_res.ty, &right_res.ty);
                if !type_eq(&left_res.ty, &right_res.ty) && promoted.is_none() {
                    self.diags.push("binary operand type mismatch", Some(expr.span.clone()));
                }
                let result_ty = match op {
                    BinaryOp::Eq
                    | BinaryOp::NotEq
                    | BinaryOp::Lt
                    | BinaryOp::Lte
                    | BinaryOp::Gt
                    | BinaryOp::Gte
                    | BinaryOp::And
                    | BinaryOp::Or => Type::Builtin(BuiltinType::Bool),
                    _ => promoted.unwrap_or_else(|| left_res.ty.clone()),
                };
                Some(ExprResult { ty: result_ty, borrow: None })
            }
            ExprKind::Borrow { is_mut, expr: inner } => {
                let (target_ty, base_name) = self.check_borrow_target(inner)?;
                let ty = if *is_mut {
                    Type::MutRef(Box::new(target_ty))
                } else {
                    Type::Ref(Box::new(target_ty))
                };
                if let Some(base) = base_name {
                    if let Some(base_info) = self.env.vars.get(&base) {
                        if *is_mut {
                            if base_info.borrowed_shared > 0 || base_info.borrowed_mut > 0 {
                                self.diags.push(
                                    "mutable borrow conflicts with existing borrows",
                                    Some(expr.span.clone()),
                                );
                            }
                        } else if base_info.borrowed_mut > 0 {
                            self.diags.push(
                                "shared borrow conflicts with mutable borrow",
                                Some(expr.span.clone()),
                            );
                        }
                    }
                    Some(ExprResult {
                        ty,
                        borrow: Some(BorrowInfo { base, is_mut: *is_mut }),
                    })
                } else {
                    self.diags.push(
                        "borrow target must be an identifier or index expression",
                        Some(expr.span.clone()),
                    );
                    Some(ExprResult { ty, borrow: None })
                }
            }
            ExprKind::Deref { expr: inner } => {
                let inner_res = self.check_expr(inner)?;
                match inner_res.ty {
                    Type::Ref(inner) | Type::MutRef(inner) => {
                        if !is_copy_type(self.defs, &inner) {
                            self.diags.push(
                                "cannot deref linear values by value",
                                Some(expr.span.clone()),
                            );
                        }
                        Some(ExprResult { ty: *inner, borrow: None })
                    }
                    _ => {
                        self.diags.push("deref expects ref type", Some(expr.span.clone()));
                        None
                    }
                }
            }
            ExprKind::Try { expr: inner } => {
                let inner_res = self.check_expr(inner)?;
                let ret_ok = matches!(self.sig.ret, Type::Builtin(BuiltinType::Error))
                    || matches!(&self.sig.ret, Type::Result(_, err) if **err == Type::Builtin(BuiltinType::Error))
                    || matches!(&self.sig.ret, Type::Tuple(items) if items.len() == 2 && items[1] == Type::Builtin(BuiltinType::Error));
                if !ret_ok {
                    self.diags.push(
                        "`?` can only be used in functions returning error or (T, error)",
                        Some(expr.span.clone()),
                    );
                }
                match inner_res.ty {
                    Type::Builtin(BuiltinType::Error) => Some(ExprResult {
                        ty: Type::Builtin(BuiltinType::Unit),
                        borrow: None,
                    }),
                    Type::Result(ok_ty, err_ty) => {
                        if *err_ty != Type::Builtin(BuiltinType::Error) {
                            self.diags
                                .push("second value must be error for ?", Some(expr.span.clone()));
                        }
                        Some(ExprResult {
                            ty: *ok_ty,
                            borrow: None,
                        })
                    }
                    Type::Tuple(items) if items.len() == 2 => {
                        let err_ty = &items[1];
                        if !type_eq(err_ty, &Type::Builtin(BuiltinType::Error)) {
                            self.diags.push("second value must be error for ?", Some(expr.span.clone()));
                        }
                        Some(ExprResult { ty: items[0].clone(), borrow: None })
                    }
                    _ => {
                        self.diags.push("`?` expects error or (T, error)", Some(expr.span.clone()));
                        None
                    }
                }
            }
            ExprKind::Send { chan, value } => {
                let chan_res = self.check_expr_no_move(chan)?;
                let value_res = self.check_expr(value)?;
                match chan_res.ty {
                    Type::Chan(inner) => {
                        if contains_view(self.defs, &value_res.ty) {
                            self.diags.push("cannot send view types", Some(expr.span.clone()));
                        }
                        if !type_eq(&value_res.ty, &inner) {
                            self.diags.push("send type mismatch", Some(expr.span.clone()));
                        }
                        Some(ExprResult { ty: Type::Builtin(BuiltinType::Unit), borrow: None })
                    }
                    _ => {
                        self.diags.push("send expects chan[T]", Some(expr.span.clone()));
                        None
                    }
                }
            }
            ExprKind::Recv { chan } => {
                let chan_res = self.check_expr_no_move(chan)?;
                match chan_res.ty {
                    Type::Chan(inner) => Some(ExprResult {
                        ty: Type::Tuple(vec![*inner, Type::Builtin(BuiltinType::Bool)]),
                        borrow: None,
                    }),
                    _ => {
                        self.diags.push("recv expects chan[T]", Some(expr.span.clone()));
                        None
                    }
                }
            }
            ExprKind::Close { chan } => {
                let chan_res = self.check_expr_no_move(chan)?;
                match chan_res.ty {
                    Type::Chan(_) => Some(ExprResult { ty: Type::Builtin(BuiltinType::Error), borrow: None }),
                    _ => {
                        self.diags.push("close expects chan[T]", Some(expr.span.clone()));
                        None
                    }
                }
            }
            ExprKind::After { ms } => {
                let _ = self.check_expr(ms)?;
                Some(ExprResult {
                    ty: Type::Chan(Box::new(Type::Builtin(BuiltinType::Unit))),
                    borrow: None,
                })
            }
        };
        if let Some(res) = &result {
            self.expr_types.insert(expr.id, res.ty.clone());
        }
        result
    }

    fn drop_scope(&mut self) {
        let locals = self.env.exit_scope();
        for name in locals {
            if let Some(info) = self.env.vars.remove(&name) {
                if let Some(base) = info.view_of {
                    if let Some(base_info) = self.env.vars.get_mut(&base) {
                        if info.view_is_mut {
                            base_info.borrowed_mut = base_info.borrowed_mut.saturating_sub(1);
                        } else {
                            base_info.borrowed_shared = base_info.borrowed_shared.saturating_sub(1);
                        }
                    }
                }
            }
        }
    }

    fn drop_all_alive(&mut self) {
        for info in self.env.vars.values_mut() {
            if info.class == TypeClass::Linear && info.state == LinearState::Alive {
                info.state = LinearState::Dropped;
            }
        }
    }

    fn join_env(&mut self, left: &Env, right: &Env, span: &Span) -> Env {
        let mut joined = left.clone();
        for (name, right_info) in &right.vars {
            if let Some(left_info) = joined.vars.get_mut(name) {
                if left_info.class == TypeClass::Linear {
                    left_info.state = join_state(left_info.state, right_info.state);
                }
                if left_info.borrowed_mut != right_info.borrowed_mut
                    || left_info.borrowed_shared != right_info.borrowed_shared
                {
                    self.diags.push(
                        "borrow state mismatch across branches",
                        Some(span.clone()),
                    );
                    left_info.borrowed_mut =
                        left_info.borrowed_mut.max(right_info.borrowed_mut);
                    left_info.borrowed_shared =
                        left_info.borrowed_shared.max(right_info.borrowed_shared);
                }
            }
        }
        joined
    }
}

fn type_eq(a: &Type, b: &Type) -> bool {
    a == b
}

fn promote_int_types(a: &Type, b: &Type) -> Option<Type> {
    if a == b {
        return Some(a.clone());
    }
    match (a, b) {
        (Type::Builtin(BuiltinType::I64), Type::Builtin(BuiltinType::I32))
        | (Type::Builtin(BuiltinType::I32), Type::Builtin(BuiltinType::I64)) => {
            Some(Type::Builtin(BuiltinType::I64))
        }
        (Type::Builtin(BuiltinType::U64), Type::Builtin(BuiltinType::U32))
        | (Type::Builtin(BuiltinType::U32), Type::Builtin(BuiltinType::U64)) => {
            Some(Type::Builtin(BuiltinType::U64))
        }
        _ => None,
    }
}

fn join_state(a: LinearState, b: LinearState) -> LinearState {
    if a == LinearState::Moved || b == LinearState::Moved {
        LinearState::Moved
    } else if a == LinearState::Dropped || b == LinearState::Dropped {
        LinearState::Dropped
    } else if a == LinearState::Uninit || b == LinearState::Uninit {
        LinearState::Uninit
    } else {
        LinearState::Alive
    }
}
