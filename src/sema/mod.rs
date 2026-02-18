pub mod types;

use std::collections::{HashMap, HashSet};

use crate::frontend::ast::*;
use crate::frontend::diagnostic::{
    Diagnostic, Diagnostics, E_UNDEFINED_NAME, E_UNKNOWN_FIELD, E_UNKNOWN_FUNCTION,
    E_UNKNOWN_METHOD, E_UNKNOWN_TYPE, E_UNKNOWN_VARIANT,
};
use crate::frontend::diagnostic::{E1104, E1105};
use crate::frontend::suggest;
use crate::frontend::symbols::logical_method_name;
use crate::intrinsics::{
    intrinsic_args_error, intrinsic_internal_std_only_error, intrinsic_type_args_error,
    is_intrinsic_name,
};
use crate::sema::types::{
    BuiltinType, EnumDef, LayoutInfo, RefKind, ReprInt, StructDef, StructField, Type, TypeClass,
    TypeDefKind, TypeDefs, builtin_from_name, builtin_names, promote_int_types, type_eq,
};

const DIAG_VIEW_ESCAPE: &str = "view value cannot escape; use own[T] or alias[T]";
const DIAG_ALIAS_MUT_BORROW: &str = "cannot mutably borrow alias[T]";
const DIAG_ALIAS_TO_OWN: &str = "cannot convert alias[T] to own[T]";
const DIAG_OWN_USE_AFTER_MOVE: &str = "use after move of own[T]";
const DIAG_FREEZE_CONSUMES_OWNER: &str = "freeze consumes owner handle";

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
pub struct GlobalVarSig {
    pub ty: Type,
    pub init: GlobalInit,
}

#[derive(Clone, Debug)]
pub enum GlobalInit {
    Const(ConstValue),
    Zero,
}

#[derive(Clone, Debug)]
pub enum ConstValue {
    Bool(bool),
    Int(i128),
    UInt(u128),
    Float(f64),
    Char(char),
    String(String),
}

#[derive(Clone, Debug)]
pub struct ConstSig {
    pub ty: Type,
    pub value: ConstValue,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub file: FileAst,
    pub types: TypeDefs,
    pub functions: HashMap<String, FunctionSig>,
    pub extern_globals: HashMap<String, ExternGlobalSig>,
    pub globals: HashMap<String, GlobalVarSig>,
    pub consts: HashMap<String, ConstSig>,
    pub expr_types: HashMap<ExprId, Type>,
}

pub fn analyze(file: &FileAst, std_funcs: &HashSet<String>) -> Result<Program, Diagnostics> {
    let mut diags = Diagnostics::default();
    let mut types = TypeDefs::default();
    let mut type_order = Vec::new();
    let mut expr_types: HashMap<ExprId, Type> = HashMap::new();
    for item in &file.items {
        match item {
            Item::Struct(def) => {
                if let Some(other) = &def.layout.repr_other {
                    diags.push(
                        format!("unsupported repr `{}`", other),
                        Some(def.span.clone()),
                    );
                }
                if def.layout.bitfield && !def.layout.repr_c {
                    diags.push("bitfield requires repr(C)", Some(def.span.clone()));
                };
                if def.layout.pack.is_some() && !def.layout.repr_c {
                    diags.push("pack(N) requires repr(C)", Some(def.span.clone()));
                };
                if def.layout.repr_int.is_some() {
                    diags.push(
                        "integer repr is only supported on enum",
                        Some(def.span.clone()),
                    );
                }
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
                if let Some(other) = &def.layout.repr_other {
                    diags.push(
                        format!("unsupported repr `{}`", other),
                        Some(def.span.clone()),
                    );
                }
                if def.layout.pack.is_some() || def.layout.bitfield {
                    diags.push(
                        "pack/bitfield are only supported on struct",
                        Some(def.span.clone()),
                    );
                }
                if def.layout.repr_transparent {
                    diags.push(
                        "repr(transparent) is only supported on struct",
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
            Item::TypeAlias(alias) => {
                if alias.is_trait {
                    types.insert_trait_name(alias.name.clone());
                }
            }
            _ => {}
        }
    }

    for item in &file.items {
        if let Item::TypeAlias(alias) = item {
            if builtin_from_name(&alias.name).is_some() {
                diags.push(
                    format!("type alias `{}` conflicts with builtin type", alias.name),
                    Some(alias.span.clone()),
                );
                continue;
            }
            if types.get(&alias.name).is_some() {
                diags.push(
                    format!("type alias `{}` conflicts with existing type", alias.name),
                    Some(alias.span.clone()),
                );
                continue;
            }
            if let Some(resolved) = resolve_type(&alias.ty, &types, &mut diags) {
                types.insert_alias(alias.name.clone(), resolved);
            }
        }
    }

    for item in &file.items {
        match item {
            Item::Struct(def) => {
                let mut fields = Vec::new();
                for field in &def.fields {
                    let ty = resolve_type(&field.ty, &types, &mut diags);
                    if let Some(ty) = ty {
                        if types.contains_view(&ty) {
                            diags.push(DIAG_VIEW_ESCAPE, Some(field.span.clone()));
                        }
                        if def.layout.bitfield
                            && !(matches!(ty, Type::Builtin(BuiltinType::Bool | BuiltinType::Char))
                                || ty.is_integer())
                        {
                            diags.push(
                                "bitfield struct fields must be integer-like types",
                                Some(field.span.clone()),
                            );
                        }
                        fields.push(StructField {
                            name: field.name.clone(),
                            ty,
                            vis: field.vis,
                        });
                    }
                }
                if def.layout.repr_transparent && fields.len() != 1 {
                    diags.push(
                        "repr(transparent) struct must have exactly one field",
                        Some(def.span.clone()),
                    );
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
                            if types.contains_view(&resolved) {
                                diags.push(DIAG_VIEW_ESCAPE, Some(var.span.clone()));
                            }
                            field_tys.push(resolved);
                        }
                    }
                    variants.push((var.name.clone(), field_tys));
                }
                if let Some(repr) = def.layout.repr_int
                    && !repr_int_can_represent_variant_count(repr, variants.len())
                {
                    diags.push(
                        "repr(integer) enum has too many variants for selected tag type",
                        Some(def.span.clone()),
                    );
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
                        for field in &def.fields {
                            if !types.is_copy_type(&field.ty) {
                                diags.push("copy struct fields must be Copy", None);
                            }
                        }
                    }
                }
                TypeDefKind::Enum(def) => {
                    if def.is_copy {
                        for (_, fields) in &def.variants {
                            for ty in fields {
                                if !types.is_copy_type(ty) {
                                    diags.push("copy enum variants must be Copy", None);
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
    let mut globals: HashMap<String, GlobalVarSig> = HashMap::new();
    let mut consts: HashMap<String, ConstSig> = HashMap::new();
    for item in &file.items {
        match item {
            Item::Function(func) => {
                if func.is_extern {
                    let abi = func.extern_abi.clone().unwrap_or_else(|| "C".to_string());
                    if !crate::abi::is_supported_extern_abi(&abi) {
                        diags.push(
                            format!(
                                "unsupported extern ABI `{}` (supported: {})",
                                abi,
                                crate::abi::supported_extern_abis_display()
                            ),
                            Some(func.span.clone()),
                        );
                    }
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
                            if types.contains_view(&ty) {
                                diags.push(DIAG_VIEW_ESCAPE, Some(ret_ty.span.clone()));
                            }
                            ty
                        }
                        None => Type::Builtin(BuiltinType::Unit),
                    }
                } else {
                    Type::Builtin(BuiltinType::Unit)
                };
                if func.name.starts_with("__gost_")
                    && func.name != "__gost_global_init_user"
                    && !std_funcs.contains(&func.name)
                {
                    diags.push("reserved internal name", Some(func.span.clone()));
                }
                if functions.contains_key(&func.name) {
                    diags.push(
                        format!("duplicate function `{}`", func.name),
                        Some(func.span.clone()),
                    );
                    continue;
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
                let abi = global.extern_abi.clone().unwrap_or_else(|| "C".to_string());
                if !crate::abi::is_supported_extern_abi(&abi) {
                    diags.push(
                        format!(
                            "unsupported extern ABI `{}` (supported: {})",
                            abi,
                            crate::abi::supported_extern_abis_display()
                        ),
                        Some(global.span.clone()),
                    );
                }
                if global.name.starts_with("__gost_") {
                    diags.push("reserved internal name", Some(global.span.clone()));
                }
                if let Some(ty) = resolve_type(&global.ty, &types, &mut diags) {
                    if types.contains_view(&ty) {
                        diags.push(DIAG_VIEW_ESCAPE, Some(global.span.clone()));
                    }
                    if extern_globals.contains_key(&global.name) {
                        diags.push(
                            format!("duplicate extern global `{}`", global.name),
                            Some(global.span.clone()),
                        );
                        continue;
                    }
                    extern_globals.insert(global.name.clone(), ExternGlobalSig { ty });
                }
            }
            Item::Global(_) => {}
            Item::Const(c) => {
                let Some((init_ty, value)) = eval_const_expr(&c.init, &consts) else {
                    diags.push(
                        "const initializer must be a compile-time constant",
                        Some(c.init.span.clone()),
                    );
                    continue;
                };
                let ty = if let Some(annot) = &c.ty {
                    match resolve_type(annot, &types, &mut diags) {
                        Some(t) => t,
                        None => continue,
                    }
                } else {
                    init_ty.clone()
                };
                let coerced = if type_eq(&ty, &init_ty) {
                    Some(value.clone())
                } else if matches!(c.init.kind, ExprKind::Int(_))
                    && promote_int_types(&ty, &init_ty).is_some()
                {
                    const_value_convert(&value, &init_ty, &ty)
                } else {
                    None
                };
                let Some(final_value) = coerced else {
                    diags.push("const initializer type mismatch", Some(c.init.span.clone()));
                    continue;
                };
                if consts.contains_key(&c.name) {
                    diags.push(
                        format!("duplicate const `{}`", c.name),
                        Some(c.span.clone()),
                    );
                    continue;
                }
                consts.insert(
                    c.name.clone(),
                    ConstSig {
                        ty,
                        value: final_value,
                    },
                );
            }
            _ => {}
        }
    }

    let namespaced_only_funcs = build_namespaced_only_funcs(file, &functions);
    let trait_methods = collect_trait_method_requirements(file);

    let global_check_sig = FunctionSig {
        params: Vec::new(),
        ret: Type::Builtin(BuiltinType::Unit),
        is_variadic: false,
        is_extern: false,
        is_unsafe: false,
        extern_abi: None,
    };
    for item in &file.items {
        let Item::Global(global) = item else {
            continue;
        };
        if globals.contains_key(&global.name) {
            diags.push(
                format!("duplicate global `{}`", global.name),
                Some(global.span.clone()),
            );
            continue;
        }

        let annot_ty = global
            .ty
            .as_ref()
            .and_then(|ast| resolve_type(ast, &types, &mut diags));

        let mut checker = FunctionChecker::new(
            &types,
            &functions,
            &extern_globals,
            &globals,
            &consts,
            &trait_methods,
            &namespaced_only_funcs,
            &global_check_sig,
            false,
            &mut diags,
            &mut expr_types,
        );
        checker.current_function = Some("__gost_global_init_user".to_string());

        let init_res = if let Some(expected) = annot_ty.as_ref() {
            checker.check_expr_expected(&global.init, expected)
        } else {
            checker.check_expr(&global.init)
        };

        let inferred_ty = init_res.map(|res| res.ty);
        let ty = match (annot_ty, inferred_ty) {
            (Some(ty), _) => ty,
            (None, Some(ty)) => ty,
            (None, None) => continue,
        };

        let init = if let Some((init_ty, init_value)) = eval_const_expr(&global.init, &consts) {
            let coerced = if type_eq(&ty, &init_ty) {
                Some(init_value.clone())
            } else if matches!(global.init.kind, ExprKind::Int(_))
                && promote_int_types(&ty, &init_ty).is_some()
            {
                const_value_convert(&init_value, &init_ty, &ty)
            } else {
                None
            };
            if let Some(value) = coerced {
                GlobalInit::Const(value)
            } else {
                GlobalInit::Zero
            }
        } else {
            GlobalInit::Zero
        };
        globals.insert(global.name.clone(), GlobalVarSig { ty, init });
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
                    &globals,
                    &consts,
                    &trait_methods,
                    &namespaced_only_funcs,
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
            globals,
            consts,
            expr_types,
        })
    } else {
        Err(diags)
    }
}

fn build_namespaced_only_funcs(
    file: &FileAst,
    funcs: &HashMap<String, FunctionSig>,
) -> HashMap<String, String> {
    let mut out = HashMap::new();
    for import in &file.imports {
        let Some(alias) = &import.alias else {
            continue;
        };
        if let Some(only) = &import.only {
            for name in only {
                let namespaced = format!("{}.{}", alias, name);
                if funcs.contains_key(&namespaced) {
                    out.entry(name.clone()).or_insert_with(|| alias.clone());
                }
            }
            continue;
        }
        let prefix = format!("{}.", alias);
        for key in funcs.keys() {
            if let Some(rest) = key.strip_prefix(&prefix)
                && !rest.is_empty()
            {
                out.entry(rest.to_string()).or_insert_with(|| alias.clone());
            }
        }
    }
    out
}

fn collect_trait_method_requirements(file: &FileAst) -> HashMap<String, Vec<TraitMethod>> {
    let mut out = HashMap::new();
    for item in &file.items {
        if let Item::TypeAlias(alias) = item
            && alias.is_trait
        {
            out.insert(alias.name.clone(), alias.trait_methods.clone());
        }
    }
    out
}

fn lower_layout_attr(layout: &LayoutAttr) -> LayoutInfo {
    let repr_int = match layout.repr_int {
        Some(crate::frontend::ast::ReprInt::I8) => Some(ReprInt::I8),
        Some(crate::frontend::ast::ReprInt::I16) => Some(ReprInt::I16),
        Some(crate::frontend::ast::ReprInt::I32) => Some(ReprInt::I32),
        Some(crate::frontend::ast::ReprInt::I64) => Some(ReprInt::I64),
        Some(crate::frontend::ast::ReprInt::Isize) => Some(ReprInt::Isize),
        Some(crate::frontend::ast::ReprInt::U8) => Some(ReprInt::U8),
        Some(crate::frontend::ast::ReprInt::U16) => Some(ReprInt::U16),
        Some(crate::frontend::ast::ReprInt::U32) => Some(ReprInt::U32),
        Some(crate::frontend::ast::ReprInt::U64) => Some(ReprInt::U64),
        Some(crate::frontend::ast::ReprInt::Usize) => Some(ReprInt::Usize),
        None => None,
    };
    LayoutInfo {
        repr_c: layout.repr_c,
        repr_transparent: layout.repr_transparent,
        repr_int,
        repr_other: layout.repr_other.clone(),
        pack: layout.pack,
        bitfield: layout.bitfield,
    }
}

fn repr_int_can_represent_variant_count(repr: crate::frontend::ast::ReprInt, count: usize) -> bool {
    let (signed, bits) = match repr {
        crate::frontend::ast::ReprInt::I8 => (true, 8u32),
        crate::frontend::ast::ReprInt::I16 => (true, 16u32),
        crate::frontend::ast::ReprInt::I32 => (true, 32u32),
        crate::frontend::ast::ReprInt::I64 | crate::frontend::ast::ReprInt::Isize => (true, 64u32),
        crate::frontend::ast::ReprInt::U8 => (false, 8u32),
        crate::frontend::ast::ReprInt::U16 => (false, 16u32),
        crate::frontend::ast::ReprInt::U32 => (false, 32u32),
        crate::frontend::ast::ReprInt::U64 | crate::frontend::ast::ReprInt::Usize => (false, 64u32),
    };
    let max_tag: u128 = if signed {
        (1u128 << (bits - 1)) - 1
    } else {
        (1u128 << bits) - 1
    };
    (count as u128) <= max_tag + 1
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

fn const_numeric_as_i128(value: &ConstValue) -> Option<i128> {
    match value {
        ConstValue::Int(v) => Some(*v),
        ConstValue::UInt(v) => i128::try_from(*v).ok(),
        ConstValue::Char(c) => Some(*c as u32 as i128),
        _ => None,
    }
}

fn const_numeric_as_u128(value: &ConstValue) -> Option<u128> {
    match value {
        ConstValue::Int(v) => u128::try_from(*v).ok(),
        ConstValue::UInt(v) => Some(*v),
        ConstValue::Char(c) => Some(*c as u32 as u128),
        _ => None,
    }
}

fn const_numeric_as_f64(value: &ConstValue) -> Option<f64> {
    match value {
        ConstValue::Float(v) => Some(*v),
        ConstValue::Int(v) => Some(*v as f64),
        ConstValue::UInt(v) => Some(*v as f64),
        ConstValue::Char(c) => Some(*c as u32 as f64),
        _ => None,
    }
}

fn const_value_convert(value: &ConstValue, from: &Type, to: &Type) -> Option<ConstValue> {
    if type_eq(from, to) {
        return Some(value.clone());
    }
    match to {
        Type::Builtin(BuiltinType::Bool) => match value {
            ConstValue::Bool(v) => Some(ConstValue::Bool(*v)),
            _ => None,
        },
        Type::Builtin(BuiltinType::Char) => {
            let n = const_numeric_as_u128(value)?;
            let u = u32::try_from(n).ok()?;
            let c = char::from_u32(u)?;
            Some(ConstValue::Char(c))
        }
        Type::Builtin(BuiltinType::F64) => {
            let f = const_numeric_as_f64(value)?;
            Some(ConstValue::Float(f))
        }
        Type::Builtin(BuiltinType::String) => match value {
            ConstValue::String(s) => Some(ConstValue::String(s.clone())),
            _ => None,
        },
        _ if to.is_integer() => {
            let (signed, bits) = to.int_info()?;
            if signed {
                let n = const_numeric_as_i128(value)?;
                let min = -(1i128 << (bits.saturating_sub(1)));
                let max = (1i128 << (bits.saturating_sub(1))) - 1;
                if n < min || n > max {
                    return None;
                }
                Some(ConstValue::Int(n))
            } else {
                let n = const_numeric_as_u128(value)?;
                let max = if bits == 128 {
                    u128::MAX
                } else {
                    (1u128 << bits) - 1
                };
                if n > max {
                    return None;
                }
                Some(ConstValue::UInt(n))
            }
        }
        _ => None,
    }
}

fn eval_const_expr(expr: &Expr, consts: &HashMap<String, ConstSig>) -> Option<(Type, ConstValue)> {
    match &expr.kind {
        ExprKind::Bool(v) => Some((Type::Builtin(BuiltinType::Bool), ConstValue::Bool(*v))),
        ExprKind::Int(v) => {
            let parsed = v.parse::<i128>().ok()?;
            Some((Type::Builtin(BuiltinType::I32), ConstValue::Int(parsed)))
        }
        ExprKind::Float(v) => {
            let parsed = v.parse::<f64>().ok()?;
            Some((Type::Builtin(BuiltinType::F64), ConstValue::Float(parsed)))
        }
        ExprKind::Char(v) => Some((Type::Builtin(BuiltinType::Char), ConstValue::Char(*v))),
        ExprKind::String(s) => Some((
            Type::Builtin(BuiltinType::String),
            ConstValue::String(s.clone()),
        )),
        ExprKind::Ident(name) => consts.get(name).map(|c| (c.ty.clone(), c.value.clone())),
        ExprKind::Unary { op, expr: inner } => {
            let (ty, val) = eval_const_expr(inner, consts)?;
            match op {
                UnaryOp::Neg => {
                    if type_eq(&ty, &Type::Builtin(BuiltinType::F64)) {
                        let f = const_numeric_as_f64(&val)?;
                        Some((Type::Builtin(BuiltinType::F64), ConstValue::Float(-f)))
                    } else if ty.is_integer() {
                        let n = const_numeric_as_i128(&val)?;
                        Some((ty, ConstValue::Int(-n)))
                    } else {
                        None
                    }
                }
                UnaryOp::Not => match val {
                    ConstValue::Bool(v) => {
                        Some((Type::Builtin(BuiltinType::Bool), ConstValue::Bool(!v)))
                    }
                    _ => None,
                },
                UnaryOp::BitNot => {
                    let (signed, _) = ty.int_info()?;
                    if signed {
                        let n = const_numeric_as_i128(&val)?;
                        Some((ty, ConstValue::Int(!n)))
                    } else {
                        let n = const_numeric_as_u128(&val)?;
                        Some((ty, ConstValue::UInt(!n)))
                    }
                }
            }
        }
        ExprKind::Binary { op, left, right } => {
            let (lt, lv) = eval_const_expr(left, consts)?;
            let (rt, rv) = eval_const_expr(right, consts)?;
            match op {
                BinaryOp::Add
                    if type_eq(&lt, &Type::Builtin(BuiltinType::String))
                        && type_eq(&rt, &Type::Builtin(BuiltinType::String)) =>
                {
                    if let (ConstValue::String(a), ConstValue::String(b)) = (&lv, &rv) {
                        return Some((
                            Type::Builtin(BuiltinType::String),
                            ConstValue::String(format!("{}{}", a, b)),
                        ));
                    }
                    None
                }
                BinaryOp::And | BinaryOp::Or => {
                    let a = matches!(lv, ConstValue::Bool(true));
                    let b = matches!(rv, ConstValue::Bool(true));
                    if !matches!(lv, ConstValue::Bool(_)) || !matches!(rv, ConstValue::Bool(_)) {
                        return None;
                    }
                    let out = match op {
                        BinaryOp::And => a && b,
                        BinaryOp::Or => a || b,
                        _ => false,
                    };
                    Some((Type::Builtin(BuiltinType::Bool), ConstValue::Bool(out)))
                }
                BinaryOp::Eq
                | BinaryOp::NotEq
                | BinaryOp::Lt
                | BinaryOp::Lte
                | BinaryOp::Gt
                | BinaryOp::Gte => {
                    if type_eq(&lt, &Type::Builtin(BuiltinType::F64))
                        || type_eq(&rt, &Type::Builtin(BuiltinType::F64))
                    {
                        let a = const_numeric_as_f64(&lv)?;
                        let b = const_numeric_as_f64(&rv)?;
                        let out = match op {
                            BinaryOp::Eq => a == b,
                            BinaryOp::NotEq => a != b,
                            BinaryOp::Lt => a < b,
                            BinaryOp::Lte => a <= b,
                            BinaryOp::Gt => a > b,
                            BinaryOp::Gte => a >= b,
                            _ => false,
                        };
                        return Some((Type::Builtin(BuiltinType::Bool), ConstValue::Bool(out)));
                    }
                    let promoted = promote_int_types(&lt, &rt)?;
                    let (signed, _) = promoted.int_info()?;
                    let out = if signed {
                        let a = const_numeric_as_i128(&lv)?;
                        let b = const_numeric_as_i128(&rv)?;
                        match op {
                            BinaryOp::Eq => a == b,
                            BinaryOp::NotEq => a != b,
                            BinaryOp::Lt => a < b,
                            BinaryOp::Lte => a <= b,
                            BinaryOp::Gt => a > b,
                            BinaryOp::Gte => a >= b,
                            _ => false,
                        }
                    } else {
                        let a = const_numeric_as_u128(&lv)?;
                        let b = const_numeric_as_u128(&rv)?;
                        match op {
                            BinaryOp::Eq => a == b,
                            BinaryOp::NotEq => a != b,
                            BinaryOp::Lt => a < b,
                            BinaryOp::Lte => a <= b,
                            BinaryOp::Gt => a > b,
                            BinaryOp::Gte => a >= b,
                            _ => false,
                        }
                    };
                    Some((Type::Builtin(BuiltinType::Bool), ConstValue::Bool(out)))
                }
                _ => {
                    if type_eq(&lt, &Type::Builtin(BuiltinType::F64))
                        || type_eq(&rt, &Type::Builtin(BuiltinType::F64))
                    {
                        let a = const_numeric_as_f64(&lv)?;
                        let b = const_numeric_as_f64(&rv)?;
                        let out = match op {
                            BinaryOp::Add => a + b,
                            BinaryOp::Sub => a - b,
                            BinaryOp::Mul => a * b,
                            BinaryOp::Div => a / b,
                            BinaryOp::Rem => a % b,
                            _ => return None,
                        };
                        return Some((Type::Builtin(BuiltinType::F64), ConstValue::Float(out)));
                    }
                    let promoted = promote_int_types(&lt, &rt)?;
                    let (signed, _) = promoted.int_info()?;
                    if signed {
                        let a = const_numeric_as_i128(&lv)?;
                        let b = const_numeric_as_i128(&rv)?;
                        let out = match op {
                            BinaryOp::Add => ConstValue::Int(a + b),
                            BinaryOp::Sub => ConstValue::Int(a - b),
                            BinaryOp::Mul => ConstValue::Int(a * b),
                            BinaryOp::Div => ConstValue::Int(a / b),
                            BinaryOp::Rem => ConstValue::Int(a % b),
                            BinaryOp::BitAnd => ConstValue::Int(a & b),
                            BinaryOp::BitOr => ConstValue::Int(a | b),
                            BinaryOp::BitXor => ConstValue::Int(a ^ b),
                            BinaryOp::Shl => ConstValue::Int(a << (b as u32)),
                            BinaryOp::Shr => ConstValue::Int(a >> (b as u32)),
                            _ => return None,
                        };
                        Some((promoted, out))
                    } else {
                        let a = const_numeric_as_u128(&lv)?;
                        let b = const_numeric_as_u128(&rv)?;
                        let out = match op {
                            BinaryOp::Add => ConstValue::UInt(a + b),
                            BinaryOp::Sub => ConstValue::UInt(a - b),
                            BinaryOp::Mul => ConstValue::UInt(a * b),
                            BinaryOp::Div => ConstValue::UInt(a / b),
                            BinaryOp::Rem => ConstValue::UInt(a % b),
                            BinaryOp::BitAnd => ConstValue::UInt(a & b),
                            BinaryOp::BitOr => ConstValue::UInt(a | b),
                            BinaryOp::BitXor => ConstValue::UInt(a ^ b),
                            BinaryOp::Shl => ConstValue::UInt(a << (b as u32)),
                            BinaryOp::Shr => ConstValue::UInt(a >> (b as u32)),
                            _ => return None,
                        };
                        Some((promoted, out))
                    }
                }
            }
        }
        ExprKind::Cast { .. } => None,
        _ => None,
    }
}

fn resolve_type(ast: &TypeAst, defs: &TypeDefs, diags: &mut Diagnostics) -> Option<Type> {
    match &ast.kind {
        TypeAstKind::Named(name) => {
            if let Some(ty) = builtin_from_name(name) {
                Some(ty)
            } else if let Some(alias_ty) = defs.get_alias(name) {
                Some(alias_ty.clone())
            } else {
                if defs.get(name).is_none() {
                    let mut candidates: Vec<String> =
                        builtin_names().iter().map(|s| s.to_string()).collect();
                    candidates.extend(defs.all_names());
                    let mut d =
                        Diagnostic::new(format!("unknown type `{}`", name), Some(ast.span.clone()))
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
        TypeAstKind::Own(inner) => {
            let ty = resolve_type(inner, defs, diags)?;
            Some(Type::Own(Box::new(ty)))
        }
        TypeAstKind::Alias(inner) => {
            let ty = resolve_type(inner, defs, diags)?;
            Some(Type::Alias(Box::new(ty)))
        }
        TypeAstKind::Slice(inner) => {
            let ty = resolve_type(inner, defs, diags)?;
            Some(Type::Slice(Box::new(ty)))
        }
        TypeAstKind::Array(inner, len) => {
            let ty = resolve_type(inner, defs, diags)?;
            Some(Type::Array(Box::new(ty), *len))
        }
        TypeAstKind::Map(key, value) => {
            let key_ty = resolve_type(key, defs, diags)?;
            let value_ty = resolve_type(value, defs, diags)?;
            if !defs.can_be_map_key(&key_ty) {
                diags.push(
                    "map key type is not supported by runtime map",
                    Some(key.span.clone()),
                );
            }
            Some(Type::Map(Box::new(key_ty), Box::new(value_ty)))
        }
        TypeAstKind::Result(ok, err) => {
            let ok_ty = resolve_type(ok, defs, diags)?;
            let err_ty = resolve_type(err, defs, diags)?;
            Some(Type::Result(Box::new(ok_ty), Box::new(err_ty)))
        }
        TypeAstKind::Chan(inner) => {
            let ty = resolve_type(inner, defs, diags)?;
            Some(Type::Chan(Box::new(ty)))
        }
        TypeAstKind::Shared(inner) => {
            let ty = resolve_type(inner, defs, diags)?;
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
                Some(Type::Result(
                    Box::new(tys[0].clone()),
                    Box::new(tys[1].clone()),
                ))
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
        TypeAstKind::Closure {
            params,
            ret,
            is_variadic,
        } => {
            let mut ptys = Vec::new();
            for p in params {
                ptys.push(resolve_type(p, defs, diags)?);
            }
            let rty = resolve_type(ret, defs, diags)?;
            Some(Type::Closure {
                params: ptys,
                ret: Box::new(rty),
                is_variadic: *is_variadic,
            })
        }
    }
}

fn view_arg_not_allowed(defs: &TypeDefs, arg_ty: &Type, param_ty: &Type) -> bool {
    defs.contains_view(arg_ty) && !defs.contains_view(param_ty)
}

fn slice_elem_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::Slice(inner) => Some(*inner.clone()),
        Type::Array(inner, _) => Some(*inner.clone()),
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

fn check_recursive_types(defs: &TypeDefs, diags: &mut Diagnostics) {
    let mut visiting = HashSet::new();
    let mut visited = HashSet::new();
    for name in defs.names() {
        if visited.contains(&name) {
            continue;
        }
        if dfs_check_cycle(defs, &name, &mut visiting, &mut visited) {
            diags.push(
                "recursive value cycle is not allowed; break the cycle with indirection",
                None,
            );
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
            for field in &def.fields {
                collect_named_types(&field.ty, &mut deps);
            }
        }
        // Enums are represented with an out-of-line payload pointer in codegen,
        // so enum fields do not contribute to inline-size recursion cycles.
        Some(TypeDefKind::Enum(_)) => {}
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
        | Type::Own(_)
        | Type::Alias(_)
        | Type::Shared(_)
        | Type::Iter(_)
        | Type::Map(_, _) => {}
        Type::Array(inner, len) => {
            if *len > 0 {
                collect_named_types(inner, out);
            }
        }
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
    trait_obj: Option<String>,
    mutable: bool,
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
    scopes: Vec<Vec<(String, Option<VarInfo>)>>,
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

    fn exit_scope(&mut self) -> Vec<(String, Option<VarInfo>)> {
        self.scopes.pop().unwrap_or_default()
    }

    fn declare(&mut self, name: String, info: VarInfo) {
        let prev = self.vars.insert(name.clone(), info);
        if let Some(scope) = self.scopes.last_mut() {
            scope.push((name, prev));
        }
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
    extern_globals: &'a HashMap<String, ExternGlobalSig>,
    globals: &'a HashMap<String, GlobalVarSig>,
    consts: &'a HashMap<String, ConstSig>,
    trait_methods: &'a HashMap<String, Vec<TraitMethod>>,
    namespaced_only_funcs: &'a HashMap<String, String>,
    sig: &'a FunctionSig,
    is_std: bool,
    diags: &'a mut Diagnostics,
    expr_types: &'a mut HashMap<ExprId, Type>,
    env: Env,
    allow_iter_chain: bool,
    unsafe_depth: usize,
    loop_labels: Vec<Option<String>>,
    current_function: Option<String>,
    current_impl_type: Option<String>,
    defined_asm_labels: HashSet<String>,
    pending_asm_goto_labels: Vec<(String, Span)>,
}

impl<'a> FunctionChecker<'a> {
    #[allow(clippy::too_many_arguments)]
    fn new(
        defs: &'a TypeDefs,
        funcs: &'a HashMap<String, FunctionSig>,
        extern_globals: &'a HashMap<String, ExternGlobalSig>,
        globals: &'a HashMap<String, GlobalVarSig>,
        consts: &'a HashMap<String, ConstSig>,
        trait_methods: &'a HashMap<String, Vec<TraitMethod>>,
        namespaced_only_funcs: &'a HashMap<String, String>,
        sig: &'a FunctionSig,
        is_std: bool,
        diags: &'a mut Diagnostics,
        expr_types: &'a mut HashMap<ExprId, Type>,
    ) -> Self {
        Self {
            defs,
            funcs,
            extern_globals,
            globals,
            consts,
            trait_methods,
            namespaced_only_funcs,
            sig,
            is_std,
            diags,
            expr_types,
            env: Env::new(),
            allow_iter_chain: false,
            unsafe_depth: if sig.is_unsafe { 1 } else { 0 },
            loop_labels: Vec::new(),
            current_function: None,
            current_impl_type: None,
            defined_asm_labels: HashSet::new(),
            pending_asm_goto_labels: Vec::new(),
        }
    }

    fn enforce_namespaced_only(&self) -> bool {
        match self.current_function.as_deref() {
            Some(name) => !name.contains('.'),
            None => true,
        }
    }

    fn can_access_struct_field(&self, struct_name: &str, field: &StructField) -> bool {
        if field.vis.is_public() {
            return true;
        }
        self.current_impl_type.as_deref() == Some(struct_name)
    }

    fn diag_private_field(&mut self, span: &Span, struct_name: &str, field_name: &str) {
        self.diags.push(
            format!("field `{}` of `{}` is private", field_name, struct_name),
            Some(span.clone()),
        );
    }

    fn require_unsafe_operation(&mut self, span: &Span, what: &str) {
        if self.unsafe_depth == 0 {
            self.diags.push(
                format!(
                    "{} requires unsafe context (`unsafe {{ ... }}` or `unsafe fn`)",
                    what
                ),
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
            (RefKind::None, RefKind::Shared) => true, // autoref
            (RefKind::None, RefKind::Mut) => true,    // autoref mut
            (RefKind::Shared, RefKind::None) => true, // autoderef
            (RefKind::Mut, RefKind::None) => true,    // autoderef
            (RefKind::Mut, RefKind::Shared) => true,  // reborrow
            (RefKind::Shared, RefKind::Mut) => false,
        }
    }

    fn recv_adjust_cost(&self, recv_ty: &Type, param0_ty: &Type) -> Option<u8> {
        if !self.recv_param_compatible(recv_ty, param0_ty) {
            return None;
        }
        let (_, rk, _) = recv_ty.peel_refs();
        let (_, pk, _) = param0_ty.peel_refs();
        let cost = match (rk, pk) {
            (RefKind::None, RefKind::None) => 0,
            (RefKind::Shared, RefKind::Shared) => 0,
            (RefKind::Mut, RefKind::Mut) => 0,
            (RefKind::None, RefKind::Shared) => 1,
            (RefKind::None, RefKind::Mut) => 2,
            (RefKind::Shared, RefKind::None) => 1,
            (RefKind::Mut, RefKind::None) => 1,
            (RefKind::Mut, RefKind::Shared) => 1,
            (RefKind::Shared, RefKind::Mut) => return None,
        };
        Some(cost)
    }

    fn method_candidates_named(&self, method: &str) -> Vec<(String, FunctionSig)> {
        let mut out = Vec::new();
        for (symbol, sig) in self.funcs.iter() {
            if sig.params.is_empty() {
                continue;
            }
            if logical_method_name(symbol) == method {
                out.push((symbol.clone(), sig.clone()));
            }
        }
        out.sort_by(|a, b| a.0.cmp(&b.0));
        out
    }

    fn trait_name_from_type_ast(&self, ty: &TypeAst) -> Option<String> {
        match &ty.kind {
            TypeAstKind::Named(name) if self.defs.is_trait_name(name) => Some(name.clone()),
            _ => None,
        }
    }

    fn trait_name_from_expr(&self, expr: &Expr) -> Option<String> {
        match &expr.kind {
            ExprKind::Ident(name) => self.env.vars.get(name).and_then(|v| v.trait_obj.clone()),
            ExprKind::Cast { ty, .. } => self.trait_name_from_type_ast(ty),
            _ => None,
        }
    }

    fn trait_method_names(&self, trait_name: &str) -> Vec<String> {
        let mut out = self
            .trait_methods
            .get(trait_name)
            .into_iter()
            .flat_map(|methods| methods.iter().map(|m| m.name.clone()))
            .collect::<Vec<_>>();
        out.sort();
        out.dedup();
        out
    }

    fn substitute_type_params_in_type_ast(
        &self,
        ty: &TypeAst,
        subst: &HashMap<String, TypeAst>,
    ) -> TypeAst {
        if let TypeAstKind::Named(name) = &ty.kind
            && let Some(repl) = subst.get(name)
        {
            return repl.clone();
        }
        let kind = match &ty.kind {
            TypeAstKind::Named(name) => TypeAstKind::Named(name.clone()),
            TypeAstKind::Ref(inner) => TypeAstKind::Ref(Box::new(
                self.substitute_type_params_in_type_ast(inner, subst),
            )),
            TypeAstKind::MutRef(inner) => TypeAstKind::MutRef(Box::new(
                self.substitute_type_params_in_type_ast(inner, subst),
            )),
            TypeAstKind::Own(inner) => TypeAstKind::Own(Box::new(
                self.substitute_type_params_in_type_ast(inner, subst),
            )),
            TypeAstKind::Alias(inner) => TypeAstKind::Alias(Box::new(
                self.substitute_type_params_in_type_ast(inner, subst),
            )),
            TypeAstKind::Slice(inner) => TypeAstKind::Slice(Box::new(
                self.substitute_type_params_in_type_ast(inner, subst),
            )),
            TypeAstKind::Array(inner, len) => TypeAstKind::Array(
                Box::new(self.substitute_type_params_in_type_ast(inner, subst)),
                *len,
            ),
            TypeAstKind::Map(key, val) => TypeAstKind::Map(
                Box::new(self.substitute_type_params_in_type_ast(key, subst)),
                Box::new(self.substitute_type_params_in_type_ast(val, subst)),
            ),
            TypeAstKind::Result(ok, err) => TypeAstKind::Result(
                Box::new(self.substitute_type_params_in_type_ast(ok, subst)),
                Box::new(self.substitute_type_params_in_type_ast(err, subst)),
            ),
            TypeAstKind::Chan(inner) => TypeAstKind::Chan(Box::new(
                self.substitute_type_params_in_type_ast(inner, subst),
            )),
            TypeAstKind::Shared(inner) => TypeAstKind::Shared(Box::new(
                self.substitute_type_params_in_type_ast(inner, subst),
            )),
            TypeAstKind::Interface => TypeAstKind::Interface,
            TypeAstKind::Tuple(items) => TypeAstKind::Tuple(
                items
                    .iter()
                    .map(|item| self.substitute_type_params_in_type_ast(item, subst))
                    .collect(),
            ),
            TypeAstKind::FnPtr {
                params,
                ret,
                is_variadic,
            } => TypeAstKind::FnPtr {
                params: params
                    .iter()
                    .map(|p| self.substitute_type_params_in_type_ast(p, subst))
                    .collect(),
                ret: Box::new(self.substitute_type_params_in_type_ast(ret, subst)),
                is_variadic: *is_variadic,
            },
            TypeAstKind::Closure {
                params,
                ret,
                is_variadic,
            } => TypeAstKind::Closure {
                params: params
                    .iter()
                    .map(|p| self.substitute_type_params_in_type_ast(p, subst))
                    .collect(),
                ret: Box::new(self.substitute_type_params_in_type_ast(ret, subst)),
                is_variadic: *is_variadic,
            },
        };
        TypeAst {
            kind,
            span: ty.span.clone(),
        }
    }

    fn resolve_trait_method_signature_for_call(
        &mut self,
        trait_name: &str,
        method_name: &str,
        type_args: &[TypeAst],
        span: &Span,
    ) -> Option<FunctionSig> {
        let Some(methods) = self.trait_methods.get(trait_name) else {
            self.diags.push(
                format!("unknown trait `{}`", trait_name),
                Some(span.clone()),
            );
            return None;
        };
        let Some(required) = methods.iter().find(|m| m.name == method_name) else {
            // Marker-style empty traits act as open interface aliases.
            // Allow method dispatch recovery from available receiver-first candidates.
            if methods.is_empty() {
                let mut dispatch = Vec::<(String, FunctionSig)>::new();
                for (symbol, sig) in self.method_candidates_named(method_name) {
                    let Some(param0) = sig.params.first() else {
                        continue;
                    };
                    if self.defs.interface_cast_supported(param0) {
                        dispatch.push((symbol, sig));
                    }
                }
                if dispatch.is_empty() {
                    return None;
                }
                dispatch.sort_by(|a, b| a.0.cmp(&b.0));
                let canonical = dispatch[0].1.clone();
                for (_, sig) in dispatch.iter().skip(1) {
                    if sig.params.len() != canonical.params.len()
                        || sig.is_variadic != canonical.is_variadic
                        || !type_eq(&sig.ret, &canonical.ret)
                    {
                        self.diags.push(
                            format!(
                                "interface method `{}` has incompatible implementations",
                                method_name
                            ),
                            Some(span.clone()),
                        );
                        return None;
                    }
                    for idx in 1..sig.params.len() {
                        if !type_eq(&sig.params[idx], &canonical.params[idx]) {
                            self.diags.push(
                                format!(
                                    "interface method `{}` has incompatible implementations",
                                    method_name
                                ),
                                Some(span.clone()),
                            );
                            return None;
                        }
                    }
                }
                let mut params = Vec::with_capacity(canonical.params.len());
                params.push(Type::Interface);
                params.extend(canonical.params.iter().skip(1).cloned());
                return Some(FunctionSig {
                    params,
                    ret: canonical.ret,
                    is_variadic: canonical.is_variadic,
                    is_extern: false,
                    is_unsafe: false,
                    extern_abi: None,
                });
            }
            return None;
        };
        let mut subst = HashMap::<String, TypeAst>::new();
        if required.type_params.is_empty() {
            if !type_args.is_empty() {
                self.diag_type_args_call_misuse(span);
                return None;
            }
        } else if type_args.is_empty() {
            // Method generic args may be consumed during earlier monomorphization rewrites.
            // In that case, recover the callable signature from available dispatch candidates.
            let mut dispatch = Vec::<FunctionSig>::new();
            for (_, sig) in self.method_candidates_named(method_name) {
                let Some(param0) = sig.params.first() else {
                    continue;
                };
                if !self.defs.interface_cast_supported(param0) {
                    continue;
                }
                dispatch.push(sig);
            }
            if let Some(canonical) = dispatch.into_iter().next() {
                let mut params = Vec::with_capacity(canonical.params.len());
                params.push(Type::Interface);
                params.extend(canonical.params.into_iter().skip(1));
                return Some(FunctionSig {
                    params,
                    ret: canonical.ret,
                    is_variadic: canonical.is_variadic,
                    is_extern: false,
                    is_unsafe: false,
                    extern_abi: None,
                });
            }
            self.diags.push(
                format!("generic method `{}` requires type arguments", method_name),
                Some(span.clone()),
            );
            return None;
        } else if type_args.len() != required.type_params.len() {
            self.diags.push(
                format!(
                    "generic method `{}` expects {} type arguments, got {}",
                    method_name,
                    required.type_params.len(),
                    type_args.len()
                ),
                Some(span.clone()),
            );
            return None;
        } else {
            for (tp, arg) in required.type_params.iter().zip(type_args.iter()) {
                subst.insert(tp.name.clone(), arg.clone());
            }
        }
        let mut params = Vec::with_capacity(required.params.len() + 1);
        params.push(Type::Interface);
        for param in &required.params {
            let param_ty_ast = if subst.is_empty() {
                param.ty.clone()
            } else {
                self.substitute_type_params_in_type_ast(&param.ty, &subst)
            };
            params.push(resolve_type(&param_ty_ast, self.defs, self.diags)?);
        }
        let ret_ty_ast = if subst.is_empty() {
            required.ret_type.clone()
        } else {
            self.substitute_type_params_in_type_ast(&required.ret_type, &subst)
        };
        let ret = resolve_type(&ret_ty_ast, self.defs, self.diags)?;
        Some(FunctionSig {
            params,
            ret,
            is_variadic: required.is_variadic,
            is_extern: false,
            is_unsafe: false,
            extern_abi: None,
        })
    }

    fn resolve_trait_method_signature(
        &mut self,
        trait_name: &str,
        method_name: &str,
        span: &Span,
    ) -> Option<FunctionSig> {
        let Some(methods) = self.trait_methods.get(trait_name) else {
            self.diags.push(
                format!("unknown trait `{}`", trait_name),
                Some(span.clone()),
            );
            return None;
        };
        let required = methods.iter().find(|m| m.name == method_name)?;
        let mut params = Vec::with_capacity(required.params.len() + 1);
        params.push(Type::Interface);
        for param in &required.params {
            let ty = resolve_type(&param.ty, self.defs, self.diags)?;
            params.push(ty);
        }
        let ret = resolve_type(&required.ret_type, self.defs, self.diags)?;
        Some(FunctionSig {
            params,
            ret,
            is_variadic: required.is_variadic,
            is_extern: false,
            is_unsafe: false,
            extern_abi: None,
        })
    }

    fn method_sig_matches_trait_requirement(
        &self,
        candidate: &FunctionSig,
        required: &FunctionSig,
    ) -> bool {
        if candidate.params.len() != required.params.len() {
            return false;
        }
        if candidate.is_variadic != required.is_variadic {
            return false;
        }
        if !type_eq(&candidate.ret, &required.ret) {
            return false;
        }
        for idx in 1..candidate.params.len() {
            if !type_eq(&candidate.params[idx], &required.params[idx]) {
                return false;
            }
        }
        true
    }

    fn has_trait_dispatch_candidate(
        &self,
        recv_ty: &Type,
        method_name: &str,
        required: &FunctionSig,
    ) -> bool {
        for (_, sig) in self.method_candidates_named(method_name) {
            let Some(param0) = sig.params.first() else {
                continue;
            };
            if self.recv_adjust_cost(recv_ty, param0).is_none() {
                continue;
            }
            if !self.defs.interface_cast_supported(param0) {
                continue;
            }
            if self.method_sig_matches_trait_requirement(&sig, required) {
                return true;
            }
        }
        false
    }

    fn type_implements_trait_object(
        &mut self,
        recv_ty: &Type,
        trait_name: &str,
        span: &Span,
    ) -> bool {
        let Some(methods) = self.trait_methods.get(trait_name).cloned() else {
            self.diags.push(
                format!("unknown trait `{}`", trait_name),
                Some(span.clone()),
            );
            return false;
        };
        let mut ok = true;
        for required in methods {
            if !required.type_params.is_empty() {
                // Generic trait methods are validated at call-sites where concrete type
                // arguments are available.
                continue;
            }
            let Some(req_sig) =
                self.resolve_trait_method_signature(trait_name, &required.name, &required.span)
            else {
                ok = false;
                continue;
            };
            if !self.has_trait_dispatch_candidate(recv_ty, &required.name, &req_sig) {
                self.diags.push(
                    format!(
                        "type `{}` does not implement trait `{}`: missing method `{}`",
                        recv_ty.pretty(),
                        trait_name,
                        required.name
                    ),
                    Some(span.clone()),
                );
                ok = false;
            }
        }
        ok
    }

    fn check_call_args_for_sig(
        &mut self,
        call_span: &Span,
        args: &[Expr],
        sig: &FunctionSig,
        param_offset: usize,
    ) -> Option<()> {
        let fixed_args = sig.params.len().saturating_sub(param_offset);
        if (!sig.is_variadic && args.len() != fixed_args)
            || (sig.is_variadic && args.len() < fixed_args)
        {
            self.diags
                .push("argument count mismatch", Some(call_span.clone()));
        }
        for (idx, arg) in args.iter().enumerate() {
            if let Some(param_ty) = sig.params.get(idx + param_offset) {
                let res = self.check_expr_expected(arg, param_ty)?;
                if view_arg_not_allowed(self.defs, &res.ty, param_ty) {
                    self.diags.push(
                        "view arguments can only be passed to intrinsics",
                        Some(arg.span.clone()),
                    );
                }
                if !type_eq(&res.ty, param_ty) {
                    self.diags
                        .push("argument type mismatch", Some(arg.span.clone()));
                }
            } else {
                let res = self.check_expr(arg)?;
                if self.defs.contains_view(&res.ty) {
                    self.diags.push(
                        "view arguments are not allowed in variadic positions",
                        Some(arg.span.clone()),
                    );
                }
            }
        }
        Some(())
    }

    fn preview_call_arg_types(&mut self, args: &[Expr]) -> Option<Vec<Type>> {
        let mut out = Vec::with_capacity(args.len());
        for arg in args {
            let res = self.check_expr_no_move(arg)?;
            out.push(res.ty);
        }
        Some(out)
    }

    fn call_args_match_sig_types(
        &self,
        sig: &FunctionSig,
        param_offset: usize,
        arg_types: &[Type],
    ) -> bool {
        let fixed_args = sig.params.len().saturating_sub(param_offset);
        if (!sig.is_variadic && arg_types.len() != fixed_args)
            || (sig.is_variadic && arg_types.len() < fixed_args)
        {
            return false;
        }
        for (idx, arg_ty) in arg_types.iter().enumerate() {
            if let Some(param_ty) = sig.params.get(idx + param_offset) {
                if !type_eq(arg_ty, param_ty) {
                    return false;
                }
            } else if self.defs.contains_view(arg_ty) {
                return false;
            }
        }
        true
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
                .note(format!(
                    "this call requires receiver type: {}",
                    f.param0_ty.pretty()
                ))
                .note("but the receiver expression is a temporary value");
                d = d.help("bind the receiver to a local first:");
                if need_mut {
                    d = d.with_help_snippet(f.recv_span.clone(), "let mut tmp = {snippet};");
                } else {
                    d = d.with_help_snippet(f.recv_span.clone(), "let tmp = {snippet};");
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
                .note(format!(
                    "this call requires receiver type: {}",
                    f.param0_ty.pretty()
                ))
                .note(format!("receiver type here is: {}", f.recv_ty.pretty()));

                if let Some(name) = f.recv_base_name.clone() {
                    d = d
                        .help("make it mutable:")
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
            if let Some(p0) = sig.params.first()
                && self.recv_param_compatible(recv_ty, p0)
            {
                out.push(logical_method_name(name).to_string());
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

    fn diag_type_args_call_misuse(&mut self, span: &Span) {
        self.diags.push(
            "this call target is not generic; use `[]` only for type arguments and `()` for value arguments",
            Some(span.clone()),
        );
    }

    fn check_function(&mut self, func: &Function) {
        self.current_function = Some(func.name.clone());
        self.current_impl_type = if func.params.first().map(|p| p.name.as_str()) == Some("self") {
            match self.sig.params.first() {
                Some(Type::Named(name)) => Some(name.clone()),
                Some(Type::Ref(inner)) | Some(Type::MutRef(inner)) => match inner.as_ref() {
                    Type::Named(name) => Some(name.clone()),
                    _ => None,
                },
                _ => None,
            }
        } else {
            None
        };
        self.collect_asm_labels_in_block(&func.body);
        self.env.enter_scope();
        for (idx, param) in func.params.iter().enumerate() {
            let ty = self
                .sig
                .params
                .get(idx)
                .cloned()
                .unwrap_or(Type::Builtin(BuiltinType::Unit));
            let class = self.defs.classify(&ty).unwrap_or(TypeClass::Copy);
            let info = VarInfo {
                ty,
                trait_obj: self.trait_name_from_type_ast(&param.ty),
                mutable: true,
                class,
                state: LinearState::Alive,
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
            if self.defs.contains_view(&ty) {
                self.diags.push(DIAG_VIEW_ESCAPE, Some(func.span.clone()));
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
        self.current_function = None;
        self.current_impl_type = None;
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
                | Stmt::ForIn { body, .. }
                | Stmt::ForRange { body, .. } => self.collect_asm_labels_in_block(body),
                Stmt::Select { arms, .. } => {
                    for arm in arms {
                        if let BlockOrExpr::Block(block) = &arm.body {
                            self.collect_asm_labels_in_block(block);
                        }
                    }
                }
                Stmt::Let { .. }
                | Stmt::Const { .. }
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
        if let ExprKind::Call { callee, args, .. } = &expr.kind
            && let ExprKind::Ident(name) = &callee.kind
            && name == "asm_label"
            && let Some(Expr {
                kind: ExprKind::String(s),
                ..
            }) = args.first()
        {
            return Some(s.trim().to_string());
        }
        None
    }

    fn check_block(&mut self, block: &Block) -> Option<Type> {
        self.check_block_expected(block, None)
    }

    fn check_block_expected(&mut self, block: &Block, expected: Option<&Type>) -> Option<Type> {
        self.env.enter_scope();
        let mut terminated = false;
        for stmt in &block.stmts {
            self.check_stmt(stmt);
            // Stop after a terminating statement to avoid cascading linear errors.
            if matches!(
                stmt,
                Stmt::Return { .. } | Stmt::Break { .. } | Stmt::Continue { .. }
            ) {
                terminated = true;
                break;
            }
        }
        let result = if !terminated {
            if let Some(expr) = &block.tail {
                match expected {
                    Some(expected_ty) => self.check_expr_expected(expr, expected_ty).map(|r| r.ty),
                    None => self.check_expr(expr).map(|r| r.ty),
                }
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
            Stmt::Let {
                name,
                ty,
                init,
                span,
            } => {
                let resolved_annot = ty
                    .as_ref()
                    .and_then(|annot| resolve_type(annot, self.defs, self.diags));
                let init_ty = match resolved_annot.as_ref() {
                    Some(expected) => self.check_expr_expected(init, expected),
                    None => self.check_expr(init),
                };
                let init_ty = match init_ty {
                    Some(res) => res,
                    None => return,
                };
                let final_ty = if let Some(resolved) = resolved_annot {
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
                };
                let class = self.defs.classify(&final_ty).unwrap_or(TypeClass::Copy);
                let trait_obj = ty
                    .as_ref()
                    .and_then(|annot| self.trait_name_from_type_ast(annot))
                    .or_else(|| self.trait_name_from_expr(init));
                let mut info = VarInfo {
                    ty: final_ty,
                    trait_obj,
                    mutable: true,
                    class,
                    state: LinearState::Alive,
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
                            } else if base.borrowed_mut > 0 {
                                self.diags.push(
                                    "shared borrow conflicts with mutable borrow",
                                    Some(span.clone()),
                                );
                            } else {
                                base.borrowed_shared += 1;
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
            Stmt::Const {
                name,
                ty,
                init,
                span,
            } => {
                let resolved_annot = ty
                    .as_ref()
                    .and_then(|annot| resolve_type(annot, self.defs, self.diags));
                let init_ty = match resolved_annot.as_ref() {
                    Some(expected) => self.check_expr_expected(init, expected),
                    None => self.check_expr_no_move(init),
                };
                let init_ty = match init_ty {
                    Some(res) => res,
                    None => return,
                };
                let final_ty = if let Some(resolved) = resolved_annot {
                    let can_promote = matches!(init.kind, ExprKind::Int(_))
                        && promote_int_types(&resolved, &init_ty.ty).is_some();
                    if !type_eq(&resolved, &init_ty.ty) && !can_promote {
                        let d = Diagnostic::new(
                            "const initializer type mismatch",
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
                };
                if self.defs.contains_view(&final_ty) {
                    self.diags.push(DIAG_VIEW_ESCAPE, Some(span.clone()));
                }
                let class = self.defs.classify(&final_ty).unwrap_or(TypeClass::Copy);
                let trait_obj = ty
                    .as_ref()
                    .and_then(|annot| self.trait_name_from_type_ast(annot))
                    .or_else(|| self.trait_name_from_expr(init));
                let info = VarInfo {
                    ty: final_ty,
                    trait_obj,
                    mutable: false,
                    class,
                    state: LinearState::Alive,
                    borrowed_shared: 0,
                    borrowed_mut: 0,
                    view_of: None,
                    view_is_mut: false,
                };
                self.env.declare(name.clone(), info);
            }
            Stmt::Assign {
                op,
                target,
                value,
                span,
            } => {
                let target_name = match &target.kind {
                    ExprKind::Ident(name) => Some(name.clone()),
                    _ => None,
                };
                let target_ty = match self.check_assign_target(target) {
                    Some(ty) => ty,
                    None => return,
                };
                let value_ty = match self.check_expr_expected(value, &target_ty) {
                    Some(res) => res.ty,
                    None => return,
                };
                if self.defs.contains_view(&value_ty) {
                    self.diags.push(DIAG_VIEW_ESCAPE, Some(span.clone()));
                }
                match op {
                    AssignOp::Assign => {
                        let can_promote = matches!(value.kind, ExprKind::Int(_))
                            && promote_int_types(&target_ty, &value_ty).is_some();
                        if !type_eq(&target_ty, &value_ty) && !can_promote {
                            self.diags
                                .push("assignment type mismatch", Some(span.clone()));
                        }
                    }
                    AssignOp::AddAssign
                    | AssignOp::SubAssign
                    | AssignOp::MulAssign
                    | AssignOp::DivAssign
                    | AssignOp::RemAssign => {
                        let lhs_num = target_ty.is_numeric();
                        let rhs_num = value_ty.is_numeric();
                        let lhs_int = target_ty.is_integer();
                        let rhs_int = value_ty.is_integer();
                        if !(lhs_num && rhs_num) {
                            self.diags.push(
                                "compound assignment expects numeric operands",
                                Some(span.clone()),
                            );
                        } else if lhs_int && rhs_int {
                            // allow mixed integer widths/signs via explicit assignment target cast
                        } else if !type_eq(&target_ty, &value_ty) {
                            self.diags
                                .push("compound assignment type mismatch", Some(span.clone()));
                        }
                    }
                    AssignOp::BitAndAssign
                    | AssignOp::BitOrAssign
                    | AssignOp::BitXorAssign
                    | AssignOp::ShlAssign
                    | AssignOp::ShrAssign => {
                        if !target_ty.is_integer() || !value_ty.is_integer() {
                            self.diags.push(
                                "bitwise compound assignment expects integer operands",
                                Some(span.clone()),
                            );
                        }
                    }
                }
                if let Some(name) = target_name
                    && let Some(var) = self.env.vars.get_mut(&name)
                    && var.class == TypeClass::Linear
                {
                    var.state = LinearState::Alive;
                }
            }
            Stmt::Expr { expr, .. } => {
                self.check_expr(expr);
            }
            Stmt::Return { expr, span } => {
                let ret_ty = if let Some(expr) = expr {
                    match self.check_expr_expected(expr, &self.sig.ret) {
                        Some(res) => res.ty,
                        None => return,
                    }
                } else {
                    Type::Builtin(BuiltinType::Unit)
                };
                if self.defs.contains_view(&ret_ty) {
                    self.diags.push(DIAG_VIEW_ESCAPE, Some(span.clone()));
                }
                if !type_eq(&ret_ty, &self.sig.ret) {
                    self.diags.push("return type mismatch", Some(span.clone()));
                }
            }
            Stmt::Break { label, span } => {
                if let Some(want) = label {
                    let found = self
                        .loop_labels
                        .iter()
                        .rev()
                        .any(|name| name.as_ref() == Some(want));
                    if !found {
                        self.diags
                            .push(format!("unknown loop label `{}`", want), Some(span.clone()));
                    }
                } else if self.loop_labels.is_empty() {
                    self.diags.push("break outside loop", Some(span.clone()));
                }
            }
            Stmt::Continue { label, span } => {
                if let Some(want) = label {
                    let found = self
                        .loop_labels
                        .iter()
                        .rev()
                        .any(|name| name.as_ref() == Some(want));
                    if !found {
                        self.diags
                            .push(format!("unknown loop label `{}`", want), Some(span.clone()));
                    }
                } else if self.loop_labels.is_empty() {
                    self.diags.push("continue outside loop", Some(span.clone()));
                }
            }
            Stmt::While {
                label,
                cond,
                body,
                span,
            } => {
                let cond_res = match self.check_expr(cond) {
                    Some(res) => res.ty,
                    None => return,
                };
                if !type_eq(&cond_res, &Type::Builtin(BuiltinType::Bool)) {
                    self.diags
                        .push("while condition must be bool", Some(span.clone()));
                }
                self.loop_labels.push(label.clone());
                let _ = self.check_block(body);
                self.loop_labels.pop();
            }
            Stmt::Loop { label, body, .. } => {
                self.loop_labels.push(label.clone());
                let _ = self.check_block(body);
                self.loop_labels.pop();
            }
            Stmt::ForIn {
                label,
                name,
                index,
                iter,
                body,
                span,
            } => {
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
                let iter_ty_for_guard = iter_ty.clone();
                let mut index_decl_ty = Type::Builtin(BuiltinType::I64);
                let (view_ty, view_is_mut) = match iter_ty {
                    Type::Iter(inner) => {
                        let inner_ty = *inner;
                        let view_is_mut = matches!(inner_ty, Type::MutRef(_));
                        (inner_ty, view_is_mut)
                    }
                    Type::Slice(inner) | Type::Array(inner, _) => {
                        let elem = *inner;
                        (elem, false)
                    }
                    Type::Builtin(BuiltinType::Bytes) => {
                        let elem = Type::Builtin(BuiltinType::U32);
                        (elem, false)
                    }
                    Type::Builtin(BuiltinType::String) => {
                        let elem = Type::Builtin(BuiltinType::U32);
                        (elem, false)
                    }
                    Type::Map(key, value) => {
                        index_decl_ty = *key;
                        (*value, false)
                    }
                    Type::Ref(inner) => match *inner {
                        Type::Slice(elem) | Type::Array(elem, _) => {
                            let elem = *elem;
                            (Type::Ref(Box::new(elem)), false)
                        }
                        Type::Builtin(BuiltinType::Bytes) => {
                            let elem = Type::Builtin(BuiltinType::U32);
                            (Type::Ref(Box::new(elem)), false)
                        }
                        Type::Builtin(BuiltinType::String) => {
                            let elem = Type::Builtin(BuiltinType::U32);
                            (elem, false)
                        }
                        Type::Map(key, value) => {
                            index_decl_ty = *key;
                            (*value, false)
                        }
                        _ => {
                            self.diags.push(
                                "for-in expects iterable (slice/array/map/bytes/string/iter)",
                                Some(span.clone()),
                            );
                            return;
                        }
                    },
                    Type::MutRef(inner) => match *inner {
                        Type::Slice(elem) | Type::Array(elem, _) => {
                            let elem = *elem;
                            (Type::MutRef(Box::new(elem)), true)
                        }
                        Type::Builtin(BuiltinType::Bytes) => {
                            let elem = Type::Builtin(BuiltinType::U32);
                            (Type::MutRef(Box::new(elem)), true)
                        }
                        Type::Builtin(BuiltinType::String) => {
                            let elem = Type::Builtin(BuiltinType::U32);
                            (elem, false)
                        }
                        Type::Map(key, value) => {
                            index_decl_ty = *key;
                            (*value, false)
                        }
                        _ => {
                            self.diags.push(
                                "for-in expects iterable (slice/array/map/bytes/string/iter)",
                                Some(span.clone()),
                            );
                            return;
                        }
                    },
                    _ => {
                        self.diags.push(
                            "for-in expects iterable (slice/array/map/bytes/string/iter)",
                            Some(span.clone()),
                        );
                        return;
                    }
                };
                let mut iter_borrow_guard: Option<(String, bool)> = None;
                let iter_is_map_ref = matches!(
                    &iter_ty_for_guard,
                    Type::Ref(inner) | Type::MutRef(inner) if matches!(inner.as_ref(), Type::Map(_, _))
                );
                if iter_is_map_ref
                    && let ExprKind::Borrow {
                        is_mut,
                        expr: inner,
                    } = &iter.kind
                    && let Some(base) = self.borrow_base_ident(inner)
                    && let Some(base_info) = self.env.vars.get_mut(&base)
                {
                    if *is_mut {
                        if base_info.borrowed_shared > 0 || base_info.borrowed_mut > 0 {
                            self.diags.push(
                                "mutable borrow conflicts with existing borrows",
                                Some(iter.span.clone()),
                            );
                        } else {
                            base_info.borrowed_mut += 1;
                            iter_borrow_guard = Some((base, true));
                        }
                    } else if base_info.borrowed_mut > 0 {
                        self.diags.push(
                            "shared borrow conflicts with mutable borrow",
                            Some(iter.span.clone()),
                        );
                    } else {
                        base_info.borrowed_shared += 1;
                        iter_borrow_guard = Some((base, false));
                    }
                }
                self.env.enter_scope();
                if let Some(index_name) = index {
                    let index_class = self
                        .defs
                        .classify(&index_decl_ty)
                        .unwrap_or(TypeClass::Copy);
                    self.env.declare(
                        index_name.clone(),
                        VarInfo {
                            ty: index_decl_ty,
                            trait_obj: None,
                            mutable: true,
                            class: index_class,
                            state: LinearState::Alive,
                            borrowed_shared: 0,
                            borrowed_mut: 0,
                            view_of: None,
                            view_is_mut: false,
                        },
                    );
                }
                let class = self.defs.classify(&view_ty).unwrap_or(TypeClass::Copy);
                self.env.declare(
                    name.clone(),
                    VarInfo {
                        ty: view_ty,
                        trait_obj: None,
                        mutable: true,
                        class,
                        state: LinearState::Alive,
                        borrowed_shared: 0,
                        borrowed_mut: 0,
                        view_of: None,
                        view_is_mut,
                    },
                );
                self.loop_labels.push(label.clone());
                self.check_block(body);
                self.loop_labels.pop();
                self.drop_scope();
                if let Some((base, is_mut)) = iter_borrow_guard
                    && let Some(base_info) = self.env.vars.get_mut(&base)
                {
                    if is_mut {
                        base_info.borrowed_mut = base_info.borrowed_mut.saturating_sub(1);
                    } else {
                        base_info.borrowed_shared = base_info.borrowed_shared.saturating_sub(1);
                    }
                }
            }
            Stmt::ForRange {
                label,
                name,
                index,
                start,
                end,
                inclusive: _,
                body,
                span,
            } => {
                let mut start_res = match self.check_expr(start) {
                    Some(res) => res,
                    None => return,
                };
                let mut end_res = match self.check_expr(end) {
                    Some(res) => res,
                    None => return,
                };
                if start_res.ty.is_integer()
                    && end_res.ty.is_integer()
                    && !type_eq(&start_res.ty, &end_res.ty)
                {
                    if matches!(start.kind, ExprKind::Int(_)) {
                        if let Some(coerced) = self.check_expr_expected(start, &end_res.ty) {
                            start_res = coerced;
                        }
                    } else if matches!(end.kind, ExprKind::Int(_))
                        && let Some(coerced) = self.check_expr_expected(end, &start_res.ty)
                    {
                        end_res = coerced;
                    }
                }
                let start_ty = start_res.ty;
                let end_ty = end_res.ty;
                if !start_ty.is_integer() || !end_ty.is_integer() {
                    self.diags
                        .push("range bounds must be integer", Some(span.clone()));
                    return;
                }
                if !type_eq(&start_ty, &end_ty) {
                    self.diags
                        .push("range bounds must have same type", Some(span.clone()));
                    return;
                }
                self.env.enter_scope();
                if let Some(index_name) = index {
                    self.env.declare(
                        index_name.clone(),
                        VarInfo {
                            ty: Type::Builtin(BuiltinType::I64),
                            trait_obj: None,
                            mutable: true,
                            class: TypeClass::Copy,
                            state: LinearState::Alive,
                            borrowed_shared: 0,
                            borrowed_mut: 0,
                            view_of: None,
                            view_is_mut: false,
                        },
                    );
                }
                let class = self.defs.classify(&start_ty).unwrap_or(TypeClass::Copy);
                self.env.declare(
                    name.clone(),
                    VarInfo {
                        ty: start_ty,
                        trait_obj: None,
                        mutable: true,
                        class,
                        state: LinearState::Alive,
                        borrowed_shared: 0,
                        borrowed_mut: 0,
                        view_of: None,
                        view_is_mut: false,
                    },
                );
                self.loop_labels.push(label.clone());
                self.check_block(body);
                self.loop_labels.pop();
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
                                    self.diags
                                        .push("recv expects chan[T]", Some(arm.span.clone()));
                                    continue;
                                }
                            };
                            if let Some((name, ok_name)) = bind {
                                let class = self.defs.classify(&elem_ty).unwrap_or(TypeClass::Copy);
                                self.env.declare(
                                    name.clone(),
                                    VarInfo {
                                        ty: elem_ty,
                                        trait_obj: None,
                                        mutable: true,
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
                                        trait_obj: None,
                                        mutable: true,
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
                    ExprKind::Call {
                        callee, type_args, ..
                    } => {
                        if !type_args.is_empty() {
                            self.diags
                                .push("go does not accept type arguments", Some(span.clone()));
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
                let (callee, _, _) = match &expr.kind {
                    ExprKind::Call {
                        callee,
                        type_args,
                        args,
                    } => (callee, type_args, args),
                    _ => {
                        self.diags
                            .push("defer expects a call expression", Some(span.clone()));
                        return;
                    }
                };
                if !matches!(callee.kind, ExprKind::Ident(_)) {
                    self.diags
                        .push("defer expects a direct call", Some(span.clone()));
                } else if let ExprKind::Ident(name) = &callee.kind
                    && !self.funcs.contains_key(name)
                    && !is_intrinsic_name(name)
                {
                    self.diags
                        .push("defer expects a direct call", Some(span.clone()));
                }
                let _ = self.check_expr(expr);
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> Option<ExprResult> {
        self.check_expr_with_mode(expr, true, None)
    }

    fn check_expr_no_move(&mut self, expr: &Expr) -> Option<ExprResult> {
        self.check_expr_with_mode(expr, false, None)
    }

    fn check_expr_expected(&mut self, expr: &Expr, expected: &Type) -> Option<ExprResult> {
        if let ExprKind::Call {
            callee,
            type_args,
            args,
        } = &expr.kind
            && type_args.is_empty()
            && let ExprKind::Field {
                base,
                name: variant,
            } = &callee.kind
            && let ExprKind::Ident(enum_name) = &base.kind
            && (enum_name == "Result" || enum_name == "result")
        {
            if let Some(res) =
                self.check_result_ctor(variant, type_args, args, &expr.span, Some(expected))
            {
                return self.record_expr(expr, res);
            }
            return None;
        }

        self.check_expr_with_mode(expr, true, Some(expected))
    }

    fn record_expr(&mut self, expr: &Expr, res: ExprResult) -> Option<ExprResult> {
        self.expr_types.insert(expr.id, res.ty.clone());
        Some(res)
    }

    fn closure_captures(&self, params: &[ClosureParam], body: &BlockOrExpr) -> Vec<String> {
        let enclosing = self.env.vars.keys().cloned().collect::<HashSet<_>>();
        collect_closure_captures(params, body, &enclosing)
    }

    fn apply_closure_capture_policy(&mut self, captures: &[String], span: &Span, consume: bool) {
        for name in captures {
            let Some(var) = self.env.vars.get_mut(name) else {
                continue;
            };
            match var.class {
                TypeClass::Copy => {}
                TypeClass::View => {
                    self.diags.push(
                        format!("{} (`{}` captured by closure)", DIAG_VIEW_ESCAPE, name),
                        Some(span.clone()),
                    );
                }
                TypeClass::Linear => {
                    if consume {
                        match var.state {
                            LinearState::Alive => {
                                var.state = LinearState::Moved;
                            }
                            _ => {
                                self.diags.push(
                                    format!("{} (`{}`)", DIAG_OWN_USE_AFTER_MOVE, name),
                                    Some(span.clone()),
                                );
                            }
                        }
                    } else if var.state != LinearState::Alive {
                        self.diags.push(
                            format!("{} (`{}`)", DIAG_OWN_USE_AFTER_MOVE, name),
                            Some(span.clone()),
                        );
                    }
                }
            }
        }
    }

    fn declare_pattern_binding(&mut self, name: &str, ty: Type) {
        let class = self.defs.classify(&ty).unwrap_or(TypeClass::Copy);
        let info = VarInfo {
            ty,
            trait_obj: None,
            mutable: true,
            class,
            state: LinearState::Alive,
            borrowed_shared: 0,
            borrowed_mut: 0,
            view_of: None,
            view_is_mut: false,
        };
        self.env.declare(name.to_string(), info);
    }

    fn pattern_bindings_match(
        expected: &HashMap<String, Type>,
        actual: &HashMap<String, Type>,
    ) -> bool {
        if expected.len() != actual.len() {
            return false;
        }
        for (name, expected_ty) in expected {
            let Some(actual_ty) = actual.get(name) else {
                return false;
            };
            if !type_eq(expected_ty, actual_ty) {
                return false;
            }
        }
        true
    }

    fn check_match_pattern(&mut self, pattern: &Pattern, scrut_ty: &Type, span: &Span) {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Bool(_) => {
                if !type_eq(scrut_ty, &Type::Builtin(BuiltinType::Bool)) {
                    self.diags
                        .push("match pattern expects bool", Some(span.clone()));
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
                    self.diags
                        .push("match pattern expects integer", Some(span.clone()));
                }
            }
            Pattern::Ident(name) => {
                self.declare_pattern_binding(name, scrut_ty.clone());
            }
            Pattern::Or(patterns) => {
                if patterns.is_empty() {
                    self.diags.push(
                        "or-pattern must contain at least one alternative",
                        Some(span.clone()),
                    );
                    return;
                }
                let mut canonical_binds: Option<HashMap<String, Type>> = None;
                for pat in patterns {
                    let snapshot = self.env.clone();
                    let scope_len_before = self.env.scopes.last().map(|s| s.len()).unwrap_or(0);
                    self.check_match_pattern(pat, scrut_ty, span);
                    let mut binds = HashMap::new();
                    if let Some(scope) = self.env.scopes.last() {
                        for (name, _) in scope.iter().skip(scope_len_before) {
                            if let Some(info) = self.env.vars.get(name) {
                                binds.insert(name.clone(), info.ty.clone());
                            }
                        }
                    }
                    self.env = snapshot;
                    if let Some(expected) = &canonical_binds {
                        if !Self::pattern_bindings_match(expected, &binds) {
                            self.diags.push(
                                "or-pattern alternatives must bind the same names with the same types",
                                Some(span.clone()),
                            );
                            return;
                        }
                    } else {
                        canonical_binds = Some(binds);
                    }
                }
                if let Some(mut binds) = canonical_binds {
                    let mut names = binds.keys().cloned().collect::<Vec<_>>();
                    names.sort();
                    for name in names {
                        if let Some(ty) = binds.remove(&name) {
                            self.declare_pattern_binding(&name, ty);
                        }
                    }
                }
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
                        self.diags
                            .push("enum pattern arity mismatch", Some(span.clone()));
                    }
                    if let Some(bind) = binds.first()
                        && bind != "_"
                    {
                        self.declare_pattern_binding(bind, field_ty);
                    }
                    return;
                }
                let scrut_name = match scrut_ty {
                    Type::Named(name) => name.clone(),
                    _ => {
                        self.diags
                            .push("enum pattern expects enum type", Some(span.clone()));
                        return;
                    }
                };
                if scrut_name != *enum_name {
                    self.diags.push(
                        "enum pattern does not match scrutinee type",
                        Some(span.clone()),
                    );
                    return;
                }
                let def = match self.defs.get(&scrut_name) {
                    Some(TypeDefKind::Enum(def)) => def,
                    _ => {
                        self.diags
                            .push("enum pattern expects enum type", Some(span.clone()));
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
                    self.diags
                        .push("enum pattern arity mismatch", Some(span.clone()));
                }
                for (idx, field_ty) in fields.iter().enumerate() {
                    if let Some(bind) = binds.get(idx)
                        && bind != "_"
                    {
                        self.declare_pattern_binding(bind, field_ty.clone());
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
        expected: Option<&Type>,
    ) -> Option<ExprResult> {
        let mut ok_ty: Option<Type> = None;
        let mut err_ty: Option<Type> = None;
        match type_args.len() {
            2 => {
                ok_ty = resolve_type(&type_args[0], self.defs, self.diags);
                err_ty = resolve_type(&type_args[1], self.defs, self.diags);
            }
            0 => {
                if let Some(Type::Result(ok, err)) = expected {
                    ok_ty = Some(ok.as_ref().clone());
                    err_ty = Some(err.as_ref().clone());
                }
            }
            _ => {
                self.diags.push(
                    "Result constructors require zero or two type arguments",
                    Some(span.clone()),
                );
                return None;
            }
        }

        match variant {
            "Ok" | "ok" => {
                if args.len() != 1 {
                    self.diags
                        .push("Result.Ok expects 1 argument", Some(span.clone()));
                }
                if let Some(arg) = args.first() {
                    let res = if let Some(expected_ok) = ok_ty.as_ref() {
                        self.check_expr_expected(arg, expected_ok)?
                    } else {
                        self.check_expr(arg)?
                    };
                    let current_ok = ok_ty.get_or_insert_with(|| res.ty.clone()).clone();
                    let _ = err_ty
                        .get_or_insert(Type::Builtin(BuiltinType::Error))
                        .clone();
                    if view_arg_not_allowed(self.defs, &res.ty, &current_ok) {
                        self.diags.push(
                            "view arguments can only be passed to intrinsics",
                            Some(arg.span.clone()),
                        );
                    }
                    if !type_eq(&res.ty, &current_ok) {
                        self.diags
                            .push("argument type mismatch", Some(arg.span.clone()));
                    }
                } else {
                    let _ = ok_ty.get_or_insert(Type::Builtin(BuiltinType::Unit));
                    let _ = err_ty.get_or_insert(Type::Builtin(BuiltinType::Error));
                }
            }
            "Err" | "err" => {
                if args.len() != 1 {
                    self.diags
                        .push("Result.Err expects 1 argument", Some(span.clone()));
                }
                if let Some(arg) = args.first() {
                    let res = if let Some(expected_err) = err_ty.as_ref() {
                        self.check_expr_expected(arg, expected_err)?
                    } else {
                        self.check_expr(arg)?
                    };
                    let _ = ok_ty
                        .get_or_insert(Type::Builtin(BuiltinType::Unit))
                        .clone();
                    let current_err = err_ty.get_or_insert_with(|| res.ty.clone()).clone();
                    if view_arg_not_allowed(self.defs, &res.ty, &current_err) {
                        self.diags.push(
                            "view arguments can only be passed to intrinsics",
                            Some(arg.span.clone()),
                        );
                    }
                    if !type_eq(&res.ty, &current_err) {
                        self.diags
                            .push("argument type mismatch", Some(arg.span.clone()));
                    }
                } else {
                    let _ = ok_ty.get_or_insert(Type::Builtin(BuiltinType::Unit));
                    let _ = err_ty.get_or_insert(Type::Builtin(BuiltinType::Error));
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

        let ok_ty = ok_ty.unwrap_or(Type::Builtin(BuiltinType::Unit));
        let err_ty = err_ty.unwrap_or(Type::Builtin(BuiltinType::Error));
        Some(ExprResult {
            ty: Type::Result(Box::new(ok_ty), Box::new(err_ty)),
            borrow: None,
        })
    }

    fn match_is_exhaustive(&self, scrut_ty: &Type, arms: &[MatchArm]) -> bool {
        fn pattern_contains<F: Fn(&Pattern) -> bool>(pat: &Pattern, pred: &F) -> bool {
            if pred(pat) {
                return true;
            }
            match pat {
                Pattern::Or(items) => items.iter().any(|p| pattern_contains(p, pred)),
                _ => false,
            }
        }

        if arms.iter().any(|arm| {
            arm.guard.is_none()
                && pattern_contains(&arm.pattern, &|p| {
                    matches!(p, Pattern::Wildcard | Pattern::Ident(_))
                })
        }) {
            return true;
        }
        match scrut_ty {
            Type::Builtin(BuiltinType::Bool) => {
                let mut saw_true = false;
                let mut saw_false = false;
                for arm in arms {
                    if arm.guard.is_some() {
                        continue;
                    }
                    if pattern_contains(&arm.pattern, &|p| matches!(p, Pattern::Bool(true))) {
                        saw_true = true;
                    }
                    if pattern_contains(&arm.pattern, &|p| matches!(p, Pattern::Bool(false))) {
                        saw_false = true;
                    }
                }
                saw_true && saw_false
            }
            Type::Result(_, _) => {
                let mut saw_ok = false;
                let mut saw_err = false;
                for arm in arms {
                    if arm.guard.is_some() {
                        continue;
                    }
                    if pattern_contains(&arm.pattern, &|p| {
                        matches!(
                            p,
                            Pattern::Variant {
                                enum_name,
                                variant,
                                ..
                            } if (enum_name == "Result" || enum_name == "result")
                                && (variant == "Ok" || variant == "ok")
                        )
                    }) {
                        saw_ok = true;
                    }
                    if pattern_contains(&arm.pattern, &|p| {
                        matches!(
                            p,
                            Pattern::Variant {
                                enum_name,
                                variant,
                                ..
                            } if (enum_name == "Result" || enum_name == "result")
                                && (variant == "Err" || variant == "err")
                        )
                    }) {
                        saw_err = true;
                    }
                }
                saw_ok && saw_err
            }
            Type::Named(name) => match self.defs.get(name) {
                Some(TypeDefKind::Enum(def)) => {
                    let mut seen = HashSet::new();
                    for arm in arms {
                        if arm.guard.is_some() {
                            continue;
                        }
                        for (variant_name, _) in &def.variants {
                            if pattern_contains(&arm.pattern, &|p| {
                                matches!(
                                    p,
                                    Pattern::Variant {
                                        enum_name,
                                        variant,
                                        ..
                                    } if enum_name == name && variant == variant_name
                                )
                            }) {
                                seen.insert(variant_name.clone());
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
        if !is_intrinsic_name(callee_name) {
            return None;
        }
        if let Some(msg) = intrinsic_internal_std_only_error(callee_name)
            && !self.is_std
        {
            self.diags.push(msg, Some(span.clone()));
        }
        if let Some(msg) = intrinsic_type_args_error(callee_name, type_args.len()) {
            self.diags.push(msg, Some(span.clone()));
            return None;
        }
        if let Some(msg) = intrinsic_args_error(callee_name, args.len()) {
            self.diags.push(msg, Some(span.clone()));
            return None;
        }
        match callee_name {
            "panic" => {
                if !type_args.is_empty() {
                    self.diags
                        .push("panic does not take type arguments", Some(span.clone()));
                }
                if args.len() != 1 {
                    self.diags
                        .push("panic expects 1 argument", Some(span.clone()));
                    return None;
                }
                let msg = self.check_expr(&args[0])?;
                if !type_eq(&msg.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags
                        .push("panic expects string message", Some(args[0].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::Unit),
                    borrow: None,
                });
            }
            "recover" => {
                if !type_args.is_empty() {
                    self.diags
                        .push("recover does not take type arguments", Some(span.clone()));
                }
                if !args.is_empty() {
                    self.diags
                        .push("recover expects 0 arguments", Some(span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::Error),
                    borrow: None,
                });
            }
            "__gost_println" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_println is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_println does not take type arguments",
                        Some(span.clone()),
                    );
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
                    self.diags.push(
                        "string_len does not take type arguments",
                        Some(span.clone()),
                    );
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
                    self.diags.push(
                        "string_get does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 2 {
                    self.diags
                        .push("string_get expects 2 arguments", Some(span.clone()));
                    return None;
                }
                let s = self.check_expr(&args[0])?;
                if !type_eq(&s.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags.push(
                        "string_get expects string as first argument",
                        Some(args[0].span.clone()),
                    );
                }
                let idx = self.check_expr_expected(&args[1], &Type::Builtin(BuiltinType::I64))?;
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
                    self.diags.push(
                        "string_slice does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 3 {
                    self.diags
                        .push("string_slice expects 3 arguments", Some(span.clone()));
                    return None;
                }
                let s = self.check_expr(&args[0])?;
                if !type_eq(&s.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags.push(
                        "string_slice expects string as first argument",
                        Some(args[0].span.clone()),
                    );
                }
                let start = self.check_expr_expected(&args[1], &Type::Builtin(BuiltinType::I64))?;
                let len = self.check_expr_expected(&args[2], &Type::Builtin(BuiltinType::I64))?;
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
                    self.diags.push(
                        "string_concat does not take type arguments",
                        Some(span.clone()),
                    );
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
                    self.diags.push(
                        "string_from_byte does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 1 {
                    self.diags
                        .push("string_from_byte expects 1 argument", Some(span.clone()));
                    return None;
                }
                let b = self.check_expr(&args[0])?;
                if !matches!(b.ty, Type::Builtin(BuiltinType::I32 | BuiltinType::I64)) {
                    self.diags.push(
                        "string_from_byte expects i32 byte value",
                        Some(args[0].span.clone()),
                    );
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
                    self.diags.push(
                        "asm template must be string literal",
                        Some(args[0].span.clone()),
                    );
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
                        self.diags
                            .push("asm operands cannot be unit", Some(arg.span.clone()));
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
                            if explicit_style || spec.readwrite_outputs > 0 {
                                operand_results
                                    .first()
                                    .cloned()
                                    .unwrap_or(Type::Builtin(BuiltinType::I64))
                            } else {
                                Type::Builtin(BuiltinType::I64)
                            }
                        } else if explicit_style
                            || (spec.readwrite_outputs == spec.outputs.len()
                                && operand_results.len() >= spec.outputs.len())
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
                    self.diags.push(
                        "asm_label expects string argument",
                        Some(args[0].span.clone()),
                    );
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
                    self.diags.push(
                        "asm_goto expects at most 1 type argument",
                        Some(span.clone()),
                    );
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
                        self.diags
                            .push("asm_goto operands cannot be unit", Some(arg.span.clone()));
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
                    if explicit_style || spec.readwrite_outputs > 0 {
                        operand_results
                            .first()
                            .cloned()
                            .unwrap_or(Type::Builtin(BuiltinType::I64))
                    } else {
                        Type::Builtin(BuiltinType::I64)
                    }
                } else if explicit_style
                    || (spec.readwrite_outputs == spec.outputs.len()
                        && operand_results.len() >= spec.outputs.len())
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
                    self.diags.push(
                        "__gost_error_new does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 1 {
                    self.diags
                        .push("__gost_error_new expects 1 argument", Some(span.clone()));
                }
                if let Some(arg) = args.first() {
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
            "__gost_error_message" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_error_message is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_error_message does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 1 {
                    self.diags.push(
                        "__gost_error_message expects 1 argument",
                        Some(span.clone()),
                    );
                }
                if let Some(arg) = args.first() {
                    let res = self.check_expr(arg)?;
                    if !type_eq(&res.ty, &Type::Builtin(BuiltinType::Error)) {
                        self.diags
                            .push("__gost_error_message expects error", Some(arg.span.clone()));
                    }
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::String),
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
                    self.diags.push(
                        "__gost_singleton_acquire does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 1 {
                    self.diags.push(
                        "__gost_singleton_acquire expects 1 argument",
                        Some(span.clone()),
                    );
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                if !type_eq(&a0.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags.push(
                        "__gost_singleton_acquire expects string name",
                        Some(args[0].span.clone()),
                    );
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I32),
                    borrow: None,
                });
            }
            "__gost_now_ms" => {
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_now_ms does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if !args.is_empty() {
                    self.diags
                        .push("__gost_now_ms expects 0 arguments", Some(span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I64),
                    borrow: None,
                });
            }
            "__gost_process_exit" => {
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_process_exit does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 1 {
                    self.diags
                        .push("__gost_process_exit expects 1 argument", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                if !type_eq(&a0.ty, &Type::Builtin(BuiltinType::I32))
                    && !type_eq(&a0.ty, &Type::Builtin(BuiltinType::I64))
                {
                    self.diags.push(
                        "__gost_process_exit expects i32 or i64",
                        Some(args[0].span.clone()),
                    );
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I32),
                    borrow: None,
                });
            }
            "__gost_sync_mutex_new" | "__gost_sync_waitgroup_new" | "__gost_sync_once_new" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_sync_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_sync_* does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if !args.is_empty() {
                    self.diags
                        .push("__gost_sync_* expects 0 arguments", Some(span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I64),
                    borrow: None,
                });
            }
            "__gost_sync_mutex_lock"
            | "__gost_sync_mutex_try_lock"
            | "__gost_sync_mutex_unlock"
            | "__gost_sync_waitgroup_wait"
            | "__gost_sync_once_begin" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_sync_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_sync_* does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 1 {
                    self.diags
                        .push("__gost_sync_* expects 1 argument", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                if !matches!(a0.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags.push(
                        "__gost_sync_* expects handle i64",
                        Some(args[0].span.clone()),
                    );
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I32),
                    borrow: None,
                });
            }
            "__gost_sync_waitgroup_add" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_sync_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_sync_* does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 2 {
                    self.diags.push(
                        "__gost_sync_waitgroup_add expects 2 arguments",
                        Some(span.clone()),
                    );
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                let a1 = self.check_expr_expected(&args[1], &Type::Builtin(BuiltinType::I32))?;
                if !matches!(a0.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags.push(
                        "__gost_sync_waitgroup_add expects handle i64",
                        Some(args[0].span.clone()),
                    );
                }
                if !matches!(a1.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags.push(
                        "__gost_sync_waitgroup_add expects delta i32",
                        Some(args[1].span.clone()),
                    );
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I32),
                    borrow: None,
                });
            }
            "__gost_os_last_status" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_os_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_os_last_status does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if !args.is_empty() {
                    self.diags.push(
                        "__gost_os_last_status expects 0 arguments",
                        Some(span.clone()),
                    );
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I32),
                    borrow: None,
                });
            }
            "__gost_os_last_error"
            | "__gost_os_last_output"
            | "__gost_os_getwd"
            | "__gost_os_args" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_os_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_os_* does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if !args.is_empty() {
                    self.diags
                        .push("__gost_os_* expects 0 arguments", Some(span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::String),
                    borrow: None,
                });
            }
            "__gost_os_exec" | "__gost_os_pipe" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_os_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_os_* does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 1 {
                    self.diags
                        .push("__gost_os_* expects 1 argument", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                if !type_eq(&a0.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags.push(
                        "__gost_os_* expects string argument",
                        Some(args[0].span.clone()),
                    );
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I32),
                    borrow: None,
                });
            }
            "__gost_os_read_file" | "__gost_os_readdir" | "__gost_os_getenv" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_os_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_os_* does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 1 {
                    self.diags
                        .push("__gost_os_* expects 1 argument", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                if !type_eq(&a0.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags.push(
                        "__gost_os_* expects string argument",
                        Some(args[0].span.clone()),
                    );
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::String),
                    borrow: None,
                });
            }
            "__gost_os_write_file" | "__gost_os_setenv" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_os_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_os_* does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 2 {
                    self.diags
                        .push("__gost_os_* expects 2 arguments", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                let a1 = self.check_expr(&args[1])?;
                if !type_eq(&a0.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags.push(
                        "__gost_os_* expects string argument",
                        Some(args[0].span.clone()),
                    );
                }
                if !type_eq(&a1.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags.push(
                        "__gost_os_* expects string argument",
                        Some(args[1].span.clone()),
                    );
                }
                let ret = if callee_name == "__gost_os_write_file" {
                    Type::Builtin(BuiltinType::I64)
                } else {
                    Type::Builtin(BuiltinType::I32)
                };
                return Some(ExprResult {
                    ty: ret,
                    borrow: None,
                });
            }
            "__gost_os_remove" | "__gost_os_mkdir" | "__gost_os_chdir" | "__gost_os_stat_size" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_os_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_os_* does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 1 {
                    self.diags
                        .push("__gost_os_* expects 1 argument", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                if !type_eq(&a0.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags.push(
                        "__gost_os_* expects string argument",
                        Some(args[0].span.clone()),
                    );
                }
                let ret = if callee_name == "__gost_os_stat_size" {
                    Type::Builtin(BuiltinType::I64)
                } else {
                    Type::Builtin(BuiltinType::I32)
                };
                return Some(ExprResult {
                    ty: ret,
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
                    self.diags.push(
                        "__gost_net_last_status does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if !args.is_empty() {
                    self.diags.push(
                        "__gost_net_last_status expects 0 arguments",
                        Some(span.clone()),
                    );
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
                    self.diags.push(
                        "__gost_net_last_error does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if !args.is_empty() {
                    self.diags.push(
                        "__gost_net_last_error expects 0 arguments",
                        Some(span.clone()),
                    );
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::String),
                    borrow: None,
                });
            }
            "__gost_net_tcp_listen"
            | "__gost_net_tcp_connect"
            | "__gost_net_udp_bind"
            | "__gost_net_udp_connect"
            | "__gost_net_ws_connect" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_net_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_net_* does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 1 {
                    self.diags
                        .push("__gost_net_* expects 1 argument", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                if !type_eq(&a0.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags.push(
                        "__gost_net_* expects string address",
                        Some(args[0].span.clone()),
                    );
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I64),
                    borrow: None,
                });
            }
            "__gost_net_tcp_accept"
            | "__gost_net_tcp_close"
            | "__gost_net_udp_close"
            | "__gost_net_ws_close" => {
                if !self.is_std {
                    self.diags.push(
                        "__gost_net_* is internal to the standard library",
                        Some(span.clone()),
                    );
                }
                if !type_args.is_empty() {
                    self.diags.push(
                        "__gost_net_* does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 1 {
                    self.diags
                        .push("__gost_net_* expects 1 argument", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                if !matches!(a0.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags.push(
                        "__gost_net_* expects handle i64",
                        Some(args[0].span.clone()),
                    );
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
                    self.diags.push(
                        "__gost_net_ws_send_text does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 2 {
                    self.diags.push(
                        "__gost_net_ws_send_text expects 2 arguments",
                        Some(span.clone()),
                    );
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                let a1 = self.check_expr(&args[1])?;
                if !matches!(a0.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags.push(
                        "__gost_net_ws_send_text expects handle i64",
                        Some(args[0].span.clone()),
                    );
                }
                if !type_eq(&a1.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags.push(
                        "__gost_net_ws_send_text expects string payload",
                        Some(args[1].span.clone()),
                    );
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
                    self.diags.push(
                        "__gost_net_* does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 2 {
                    self.diags
                        .push("__gost_net_* expects 2 arguments", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                let a1 = self.check_expr(&args[1])?;
                if !matches!(a0.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags.push(
                        "__gost_net_* expects handle i64",
                        Some(args[0].span.clone()),
                    );
                }
                if !type_eq(&a1.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags.push(
                        "__gost_net_* expects string payload",
                        Some(args[1].span.clone()),
                    );
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
                    self.diags.push(
                        "__gost_net_udp_send_to does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 3 {
                    self.diags.push(
                        "__gost_net_udp_send_to expects 3 arguments",
                        Some(span.clone()),
                    );
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                let a1 = self.check_expr(&args[1])?;
                let a2 = self.check_expr(&args[2])?;
                if !matches!(a0.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags.push(
                        "__gost_net_udp_send_to expects handle i64",
                        Some(args[0].span.clone()),
                    );
                }
                if !type_eq(&a1.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags.push(
                        "__gost_net_udp_send_to expects string address",
                        Some(args[1].span.clone()),
                    );
                }
                if !type_eq(&a2.ty, &Type::Builtin(BuiltinType::String)) {
                    self.diags.push(
                        "__gost_net_udp_send_to expects string payload",
                        Some(args[2].span.clone()),
                    );
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
                    self.diags.push(
                        "__gost_net_* does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 2 {
                    self.diags
                        .push("__gost_net_* expects 2 arguments", Some(span.clone()));
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                let a1 = self.check_expr_expected(&args[1], &Type::Builtin(BuiltinType::I32))?;
                if !matches!(a0.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags.push(
                        "__gost_net_* expects handle i64",
                        Some(args[0].span.clone()),
                    );
                }
                if !matches!(a1.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags.push(
                        "__gost_net_* expects i32 max length",
                        Some(args[1].span.clone()),
                    );
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
                    self.diags.push(
                        "__gost_net_ws_recv_text does not take type arguments",
                        Some(span.clone()),
                    );
                }
                if args.len() != 1 {
                    self.diags.push(
                        "__gost_net_ws_recv_text expects 1 argument",
                        Some(span.clone()),
                    );
                    return None;
                }
                let a0 = self.check_expr(&args[0])?;
                if !matches!(a0.ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags.push(
                        "__gost_net_ws_recv_text expects handle i64",
                        Some(args[0].span.clone()),
                    );
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
                    self.diags.push(
                        "__gost_net_http_request does not take type arguments",
                        Some(span.clone()),
                    );
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
                    self.diags
                        .push("iterator value cannot escape for-loop", Some(span.clone()));
                }
                if !type_args.is_empty() {
                    self.diags.push(
                        "iterator helpers do not take type arguments",
                        Some(span.clone()),
                    );
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
                                    self.diags
                                        .push("iter expects &slice", Some(args[0].span.clone()));
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
                                self.diags
                                    .push("iter expects &slice", Some(args[0].span.clone()));
                                return None;
                            }
                        };
                        if callee_name == "iter" && is_mut {
                            self.diags
                                .push("iter expects &slice", Some(args[0].span.clone()));
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
                                self.diags
                                    .push("filter expects iterator", Some(args[0].span.clone()));
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
                            self.diags
                                .push("filter predicate type mismatch", Some(args[1].span.clone()));
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
                            self.diags
                                .push("map expects two arguments", Some(span.clone()));
                            return None;
                        }
                        let iter_ty = self.check_expr_no_move(&args[0])?.ty;
                        let item_ty = match iter_ty {
                            Type::Iter(inner) => *inner,
                            _ => {
                                self.diags
                                    .push("map expects iterator", Some(args[0].span.clone()));
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
                            self.diags
                                .push("map function type mismatch", Some(args[1].span.clone()));
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
            "__gost_select_wait" => {
                for arg in args {
                    let arg_ty = self.check_expr(arg)?.ty;
                    if !matches!(arg_ty, Type::Chan(_))
                        && !type_eq(&arg_ty, &Type::Builtin(BuiltinType::I32))
                    {
                        self.diags.push(
                            "__gost_select_wait expects chan[T] and i32 op arguments",
                            Some(arg.span.clone()),
                        );
                    }
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I32),
                    borrow: None,
                });
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
                let cap_ty = self
                    .check_expr_expected(&args[0], &Type::Builtin(BuiltinType::I64))?
                    .ty;
                if !type_eq(&cap_ty, &Type::Builtin(BuiltinType::I64))
                    && !type_eq(&cap_ty, &Type::Builtin(BuiltinType::I32))
                {
                    self.diags
                        .push("make_chan expects i64 cap", Some(args[0].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Chan(Box::new(elem_ty)),
                    borrow: None,
                });
            }
            "__gost_chan_can_send" | "__gost_chan_can_recv" => {
                if !type_args.is_empty() {
                    self.diags.push(
                        format!("{} does not take type arguments", callee_name),
                        Some(span.clone()),
                    );
                }
                if args.len() != 1 {
                    self.diags.push(
                        format!("{} expects 1 argument", callee_name),
                        Some(span.clone()),
                    );
                    return None;
                }
                let chan_ty = self.check_expr(&args[0])?.ty;
                if !matches!(chan_ty, Type::Chan(_)) {
                    self.diags.push(
                        format!("{} expects chan[T]", callee_name),
                        Some(args[0].span.clone()),
                    );
                }
                return Some(ExprResult {
                    ty: Type::Builtin(BuiltinType::I32),
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
                let len_ty = self
                    .check_expr_expected(&args[0], &Type::Builtin(BuiltinType::I64))?
                    .ty;
                let cap_ty = self
                    .check_expr_expected(&args[1], &Type::Builtin(BuiltinType::I64))?
                    .ty;
                if !matches!(len_ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags
                        .push("make_slice expects i64 len", Some(args[0].span.clone()));
                }
                if !matches!(cap_ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags
                        .push("make_slice expects i64 cap", Some(args[1].span.clone()));
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
                    self.diags.push(
                        "slice_get_copy expects one type argument",
                        Some(span.clone()),
                    );
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
                let idx_ty = self
                    .check_expr_expected(&args[1], &Type::Builtin(BuiltinType::I64))?
                    .ty;
                if !matches!(idx_ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
                    self.diags.push(
                        "slice_get_copy expects i64 index",
                        Some(args[1].span.clone()),
                    );
                }
                if !self.defs.is_copy_type(&elem_ty) {
                    self.diags
                        .push("slice_get_copy requires Copy element", Some(span.clone()));
                }
                return Some(ExprResult {
                    ty: elem_ty,
                    borrow: None,
                });
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
                let idx_ty = self
                    .check_expr_expected(&args[1], &Type::Builtin(BuiltinType::I64))?
                    .ty;
                if !matches!(idx_ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
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
                let idx_ty = self
                    .check_expr_expected(&args[1], &Type::Builtin(BuiltinType::I64))?
                    .ty;
                if !matches!(idx_ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
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
                    self.diags.push(
                        "slice_push expects element type",
                        Some(args[1].span.clone()),
                    );
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
                    self.diags.push(
                        "shared_new expects element type",
                        Some(args[0].span.clone()),
                    );
                }
                return Some(ExprResult {
                    ty: Type::Shared(Box::new(elem_ty)),
                    borrow: None,
                });
            }
            "own_new" => {
                if type_args.len() != 1 {
                    self.diags
                        .push("own_new expects one type argument", Some(span.clone()));
                    return None;
                }
                if args.len() != 1 {
                    self.diags
                        .push("own_new expects one argument", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let val_ty = self.check_expr(&args[0])?.ty;
                if !type_eq(&val_ty, &elem_ty) {
                    self.diags
                        .push("own_new expects element type", Some(args[0].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Own(Box::new(elem_ty)),
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
                        self.diags.push(
                            "shared_get expects ref shared[T]",
                            Some(args[0].span.clone()),
                        );
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
            "own_borrow" | "own_borrow_mut" => {
                if type_args.len() != 1 {
                    self.diags.push(
                        "handle intrinsic expects one type argument",
                        Some(span.clone()),
                    );
                    return None;
                }
                if args.len() != 1 {
                    self.diags
                        .push("handle intrinsic expects one argument", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let arg_res = self.check_expr(&args[0])?;
                match (&arg_res.ty, callee_name) {
                    (Type::Ref(inner), "own_borrow") | (Type::MutRef(inner), "own_borrow")
                        if **inner == Type::Own(Box::new(elem_ty.clone())) => {}
                    (Type::MutRef(inner), "own_borrow_mut")
                        if **inner == Type::Own(Box::new(elem_ty.clone())) => {}
                    (Type::Ref(inner) | Type::MutRef(inner), "own_borrow_mut")
                        if matches!(inner.as_ref(), Type::Alias(_)) =>
                    {
                        self.diags
                            .push(DIAG_ALIAS_MUT_BORROW, Some(args[0].span.clone()));
                    }
                    _ => {
                        let msg = if callee_name == "own_borrow_mut" {
                            "own_borrow_mut expects mutref own[T]"
                        } else {
                            "own_borrow expects ref own[T]"
                        };
                        self.diags.push(msg, Some(args[0].span.clone()));
                    }
                }
                let borrow = arg_res.borrow.map(|b| BorrowInfo {
                    base: b.base,
                    is_mut: callee_name == "own_borrow_mut",
                });
                let ret_ty = if callee_name == "own_borrow_mut" {
                    Type::MutRef(Box::new(elem_ty))
                } else {
                    Type::Ref(Box::new(elem_ty))
                };
                return Some(ExprResult { ty: ret_ty, borrow });
            }
            "freeze" => {
                if type_args.len() != 1 {
                    self.diags
                        .push("freeze expects one type argument", Some(span.clone()));
                    return None;
                }
                if args.len() != 1 {
                    self.diags
                        .push("freeze expects one argument", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let arg_ty = self.check_expr(&args[0])?.ty;
                if !matches!(arg_ty, Type::Own(inner) if *inner == elem_ty) {
                    self.diags
                        .push(DIAG_FREEZE_CONSUMES_OWNER, Some(args[0].span.clone()));
                }
                return Some(ExprResult {
                    ty: Type::Alias(Box::new(elem_ty)),
                    borrow: None,
                });
            }
            "alias_borrow" => {
                if type_args.len() != 1 {
                    self.diags.push(
                        "handle intrinsic expects one type argument",
                        Some(span.clone()),
                    );
                    return None;
                }
                if args.len() != 1 {
                    self.diags
                        .push("handle intrinsic expects one argument", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let arg_res = self.check_expr(&args[0])?;
                match arg_res.ty {
                    Type::Ref(inner) | Type::MutRef(inner)
                        if *inner == Type::Alias(Box::new(elem_ty.clone())) => {}
                    _ => {
                        self.diags.push(
                            "alias_borrow expects ref alias[T]",
                            Some(args[0].span.clone()),
                        );
                    }
                }
                let borrow = arg_res.borrow.map(|b| BorrowInfo {
                    base: b.base,
                    is_mut: false,
                });
                return Some(ExprResult {
                    ty: Type::Ref(Box::new(elem_ty)),
                    borrow,
                });
            }
            "own_into_value" => {
                if type_args.len() != 1 {
                    self.diags.push(
                        "handle intrinsic expects one type argument",
                        Some(span.clone()),
                    );
                    return None;
                }
                if args.len() != 1 {
                    self.diags
                        .push("handle intrinsic expects one argument", Some(span.clone()));
                    return None;
                }
                let elem_ty = resolve_type(&type_args[0], self.defs, self.diags)?;
                let arg_ty = self.check_expr(&args[0])?.ty;
                if !matches!(&arg_ty, Type::Own(inner) if **inner == elem_ty) {
                    if matches!(arg_ty, Type::Alias(_)) {
                        self.diags
                            .push(DIAG_ALIAS_TO_OWN, Some(args[0].span.clone()));
                    } else {
                        self.diags
                            .push("own_into_value expects own[T]", Some(args[0].span.clone()));
                    }
                }
                return Some(ExprResult {
                    ty: elem_ty,
                    borrow: None,
                });
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
                if !self.defs.can_be_map_key(&key_ty) {
                    self.diags.push(
                        "map key type is not supported by runtime map",
                        Some(span.clone()),
                    );
                }
                let cap_ty = self
                    .check_expr_expected(&args[0], &Type::Builtin(BuiltinType::I64))?
                    .ty;
                if !matches!(cap_ty, Type::Builtin(BuiltinType::I64 | BuiltinType::I32)) {
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
                        if *inner
                            == Type::Map(Box::new(key_ty.clone()), Box::new(val_ty.clone())) => {}
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
                        if *inner
                            == Type::Map(Box::new(key_ty.clone()), Box::new(val_ty.clone())) => {}
                    _ => {
                        self.diags.push(
                            "map_set expects mutref map[K,V]",
                            Some(args[0].span.clone()),
                        );
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
                        if *inner
                            == Type::Map(Box::new(key_ty.clone()), Box::new(val_ty.clone())) => {}
                    _ => {
                        self.diags.push(
                            "map_del expects mutref map[K,V]",
                            Some(args[0].span.clone()),
                        );
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
        if let Some(base) = self.borrow_base_ident(expr)
            && let Some(info) = self.env.vars.get(&base)
        {
            // if the base is a shared view, don't allow &mut
            if info.view_of.is_some() && !info.view_is_mut {
                return false;
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
                if let Type::Named(type_name) = base_res.ty
                    && let Some(TypeDefKind::Struct(def)) = self.defs.get(&type_name)
                {
                    if let Some(field) = def.fields.iter().find(|field| field.name == *name) {
                        if !self.can_access_struct_field(&type_name, field) {
                            self.diag_private_field(&expr.span, &type_name, name);
                            return None;
                        }
                        let base_name = self.borrow_base_ident(base);
                        return Some((field.ty.clone(), base_name));
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
                        .map(|field| field.name.clone())
                        .collect::<Vec<_>>();
                    if let Some(h) = suggest::did_you_mean(name, candidates) {
                        d = d.help(h);
                    }
                    self.diags.push_diag(d);
                    return None;
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
                        self.diags
                            .push("indexing expects a slice", Some(expr.span.clone()));
                        None
                    }
                }
            }
            ExprKind::Deref { expr: inner } => {
                let inner_res = self.check_expr_no_move(inner)?;
                let base_name = self.borrow_base_ident(inner);
                match inner_res.ty {
                    Type::Ref(inner_ty) | Type::MutRef(inner_ty) => Some((*inner_ty, base_name)),
                    _ => {
                        self.diags
                            .push("deref expects ref type", Some(expr.span.clone()));
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
                    if !var.mutable {
                        self.diags.push(
                            format!("cannot assign to immutable binding `{}`", name),
                            Some(expr.span.clone()),
                        );
                    }
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
                } else if self.consts.contains_key(name) {
                    self.diags.push(
                        format!("cannot assign to const `{}`", name),
                        Some(expr.span.clone()),
                    );
                    None
                } else if let Some(global) = self.globals.get(name) {
                    Some(global.ty.clone())
                } else if let Some(global) = self.extern_globals.get(name) {
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
                if let Type::Named(type_name) = base_res.ty
                    && let Some(TypeDefKind::Struct(def)) = self.defs.get(&type_name)
                {
                    if let Some(field) = def.fields.iter().find(|field| field.name == *name) {
                        if !self.can_access_struct_field(&type_name, field) {
                            self.diag_private_field(&expr.span, &type_name, name);
                            return None;
                        }
                        return Some(field.ty.clone());
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
                        .map(|field| field.name.clone())
                        .collect::<Vec<_>>();
                    if let Some(h) = suggest::did_you_mean(name, candidates) {
                        d = d.help(h);
                    }
                    self.diags.push_diag(d);
                    return None;
                }
                None
            }
            ExprKind::Index { base, index } => {
                let base_res = self.check_expr_no_move(base)?;
                let _ = self.check_expr(index)?;
                match slice_like_elem_type(&base_res.ty) {
                    Some(elem) => Some(elem),
                    None => {
                        self.diags
                            .push("indexing expects a slice", Some(expr.span.clone()));
                        None
                    }
                }
            }
            ExprKind::Deref { expr: inner } => {
                let inner_res = self.check_expr_no_move(inner)?;
                match inner_res.ty {
                    Type::Ref(inner) | Type::MutRef(inner) => Some(*inner),
                    _ => {
                        self.diags
                            .push("deref expects ref type", Some(expr.span.clone()));
                        None
                    }
                }
            }
            _ => {
                self.diags.push(
                    "assignment target must be addressable",
                    Some(expr.span.clone()),
                );
                None
            }
        }
    }

    fn check_expr_with_mode(
        &mut self,
        expr: &Expr,
        consume: bool,
        expected: Option<&Type>,
    ) -> Option<ExprResult> {
        let result = match &expr.kind {
            ExprKind::Bool(_) => Some(ExprResult {
                ty: Type::Builtin(BuiltinType::Bool),
                borrow: None,
            }),
            ExprKind::Int(_) => {
                let ty = if let Some(expected_ty) = expected {
                    if expected_ty.is_integer() {
                        expected_ty.clone()
                    } else {
                        Type::Builtin(BuiltinType::I32)
                    }
                } else {
                    Type::Builtin(BuiltinType::I32)
                };
                Some(ExprResult { ty, borrow: None })
            }
            ExprKind::Float(_) => {
                let ty = if let Some(expected_ty) = expected {
                    if expected_ty.is_float() {
                        expected_ty.clone()
                    } else {
                        Type::Builtin(BuiltinType::F64)
                    }
                } else {
                    Type::Builtin(BuiltinType::F64)
                };
                Some(ExprResult { ty, borrow: None })
            }
            ExprKind::Char(_) => Some(ExprResult {
                ty: Type::Builtin(BuiltinType::Char),
                borrow: None,
            }),
            ExprKind::String(_) => Some(ExprResult {
                ty: Type::Builtin(BuiltinType::String),
                borrow: None,
            }),
            ExprKind::Nil => {
                let ty = if let Some(expected_ty) = expected {
                    match expected_ty {
                        Type::Ref(_)
                        | Type::MutRef(_)
                        | Type::Slice(_)
                        | Type::Map(_, _)
                        | Type::Chan(_)
                        | Type::Own(_)
                        | Type::Alias(_)
                        | Type::Shared(_)
                        | Type::Interface
                        | Type::Result(_, _) => expected_ty.clone(),
                        _ => Type::Builtin(BuiltinType::Error),
                    }
                } else {
                    Type::Builtin(BuiltinType::Error)
                };
                Some(ExprResult { ty, borrow: None })
            }
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
                                        format!("{} (`{}`)", DIAG_OWN_USE_AFTER_MOVE, name),
                                        Some(expr.span.clone()),
                                    );
                                }
                            }
                        } else if var.state != LinearState::Alive {
                            self.diags.push(
                                format!("{} (`{}`)", DIAG_OWN_USE_AFTER_MOVE, name),
                                Some(expr.span.clone()),
                            );
                        }
                    }
                    Some(ExprResult {
                        ty: var.ty.clone(),
                        borrow: None,
                    })
                } else {
                    if let Some(c) = self.consts.get(name) {
                        return self.record_expr(
                            expr,
                            ExprResult {
                                ty: c.ty.clone(),
                                borrow: None,
                            },
                        );
                    }
                    if self.enforce_namespaced_only()
                        && let Some(alias) = self.namespaced_only_funcs.get(name)
                    {
                        self.diags.push(
                            format!(
                                "`{}` is imported via `{}`; use `{}.{}(...)`",
                                name, alias, alias, name
                            ),
                            Some(expr.span.clone()),
                        );
                        return None;
                    }
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
                        return self.record_expr(
                            expr,
                            ExprResult {
                                ty: global.ty.clone(),
                                borrow: None,
                            },
                        );
                    }
                    if let Some(global) = self.extern_globals.get(name) {
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
                    let mut candidates: Vec<String> = self.env.vars.keys().cloned().collect();
                    candidates.extend(self.consts.keys().cloned());
                    candidates.extend(self.funcs.keys().cloned());
                    candidates.extend(self.globals.keys().cloned());
                    candidates.extend(self.extern_globals.keys().cloned());
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
                        let d = Diagnostic::new(
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
                for field in &def.fields {
                    if !self.can_access_struct_field(name, field) {
                        if field_map.contains_key(&field.name) {
                            self.diag_private_field(&expr.span, name, &field.name);
                        } else {
                            self.diags.push(
                                format!(
                                    "cannot construct `{}`: field `{}` is private",
                                    name, field.name
                                ),
                                Some(expr.span.clone()),
                            );
                        }
                        continue;
                    }
                    match field_map.get(&field.name) {
                        Some(fexpr) => {
                            let res = self.check_expr_expected(fexpr, &field.ty)?;
                            if !type_eq(&res.ty, &field.ty) {
                                self.diags.push(
                                    "field initializer type mismatch",
                                    Some(fexpr.span.clone()),
                                );
                            }
                        }
                        None => {
                            self.diags.push(
                                format!("missing field `{}` in struct literal", field.name),
                                Some(expr.span.clone()),
                            );
                        }
                    }
                }

                // unknown fields
                for (fname, fexpr) in fields.iter() {
                    if !def.fields.iter().any(|field| field.name == *fname) {
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
                            .map(|field| field.name.clone())
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
            ExprKind::ArrayLit(items) => {
                let expected_array = match expected {
                    Some(Type::Array(elem, len)) => Some(((*elem.clone()), *len)),
                    _ => None,
                };
                if let Some((_, expected_len)) = &expected_array
                    && *expected_len != items.len()
                {
                    self.diags.push(
                        format!(
                            "array literal length mismatch (expected {}, found {})",
                            expected_len,
                            items.len()
                        ),
                        Some(expr.span.clone()),
                    );
                }
                let elem_ty = if let Some((expected_elem, _)) = &expected_array {
                    for item in items {
                        let res = self.check_expr_expected(item, expected_elem)?;
                        if !type_eq(&res.ty, expected_elem) {
                            self.diags
                                .push("array element type mismatch", Some(item.span.clone()));
                        }
                    }
                    expected_elem.clone()
                } else {
                    let Some(first) = items.first() else {
                        self.diags.push(
                            "cannot infer type of empty array literal",
                            Some(expr.span.clone()),
                        );
                        return None;
                    };
                    let first_ty = self.check_expr(first)?.ty;
                    for item in items.iter().skip(1) {
                        let res = self.check_expr_expected(item, &first_ty)?;
                        if !type_eq(&res.ty, &first_ty) {
                            self.diags
                                .push("array element type mismatch", Some(item.span.clone()));
                        }
                    }
                    first_ty
                };
                Some(ExprResult {
                    ty: Type::Array(Box::new(elem_ty), items.len()),
                    borrow: None,
                })
            }
            ExprKind::Tuple(items) => {
                let expected_items = match expected {
                    Some(Type::Tuple(expected_items)) if expected_items.len() == items.len() => {
                        Some(expected_items)
                    }
                    _ => None,
                };
                let mut tys = Vec::with_capacity(items.len());
                for (idx, item) in items.iter().enumerate() {
                    let res = if let Some(exp_items) = expected_items {
                        if let Some(exp) = exp_items.get(idx) {
                            self.check_expr_expected(item, exp)?
                        } else {
                            self.check_expr(item)?
                        }
                    } else {
                        self.check_expr(item)?
                    };
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
                let ty = self.check_block_expected(block, expected)?;
                Some(ExprResult { ty, borrow: None })
            }
            ExprKind::UnsafeBlock(block) => {
                self.unsafe_depth += 1;
                let ty = self.check_block_expected(block, expected);
                self.unsafe_depth = self.unsafe_depth.saturating_sub(1);
                ty.map(|ty| ExprResult { ty, borrow: None })
            }
            ExprKind::If {
                cond,
                then_block,
                else_block,
            } => {
                let cond_ty = self.check_expr(cond)?;
                if !type_eq(&cond_ty.ty, &Type::Builtin(BuiltinType::Bool)) {
                    self.diags
                        .push("if condition must be bool", Some(cond.span.clone()));
                }
                let base_env = self.env.clone();
                self.env = base_env.clone();
                let then_ty = self.check_block_expected(then_block, expected)?;
                let then_env = self.env.clone();
                self.env = base_env.clone();
                let else_ty = if let Some(block) = else_block {
                    self.check_block_expected(block, Some(&then_ty))?
                } else {
                    Type::Builtin(BuiltinType::Unit)
                };
                let else_env = self.env.clone();
                self.env = self.join_env(&then_env, &else_env, &expr.span);
                if !type_eq(&then_ty, &else_ty) {
                    self.diags
                        .push("if branches must have same type", Some(expr.span.clone()));
                }
                Some(ExprResult {
                    ty: then_ty,
                    borrow: None,
                })
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
                    if let Some(guard) = &arm.guard {
                        let guard_res = self.check_expr(guard)?;
                        if !type_eq(&guard_res.ty, &Type::Builtin(BuiltinType::Bool)) {
                            self.diags
                                .push("match guard must be bool", Some(guard.span.clone()));
                        }
                    }
                    let arm_expected = expected.or(arm_ty.as_ref());
                    let ty = match &arm.body {
                        BlockOrExpr::Block(block) => self.check_block_expected(block, arm_expected),
                        BlockOrExpr::Expr(expr) => match arm_expected {
                            Some(exp) => self.check_expr_expected(expr, exp).map(|r| r.ty),
                            None => self.check_expr(expr).map(|r| r.ty),
                        },
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
            ExprKind::Closure { params, body } => {
                let captures = self.closure_captures(params, body.as_ref());
                self.apply_closure_capture_policy(&captures, &expr.span, consume);
                let ty = match expected {
                    Some(Type::Closure { .. }) => expected.cloned().unwrap_or(Type::Interface),
                    Some(Type::FnPtr {
                        params,
                        ret,
                        is_variadic,
                    }) => {
                        if captures.is_empty() {
                            expected.cloned().unwrap_or(Type::Interface)
                        } else {
                            Type::Closure {
                                params: params.clone(),
                                ret: ret.clone(),
                                is_variadic: *is_variadic,
                            }
                        }
                    }
                    _ => Type::Interface,
                };
                Some(ExprResult { ty, borrow: None })
            }
            ExprKind::Call {
                callee,
                type_args,
                args,
            } => {
                if let ExprKind::Field {
                    base,
                    name: variant,
                } = &callee.kind
                {
                    if let ExprKind::Ident(pkg_name) = &base.kind {
                        let pkg_prefix = format!("{}.", pkg_name);
                        let has_pkg_namespace = !self.env.vars.contains_key(pkg_name)
                            && self.funcs.keys().any(|k| k.starts_with(&pkg_prefix));
                        let namespaced = format!("{}.{}", pkg_name, variant);
                        if let Some(sig) = self.funcs.get(&namespaced) {
                            self.expr_types
                                .insert(base.id, Type::Builtin(BuiltinType::Unit));
                            self.expr_types
                                .insert(callee.id, Self::function_ptr_type(sig));
                            if !type_args.is_empty() {
                                self.diag_type_args_call_misuse(&expr.span);
                            }
                            if sig.is_extern || sig.is_unsafe {
                                self.require_unsafe_operation(
                                    &expr.span,
                                    &format!("call to `{}`", namespaced),
                                );
                            }
                            if (!sig.is_variadic && sig.params.len() != args.len())
                                || (sig.is_variadic && args.len() < sig.params.len())
                            {
                                self.diags
                                    .push("argument count mismatch", Some(expr.span.clone()));
                            }
                            for (idx, arg) in args.iter().enumerate() {
                                if let Some(param_ty) = sig.params.get(idx) {
                                    let res = self.check_expr_expected(arg, param_ty)?;
                                    if view_arg_not_allowed(self.defs, &res.ty, param_ty) {
                                        self.diags.push(
                                            "view arguments can only be passed to intrinsics",
                                            Some(arg.span.clone()),
                                        );
                                    }
                                    if !type_eq(&res.ty, param_ty) {
                                        self.diags
                                            .push("argument type mismatch", Some(arg.span.clone()));
                                    }
                                } else {
                                    let res = self.check_expr(arg)?;
                                    if self.defs.contains_view(&res.ty) {
                                        self.diags.push(
                                            "view arguments are not allowed in variadic positions",
                                            Some(arg.span.clone()),
                                        );
                                    }
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
                        if has_pkg_namespace {
                            self.diags.push(
                                format!("unknown member `{}` in package `{}`", variant, pkg_name),
                                Some(callee.span.clone()),
                            );
                            return None;
                        }
                    }
                    if let ExprKind::Ident(enum_name) = &base.kind {
                        if enum_name == "Result" || enum_name == "result" {
                            if let Some(res) =
                                self.check_result_ctor(variant, type_args, args, &expr.span, None)
                            {
                                return self.record_expr(expr, res);
                            }
                            return None;
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
                                self.diags.push(
                                    "enum constructor arity mismatch",
                                    Some(expr.span.clone()),
                                );
                            }
                            for (idx, arg) in args.iter().enumerate() {
                                let param_ty =
                                    fields.get(idx).unwrap_or(&Type::Builtin(BuiltinType::Unit));
                                let res = self.check_expr_expected(arg, param_ty)?;
                                if view_arg_not_allowed(self.defs, &res.ty, param_ty) {
                                    self.diags.push(
                                        "view arguments can only be passed to intrinsics",
                                        Some(arg.span.clone()),
                                    );
                                }
                                if !type_eq(&res.ty, param_ty) {
                                    self.diags
                                        .push("argument type mismatch", Some(arg.span.clone()));
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
                    let recv_res = self.check_expr_no_move(base)?;
                    let recv_ty = recv_res.ty.clone();
                    let method_candidates = self.method_candidates_named(variant);

                    if matches!(recv_ty, Type::Interface | Type::Closure { .. }) {
                        if let Some(trait_name) = self.trait_name_from_expr(base) {
                            let Some(required_sig) = self.resolve_trait_method_signature_for_call(
                                &trait_name,
                                variant,
                                type_args,
                                &callee.span,
                            ) else {
                                let mut d = Diagnostic::new(
                                    format!(
                                        "unknown method `{}` on trait object `{}`",
                                        variant, trait_name
                                    ),
                                    Some(callee.span.clone()),
                                )
                                .code(E_UNKNOWN_METHOD)
                                .label(
                                    callee.span.clone(),
                                    format!("unknown method `{}`", variant),
                                );
                                let candidates = self.trait_method_names(&trait_name);
                                if let Some(h) = suggest::did_you_mean(variant, candidates) {
                                    d = d.help(h);
                                }
                                self.diags.push_diag(d);
                                return None;
                            };
                            let mut dispatch = Vec::<(String, FunctionSig)>::new();
                            for (symbol, sig) in method_candidates {
                                let Some(param0) = sig.params.first() else {
                                    continue;
                                };
                                if !self.defs.interface_cast_supported(param0) {
                                    continue;
                                }
                                if !self.method_sig_matches_trait_requirement(&sig, &required_sig) {
                                    continue;
                                }
                                dispatch.push((symbol, sig));
                            }
                            if dispatch.is_empty() {
                                self.diags.push(
                                    format!(
                                        "trait object `{}` has no dispatch target for method `{}`",
                                        trait_name, variant
                                    ),
                                    Some(callee.span.clone()),
                                );
                                return None;
                            }
                            if dispatch
                                .iter()
                                .any(|(_, sig)| sig.is_extern || sig.is_unsafe)
                            {
                                self.require_unsafe_operation(
                                    &expr.span,
                                    &format!("call to `{}`", variant),
                                );
                            }
                            self.check_call_args_for_sig(&expr.span, args, &required_sig, 1)?;
                            return self.record_expr(
                                expr,
                                ExprResult {
                                    ty: required_sig.ret,
                                    borrow: None,
                                },
                            );
                        } else {
                            let mut dispatch = Vec::<(String, FunctionSig)>::new();
                            for (symbol, sig) in method_candidates {
                                let Some(param0) = sig.params.first() else {
                                    continue;
                                };
                                if self.defs.interface_cast_supported(param0) {
                                    dispatch.push((symbol, sig));
                                }
                            }
                            if dispatch.is_empty() {
                                if let Type::Closure {
                                    params,
                                    ret,
                                    is_variadic,
                                } = &recv_ty
                                    && variant.starts_with("__gost_closure_call_")
                                {
                                    if (!*is_variadic && params.len() != args.len())
                                        || (*is_variadic && args.len() < params.len())
                                    {
                                        self.diags.push(
                                            "argument count mismatch",
                                            Some(expr.span.clone()),
                                        );
                                    }
                                    for (idx, arg) in args.iter().enumerate() {
                                        if let Some(param_ty) = params.get(idx) {
                                            let res = self.check_expr_expected(arg, param_ty)?;
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
                                        } else {
                                            let res = self.check_expr(arg)?;
                                            if self.defs.contains_view(&res.ty) {
                                                self.diags.push(
                                                        "view arguments are not allowed in variadic positions",
                                                        Some(arg.span.clone()),
                                                    );
                                            }
                                        }
                                    }
                                    return self.record_expr(
                                        expr,
                                        ExprResult {
                                            ty: *ret.clone(),
                                            borrow: None,
                                        },
                                    );
                                }
                                self.diag_unknown_method(
                                    callee.span.clone(),
                                    Some(recv_ty),
                                    variant,
                                );
                                return None;
                            }
                            dispatch.sort_by(|a, b| a.0.cmp(&b.0));
                            let canonical = dispatch[0].1.clone();
                            for (_, sig) in dispatch.iter().skip(1) {
                                if sig.params.len() != canonical.params.len()
                                    || sig.is_variadic != canonical.is_variadic
                                    || !type_eq(&sig.ret, &canonical.ret)
                                {
                                    self.diags.push(
                                        format!(
                                            "interface method `{}` has incompatible implementations",
                                            variant
                                        ),
                                        Some(callee.span.clone()),
                                    );
                                    return None;
                                }
                                for idx in 1..sig.params.len() {
                                    if !type_eq(&sig.params[idx], &canonical.params[idx]) {
                                        self.diags.push(
                                            format!(
                                                "interface method `{}` has incompatible implementations",
                                                variant
                                            ),
                                            Some(callee.span.clone()),
                                        );
                                        return None;
                                    }
                                }
                            }
                            if dispatch
                                .iter()
                                .any(|(_, sig)| sig.is_extern || sig.is_unsafe)
                            {
                                self.require_unsafe_operation(
                                    &expr.span,
                                    &format!("call to `{}`", variant),
                                );
                            }
                            self.check_call_args_for_sig(&expr.span, args, &canonical, 1)?;
                            return self.record_expr(
                                expr,
                                ExprResult {
                                    ty: canonical.ret,
                                    borrow: None,
                                },
                            );
                        }
                    }

                    let mut resolved = Vec::<(u8, String, FunctionSig)>::new();
                    let mut autoadj_fail: Option<AutoadjFail> = None;
                    for (symbol, sig) in method_candidates {
                        let Some(param0) = sig.params.first() else {
                            continue;
                        };
                        let Some(cost) = self.recv_adjust_cost(&recv_ty, param0) else {
                            continue;
                        };
                        let need_mut = matches!(param0, Type::MutRef(_));
                        if matches!(param0, Type::Ref(_) | Type::MutRef(_))
                            && !self.is_addressable_expr(base)
                        {
                            autoadj_fail.get_or_insert_with(|| AutoadjFail {
                                kind: AutoadjFailKind::NotAddressable { need_mut },
                                recv_span: base.span.clone(),
                                method: variant.clone(),
                                recv_base_name: self.borrow_base_ident(base),
                                recv_ty: recv_ty.clone(),
                                param0_ty: param0.clone(),
                            });
                            continue;
                        }
                        if matches!(param0, Type::MutRef(_)) && !self.can_borrow_mut(base) {
                            autoadj_fail.get_or_insert_with(|| AutoadjFail {
                                kind: AutoadjFailKind::NotMutable,
                                recv_span: base.span.clone(),
                                method: variant.clone(),
                                recv_base_name: self.borrow_base_ident(base),
                                recv_ty: recv_ty.clone(),
                                param0_ty: param0.clone(),
                            });
                            continue;
                        }
                        resolved.push((cost, symbol, sig));
                    }

                    if resolved.is_empty() {
                        if let Some(fail) = autoadj_fail {
                            self.diag_autoadj_fail(fail);
                            return None;
                        }
                        self.diag_unknown_method(callee.span.clone(), Some(recv_ty), variant);
                        return None;
                    }
                    resolved.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
                    let best_cost = resolved[0].0;
                    let mut best: Vec<(u8, String, FunctionSig)> = resolved
                        .into_iter()
                        .filter(|(cost, _, _)| *cost == best_cost)
                        .collect();
                    if best.len() > 1
                        && let Some(arg_types) = self.preview_call_arg_types(args)
                    {
                        let narrowed: Vec<(u8, String, FunctionSig)> = best
                            .iter()
                            .filter(|(_, _, sig)| {
                                self.call_args_match_sig_types(sig, 1, &arg_types)
                            })
                            .cloned()
                            .collect();
                        if !narrowed.is_empty() {
                            best = narrowed;
                        }
                    }
                    if best.len() > 1 {
                        self.diags.push(
                            format!("ambiguous method `{}` on `{}`", variant, recv_ty.pretty()),
                            Some(callee.span.clone()),
                        );
                        return None;
                    }
                    let (_, chosen_symbol, chosen_sig) = best.remove(0);
                    if chosen_sig.is_extern || chosen_sig.is_unsafe {
                        self.require_unsafe_operation(
                            &expr.span,
                            &format!("call to `{}`", logical_method_name(&chosen_symbol)),
                        );
                    }
                    self.check_call_args_for_sig(&expr.span, args, &chosen_sig, 1)?;
                    return self.record_expr(
                        expr,
                        ExprResult {
                            ty: chosen_sig.ret,
                            borrow: None,
                        },
                    );
                }
                if let ExprKind::Ident(callee_name) = &callee.kind {
                    let intrinsic_name = is_intrinsic_name(callee_name);
                    if let Some(res) =
                        self.check_intrinsic_call(callee_name, type_args, args, &expr.span)
                    {
                        return self.record_expr(expr, res);
                    }
                    if intrinsic_name {
                        return None;
                    }
                    if self.enforce_namespaced_only()
                        && let Some(alias) = self.namespaced_only_funcs.get(callee_name)
                    {
                        self.diags.push(
                            format!(
                                "`{}` is imported via `{}`; use `{}.{}(...)`",
                                callee_name, alias, alias, callee_name
                            ),
                            Some(callee.span.clone()),
                        );
                        return None;
                    }
                    if let Some(sig) = self.funcs.get(callee_name) {
                        if !type_args.is_empty() {
                            self.diag_type_args_call_misuse(&expr.span);
                        }
                        if sig.is_extern || sig.is_unsafe {
                            self.require_unsafe_operation(
                                &expr.span,
                                &format!("call to `{}`", callee_name),
                            );
                        }
                        if (!sig.is_variadic && sig.params.len() != args.len())
                            || (sig.is_variadic && args.len() < sig.params.len())
                        {
                            self.diags
                                .push("argument count mismatch", Some(expr.span.clone()));
                        }
                        for (idx, arg) in args.iter().enumerate() {
                            if let Some(param_ty) = sig.params.get(idx) {
                                let res = self.check_expr_expected(arg, param_ty)?;
                                if view_arg_not_allowed(self.defs, &res.ty, param_ty) {
                                    self.diags.push(
                                        "view arguments can only be passed to intrinsics",
                                        Some(arg.span.clone()),
                                    );
                                }
                                if !type_eq(&res.ty, param_ty) {
                                    self.diags
                                        .push("argument type mismatch", Some(arg.span.clone()));
                                }
                            } else {
                                let res = self.check_expr(arg)?;
                                if self.defs.contains_view(&res.ty) {
                                    self.diags.push(
                                        "view arguments are not allowed in variadic positions",
                                        Some(arg.span.clone()),
                                    );
                                }
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

                let callee_ty = self.check_expr_no_move(callee)?.ty;
                match callee_ty {
                    Type::FnPtr {
                        params,
                        ret,
                        is_variadic,
                    }
                    | Type::Closure {
                        params,
                        ret,
                        is_variadic,
                    } => {
                        if (!is_variadic && params.len() != args.len())
                            || (is_variadic && args.len() < params.len())
                        {
                            self.diags
                                .push("argument count mismatch", Some(expr.span.clone()));
                        }
                        for (idx, arg) in args.iter().enumerate() {
                            if let Some(param_ty) = params.get(idx) {
                                let res = self.check_expr_expected(arg, param_ty)?;
                                if view_arg_not_allowed(self.defs, &res.ty, param_ty) {
                                    self.diags.push(
                                        "view arguments can only be passed to intrinsics",
                                        Some(arg.span.clone()),
                                    );
                                }
                                if !type_eq(&res.ty, param_ty) {
                                    self.diags
                                        .push("argument type mismatch", Some(arg.span.clone()));
                                }
                            } else {
                                let res = self.check_expr(arg)?;
                                if self.defs.contains_view(&res.ty) {
                                    self.diags.push(
                                        "view arguments are not allowed in variadic positions",
                                        Some(arg.span.clone()),
                                    );
                                }
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
            ExprKind::Field {
                base,
                name: field_name,
            } => {
                if let ExprKind::Ident(pkg_name) = &base.kind {
                    let pkg_prefix = format!("{}.", pkg_name);
                    let has_pkg_namespace = !self.env.vars.contains_key(pkg_name)
                        && self.funcs.keys().any(|k| k.starts_with(&pkg_prefix));
                    let namespaced = format!("{}.{}", pkg_name, field_name);
                    if let Some(sig) = self.funcs.get(&namespaced) {
                        self.expr_types
                            .insert(base.id, Type::Builtin(BuiltinType::Unit));
                        return self.record_expr(
                            expr,
                            ExprResult {
                                ty: Self::function_ptr_type(sig),
                                borrow: None,
                            },
                        );
                    }
                    if has_pkg_namespace {
                        self.diags.push(
                            format!("unknown member `{}` in package `{}`", field_name, pkg_name),
                            Some(expr.span.clone()),
                        );
                        return None;
                    }
                }
                if let ExprKind::Ident(enum_name) = &base.kind
                    && !self.env.vars.contains_key(enum_name)
                    && let Some(TypeDefKind::Enum(def)) = self.defs.get(enum_name)
                {
                    let fields = match def.variants.iter().find(|(name, _)| name == field_name) {
                        Some((_, fields)) => fields,
                        None => {
                            let mut d =
                                Diagnostic::new("unknown enum variant", Some(expr.span.clone()))
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
                        self.diags
                            .push("enum variant requires arguments", Some(expr.span.clone()));
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
                let base_res = self.check_expr_no_move(base)?;
                if let Type::Named(type_name) = base_res.ty
                    && let Some(TypeDefKind::Struct(def)) = self.defs.get(&type_name)
                {
                    if let Some(field) = def.fields.iter().find(|field| field.name == *field_name) {
                        if !self.can_access_struct_field(&type_name, field) {
                            self.diag_private_field(&expr.span, &type_name, field_name);
                            return None;
                        }
                        return self.record_expr(
                            expr,
                            ExprResult {
                                ty: field.ty.clone(),
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
                        .map(|field| field.name.clone())
                        .collect::<Vec<_>>();
                    if let Some(h) = suggest::did_you_mean(field_name, candidates) {
                        d = d.help(h);
                    }
                    self.diags.push_diag(d);
                    return None;
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
                        if !self.defs.is_copy_type(&elem) {
                            self.diags.push(
                                "cannot index linear element by value; use & or &mut",
                                Some(expr.span.clone()),
                            );
                        }
                        Some(ExprResult {
                            ty: elem,
                            borrow: None,
                        })
                    }
                    None => {
                        self.diags
                            .push("indexing expects a slice", Some(expr.span.clone()));
                        None
                    }
                }
            }
            ExprKind::Unary { op, expr: inner } => {
                let inner_res = self.check_expr(inner)?;
                match op {
                    UnaryOp::Neg => {
                        if !inner_res.ty.is_numeric() {
                            self.diags
                                .push("unary `-` expects numeric operand", Some(expr.span.clone()));
                        }
                        Some(ExprResult {
                            ty: inner_res.ty,
                            borrow: None,
                        })
                    }
                    UnaryOp::Not => {
                        if !type_eq(&inner_res.ty, &Type::Builtin(BuiltinType::Bool)) {
                            self.diags
                                .push("unary `!` expects bool operand", Some(expr.span.clone()));
                        }
                        Some(ExprResult {
                            ty: Type::Builtin(BuiltinType::Bool),
                            borrow: None,
                        })
                    }
                    UnaryOp::BitNot => {
                        if !inner_res.ty.is_integer() {
                            self.diags
                                .push("unary `~` expects integer operand", Some(expr.span.clone()));
                        }
                        Some(ExprResult {
                            ty: inner_res.ty,
                            borrow: None,
                        })
                    }
                }
            }
            ExprKind::Cast { expr: inner, ty } => {
                let inner_res = self.check_expr(inner)?;
                let target_ty = resolve_type(ty, self.defs, self.diags)?;
                if let Some(trait_name) = self.trait_name_from_type_ast(ty) {
                    self.type_implements_trait_object(&inner_res.ty, &trait_name, &expr.span);
                }
                if !self.defs.can_explicit_cast(&inner_res.ty, &target_ty) {
                    if matches!((&inner_res.ty, &target_ty), (Type::Alias(_), Type::Own(_))) {
                        self.diags.push(DIAG_ALIAS_TO_OWN, Some(expr.span.clone()));
                    } else {
                        self.diags.push(
                            format!(
                                "cannot cast `{}` to `{}`",
                                inner_res.ty.pretty(),
                                target_ty.pretty()
                            ),
                            Some(expr.span.clone()),
                        );
                    }
                }
                Some(ExprResult {
                    ty: target_ty,
                    borrow: None,
                })
            }
            ExprKind::Binary { op, left, right } => {
                let left_res = self.check_expr(left)?;
                let right_res = self.check_expr(right)?;
                let promoted = promote_int_types(&left_res.ty, &right_res.ty);
                let result_ty = match op {
                    BinaryOp::And | BinaryOp::Or => {
                        if !type_eq(&left_res.ty, &Type::Builtin(BuiltinType::Bool))
                            || !type_eq(&right_res.ty, &Type::Builtin(BuiltinType::Bool))
                        {
                            self.diags.push(
                                "logical operators expect bool operands",
                                Some(expr.span.clone()),
                            );
                        }
                        Type::Builtin(BuiltinType::Bool)
                    }
                    BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => {
                        if !left_res.ty.is_integer() || !right_res.ty.is_integer() {
                            self.diags.push(
                                "bitwise operators expect integer operands",
                                Some(expr.span.clone()),
                            );
                        } else if !type_eq(&left_res.ty, &right_res.ty) && promoted.is_none() {
                            self.diags
                                .push("binary operand type mismatch", Some(expr.span.clone()));
                        }
                        promoted.unwrap_or_else(|| left_res.ty.clone())
                    }
                    BinaryOp::Shl | BinaryOp::Shr => {
                        if !left_res.ty.is_integer() || !right_res.ty.is_integer() {
                            self.diags.push(
                                "shift operators expect integer operands",
                                Some(expr.span.clone()),
                            );
                        }
                        left_res.ty.clone()
                    }
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Rem => {
                        if matches!(op, BinaryOp::Add)
                            && type_eq(&left_res.ty, &Type::Builtin(BuiltinType::String))
                            && type_eq(&right_res.ty, &Type::Builtin(BuiltinType::String))
                        {
                            return self.record_expr(
                                expr,
                                ExprResult {
                                    ty: Type::Builtin(BuiltinType::String),
                                    borrow: None,
                                },
                            );
                        }
                        let both_int = left_res.ty.is_integer() && right_res.ty.is_integer();
                        let both_float = left_res.ty.is_float() && right_res.ty.is_float();
                        if both_int {
                            promoted.unwrap_or_else(|| left_res.ty.clone())
                        } else if both_float && type_eq(&left_res.ty, &right_res.ty) {
                            left_res.ty.clone()
                        } else {
                            self.diags.push(
                                "arithmetic operators expect matching numeric operands",
                                Some(expr.span.clone()),
                            );
                            left_res.ty.clone()
                        }
                    }
                    BinaryOp::Eq
                    | BinaryOp::NotEq
                    | BinaryOp::Lt
                    | BinaryOp::Lte
                    | BinaryOp::Gt
                    | BinaryOp::Gte => {
                        let comparable = type_eq(&left_res.ty, &right_res.ty)
                            || promoted.is_some()
                            || (left_res.ty.is_float()
                                && right_res.ty.is_float()
                                && type_eq(&left_res.ty, &right_res.ty));
                        if !comparable {
                            self.diags
                                .push("binary operand type mismatch", Some(expr.span.clone()));
                        }
                        Type::Builtin(BuiltinType::Bool)
                    }
                };
                Some(ExprResult {
                    ty: result_ty,
                    borrow: None,
                })
            }
            ExprKind::Borrow {
                is_mut,
                expr: inner,
            } => {
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
                        borrow: Some(BorrowInfo {
                            base,
                            is_mut: *is_mut,
                        }),
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
                        if !self.defs.is_copy_type(&inner) {
                            self.diags.push(
                                "cannot deref linear values by value",
                                Some(expr.span.clone()),
                            );
                        }
                        Some(ExprResult {
                            ty: *inner,
                            borrow: None,
                        })
                    }
                    _ => {
                        self.diags
                            .push("deref expects ref type", Some(expr.span.clone()));
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
                            self.diags
                                .push("second value must be error for ?", Some(expr.span.clone()));
                        }
                        Some(ExprResult {
                            ty: items[0].clone(),
                            borrow: None,
                        })
                    }
                    _ => {
                        self.diags
                            .push("`?` expects error or (T, error)", Some(expr.span.clone()));
                        None
                    }
                }
            }
            ExprKind::Send { chan, value } => {
                let chan_res = self.check_expr_no_move(chan)?;
                match chan_res.ty {
                    Type::Chan(inner) => {
                        let value_res = self.check_expr_expected(value, &inner)?;
                        if self.defs.contains_view(&value_res.ty) {
                            self.diags.push(DIAG_VIEW_ESCAPE, Some(expr.span.clone()));
                        }
                        if !type_eq(&value_res.ty, &inner) {
                            self.diags
                                .push("send type mismatch", Some(expr.span.clone()));
                        }
                        Some(ExprResult {
                            ty: Type::Builtin(BuiltinType::Unit),
                            borrow: None,
                        })
                    }
                    _ => {
                        self.diags
                            .push("send expects chan[T]", Some(expr.span.clone()));
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
                        self.diags
                            .push("recv expects chan[T]", Some(expr.span.clone()));
                        None
                    }
                }
            }
            ExprKind::Close { chan } => {
                let chan_res = self.check_expr_no_move(chan)?;
                match chan_res.ty {
                    Type::Chan(_) => Some(ExprResult {
                        ty: Type::Builtin(BuiltinType::Error),
                        borrow: None,
                    }),
                    _ => {
                        self.diags
                            .push("close expects chan[T]", Some(expr.span.clone()));
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
        for (name, prev) in locals.into_iter().rev() {
            if let Some(info) = self.env.vars.remove(&name)
                && let Some(base) = info.view_of
                && let Some(base_info) = self.env.vars.get_mut(&base)
            {
                if info.view_is_mut {
                    base_info.borrowed_mut = base_info.borrowed_mut.saturating_sub(1);
                } else {
                    base_info.borrowed_shared = base_info.borrowed_shared.saturating_sub(1);
                }
            }
            if let Some(prev_info) = prev {
                self.env.vars.insert(name, prev_info);
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
                    self.diags
                        .push("borrow state mismatch across branches", Some(span.clone()));
                    left_info.borrowed_mut = left_info.borrowed_mut.max(right_info.borrowed_mut);
                    left_info.borrowed_shared =
                        left_info.borrowed_shared.max(right_info.borrowed_shared);
                }
            }
        }
        joined
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

fn collect_closure_captures(
    params: &[ClosureParam],
    body: &BlockOrExpr,
    enclosing: &HashSet<String>,
) -> Vec<String> {
    let mut bound = HashSet::new();
    for param in params {
        bound.insert(param.name.clone());
    }
    let mut captures = HashSet::new();
    collect_captured_in_block_or_expr(body, &mut bound, enclosing, &mut captures);
    let mut out: Vec<String> = captures.into_iter().collect();
    out.sort();
    out
}

fn collect_captured_in_block_or_expr(
    body: &BlockOrExpr,
    bound: &mut HashSet<String>,
    enclosing: &HashSet<String>,
    captures: &mut HashSet<String>,
) {
    match body {
        BlockOrExpr::Block(block) => collect_captured_in_block(block, bound, enclosing, captures),
        BlockOrExpr::Expr(expr) => collect_captured_in_expr(expr, bound, enclosing, captures),
    }
}

fn collect_captured_in_block(
    block: &Block,
    bound: &mut HashSet<String>,
    enclosing: &HashSet<String>,
    captures: &mut HashSet<String>,
) {
    for stmt in &block.stmts {
        collect_captured_in_stmt(stmt, bound, enclosing, captures);
    }
    if let Some(tail) = &block.tail {
        collect_captured_in_expr(tail, bound, enclosing, captures);
    }
}

fn collect_captured_in_stmt(
    stmt: &Stmt,
    bound: &mut HashSet<String>,
    enclosing: &HashSet<String>,
    captures: &mut HashSet<String>,
) {
    match stmt {
        Stmt::Let { name, init, .. } | Stmt::Const { name, init, .. } => {
            collect_captured_in_expr(init, bound, enclosing, captures);
            bound.insert(name.clone());
        }
        Stmt::Assign { target, value, .. } => {
            collect_captured_in_expr(target, bound, enclosing, captures);
            collect_captured_in_expr(value, bound, enclosing, captures);
        }
        Stmt::Expr { expr, .. } | Stmt::Go { expr, .. } | Stmt::Defer { expr, .. } => {
            collect_captured_in_expr(expr, bound, enclosing, captures);
        }
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                collect_captured_in_expr(expr, bound, enclosing, captures);
            }
        }
        Stmt::While { cond, body, .. } => {
            collect_captured_in_expr(cond, bound, enclosing, captures);
            let mut inner = bound.clone();
            collect_captured_in_block(body, &mut inner, enclosing, captures);
        }
        Stmt::Loop { body, .. } => {
            let mut inner = bound.clone();
            collect_captured_in_block(body, &mut inner, enclosing, captures);
        }
        Stmt::ForIn {
            name,
            index,
            iter,
            body,
            ..
        } => {
            collect_captured_in_expr(iter, bound, enclosing, captures);
            let mut inner = bound.clone();
            inner.insert(name.clone());
            if let Some(index) = index {
                inner.insert(index.clone());
            }
            collect_captured_in_block(body, &mut inner, enclosing, captures);
        }
        Stmt::ForRange {
            name,
            index,
            start,
            end,
            body,
            ..
        } => {
            collect_captured_in_expr(start, bound, enclosing, captures);
            collect_captured_in_expr(end, bound, enclosing, captures);
            let mut inner = bound.clone();
            inner.insert(name.clone());
            if let Some(index) = index {
                inner.insert(index.clone());
            }
            collect_captured_in_block(body, &mut inner, enclosing, captures);
        }
        Stmt::Select { arms, .. } => {
            for arm in arms {
                let mut arm_bound = bound.clone();
                match &arm.kind {
                    SelectArmKind::Send { chan, value } => {
                        collect_captured_in_expr(chan, bound, enclosing, captures);
                        collect_captured_in_expr(value, bound, enclosing, captures);
                    }
                    SelectArmKind::Recv { chan, bind } => {
                        collect_captured_in_expr(chan, bound, enclosing, captures);
                        if let Some((value_name, ok_name)) = bind {
                            if value_name != "_" {
                                arm_bound.insert(value_name.clone());
                            }
                            if ok_name != "_" {
                                arm_bound.insert(ok_name.clone());
                            }
                        }
                    }
                    SelectArmKind::After { ms } => {
                        collect_captured_in_expr(ms, bound, enclosing, captures);
                    }
                    SelectArmKind::Default => {}
                }
                collect_captured_in_block_or_expr(&arm.body, &mut arm_bound, enclosing, captures);
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
}

fn collect_pattern_bindings_for_capture(pattern: &Pattern, out: &mut HashSet<String>) {
    match pattern {
        Pattern::Ident(name) => {
            if name != "_" {
                out.insert(name.clone());
            }
        }
        Pattern::Or(patterns) => {
            for pattern in patterns {
                collect_pattern_bindings_for_capture(pattern, out);
            }
        }
        Pattern::Variant { binds, .. } => {
            for bind in binds {
                if bind != "_" {
                    out.insert(bind.clone());
                }
            }
        }
        Pattern::Wildcard | Pattern::Bool(_) | Pattern::Int(_) => {}
    }
}

fn collect_captured_in_expr(
    expr: &Expr,
    bound: &mut HashSet<String>,
    enclosing: &HashSet<String>,
    captures: &mut HashSet<String>,
) {
    match &expr.kind {
        ExprKind::Ident(name) => {
            if enclosing.contains(name) && !bound.contains(name) {
                captures.insert(name.clone());
            }
        }
        ExprKind::StructLit { fields, .. } => {
            for (_, field_expr) in fields {
                collect_captured_in_expr(field_expr, bound, enclosing, captures);
            }
        }
        ExprKind::ArrayLit(items) | ExprKind::Tuple(items) => {
            for item in items {
                collect_captured_in_expr(item, bound, enclosing, captures);
            }
        }
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
            let mut inner = bound.clone();
            collect_captured_in_block(block, &mut inner, enclosing, captures);
        }
        ExprKind::If {
            cond,
            then_block,
            else_block,
        } => {
            collect_captured_in_expr(cond, bound, enclosing, captures);
            let mut then_bound = bound.clone();
            collect_captured_in_block(then_block, &mut then_bound, enclosing, captures);
            if let Some(else_block) = else_block {
                let mut else_bound = bound.clone();
                collect_captured_in_block(else_block, &mut else_bound, enclosing, captures);
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            collect_captured_in_expr(scrutinee, bound, enclosing, captures);
            for arm in arms {
                let mut arm_bound = bound.clone();
                collect_pattern_bindings_for_capture(&arm.pattern, &mut arm_bound);
                if let Some(guard) = &arm.guard {
                    collect_captured_in_expr(guard, &mut arm_bound, enclosing, captures);
                }
                collect_captured_in_block_or_expr(&arm.body, &mut arm_bound, enclosing, captures);
            }
        }
        ExprKind::Closure { params, body } => {
            let mut nested = bound.clone();
            for param in params {
                nested.insert(param.name.clone());
            }
            collect_captured_in_block_or_expr(body, &mut nested, enclosing, captures);
        }
        ExprKind::Call { callee, args, .. } => {
            collect_captured_in_expr(callee, bound, enclosing, captures);
            for arg in args {
                collect_captured_in_expr(arg, bound, enclosing, captures);
            }
        }
        ExprKind::Field { base, .. }
        | ExprKind::Unary { expr: base, .. }
        | ExprKind::Cast { expr: base, .. }
        | ExprKind::Borrow { expr: base, .. }
        | ExprKind::Deref { expr: base }
        | ExprKind::Try { expr: base }
        | ExprKind::Recv { chan: base }
        | ExprKind::Close { chan: base }
        | ExprKind::After { ms: base } => {
            collect_captured_in_expr(base, bound, enclosing, captures);
        }
        ExprKind::Index { base, index } => {
            collect_captured_in_expr(base, bound, enclosing, captures);
            collect_captured_in_expr(index, bound, enclosing, captures);
        }
        ExprKind::Binary { left, right, .. } => {
            collect_captured_in_expr(left, bound, enclosing, captures);
            collect_captured_in_expr(right, bound, enclosing, captures);
        }
        ExprKind::Send { chan, value } => {
            collect_captured_in_expr(chan, bound, enclosing, captures);
            collect_captured_in_expr(value, bound, enclosing, captures);
        }
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil => {}
    }
}
