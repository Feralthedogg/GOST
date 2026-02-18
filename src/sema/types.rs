use std::collections::{HashMap, HashSet};
use std::fmt;
use crate::frontend::ast::Visibility;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    Bool,
    I8,
    I16,
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
    F32,
    F64,
    Char,
    Unit,
    String,
    Error,
    Bytes,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Builtin(BuiltinType),
    Named(String),
    FnPtr {
        params: Vec<Type>,
        ret: Box<Type>,
        is_variadic: bool,
    },
    Ref(Box<Type>),
    MutRef(Box<Type>),
    Own(Box<Type>),
    Alias(Box<Type>),
    Slice(Box<Type>),
    Array(Box<Type>, usize),
    Map(Box<Type>, Box<Type>),
    Result(Box<Type>, Box<Type>),
    Iter(Box<Type>),
    Chan(Box<Type>),
    Shared(Box<Type>),
    Interface,
    Tuple(Vec<Type>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RefKind {
    None,
    Shared,
    Mut,
}

impl Type {
    /// &T / &mut T 한 번만 벗김
    #[inline]
    pub fn deref_once(&self) -> Option<&Type> {
        match self {
            Type::Ref(inner) | Type::MutRef(inner) => Some(inner.as_ref()),
            _ => None,
        }
    }

    /// 모든 ref를 벗겨서 (base, outer_ref_kind, depth)를 반환
    #[inline]
    pub fn peel_refs(&self) -> (&Type, RefKind, usize) {
        let mut cur = self;
        let mut outer = RefKind::None;
        let mut depth = 0;
        loop {
            match cur {
                Type::Ref(inner) => {
                    if depth == 0 {
                        outer = RefKind::Shared;
                    }
                    cur = inner.as_ref();
                    depth += 1;
                }
                Type::MutRef(inner) => {
                    if depth == 0 {
                        outer = RefKind::Mut;
                    }
                    cur = inner.as_ref();
                    depth += 1;
                }
                _ => break,
            }
        }
        (cur, outer, depth)
    }

    #[inline]
    pub fn is_ref(&self) -> bool {
        matches!(self, Type::Ref(_) | Type::MutRef(_))
    }

    #[inline]
    pub fn pretty(&self) -> TypePretty<'_> {
        TypePretty(self)
    }

    #[inline]
    pub fn is_float(&self) -> bool {
        matches!(self, Type::Builtin(BuiltinType::F32 | BuiltinType::F64))
    }

    #[inline]
    pub fn int_info(&self) -> Option<(bool, u8)> {
        match self {
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
            _ => None,
        }
    }

    #[inline]
    pub fn is_integer(&self) -> bool {
        self.int_info().is_some()
    }

    #[inline]
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    #[inline]
    pub fn can_be_map_key(&self) -> bool {
        self.map_runtime_key_kind().is_some()
    }

    #[inline]
    pub fn map_runtime_key_kind(&self) -> Option<i32> {
        if matches!(self, Type::Builtin(BuiltinType::Error)) {
            return Some(1);
        }
        if let Some((signed, _)) = self.int_info() {
            return Some(if signed { 1 } else { 2 });
        }
        if matches!(self, Type::Builtin(BuiltinType::F32 | BuiltinType::F64)) {
            return Some(4);
        }
        if matches!(self, Type::Builtin(BuiltinType::Bool | BuiltinType::Char)) {
            return Some(2);
        }
        if matches!(self, Type::FnPtr { .. }) {
            return Some(4);
        }
        if matches!(self, Type::Alias(_) | Type::Shared(_) | Type::Chan(_)) {
            return Some(4);
        }
        if matches!(self, Type::Builtin(BuiltinType::String)) {
            return Some(3);
        }
        None
    }
}

pub struct TypePretty<'a>(pub &'a Type);

impl fmt::Display for TypePretty<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_type_pretty(self.0, f)
    }
}

fn fmt_type_pretty(ty: &Type, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match ty {
        Type::Ref(inner) => {
            write!(f, "&")?;
            fmt_type_pretty(inner, f)
        }
        Type::MutRef(inner) => {
            write!(f, "&mut ")?;
            fmt_type_pretty(inner, f)
        }
        Type::Own(inner) => {
            write!(f, "own[")?;
            fmt_type_pretty(inner, f)?;
            write!(f, "]")
        }
        Type::Alias(inner) => {
            write!(f, "alias[")?;
            fmt_type_pretty(inner, f)?;
            write!(f, "]")
        }
        Type::FnPtr {
            params,
            ret,
            is_variadic,
        } => {
            write!(f, "fn(")?;
            for (i, p) in params.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                fmt_type_pretty(p, f)?;
            }
            if *is_variadic {
                if !params.is_empty() {
                    write!(f, ", ")?;
                }
                write!(f, "...")?;
            }
            write!(f, ") -> ")?;
            fmt_type_pretty(ret, f)
        }
        Type::Slice(inner) => {
            write!(f, "[]")?;
            fmt_type_pretty(inner, f)
        }
        Type::Array(inner, len) => {
            write!(f, "[{}]", len)?;
            fmt_type_pretty(inner, f)
        }
        Type::Chan(inner) => {
            write!(f, "chan[")?;
            fmt_type_pretty(inner, f)?;
            write!(f, "]")
        }
        Type::Shared(inner) => {
            write!(f, "shared[")?;
            fmt_type_pretty(inner, f)?;
            write!(f, "]")
        }
        Type::Iter(inner) => {
            write!(f, "iter[")?;
            fmt_type_pretty(inner, f)?;
            write!(f, "]")
        }
        Type::Map(k, v) => {
            write!(f, "map[")?;
            fmt_type_pretty(k, f)?;
            write!(f, "]")?;
            fmt_type_pretty(v, f)
        }
        Type::Result(ok, err) => {
            write!(f, "result[")?;
            fmt_type_pretty(ok, f)?;
            write!(f, ", ")?;
            fmt_type_pretty(err, f)?;
            write!(f, "]")
        }
        Type::Tuple(items) => {
            write!(f, "(")?;
            for (i, it) in items.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                fmt_type_pretty(it, f)?;
            }
            write!(f, ")")
        }
        Type::Interface => write!(f, "interface"),
        Type::Named(name) => write!(f, "{}", name),
        Type::Builtin(b) => match b {
            BuiltinType::Bool => write!(f, "bool"),
            BuiltinType::I8 => write!(f, "i8"),
            BuiltinType::I16 => write!(f, "i16"),
            BuiltinType::I32 => write!(f, "i32"),
            BuiltinType::I64 => write!(f, "i64"),
            BuiltinType::Isize => write!(f, "isize"),
            BuiltinType::U8 => write!(f, "u8"),
            BuiltinType::U16 => write!(f, "u16"),
            BuiltinType::U32 => write!(f, "u32"),
            BuiltinType::U64 => write!(f, "u64"),
            BuiltinType::Usize => write!(f, "usize"),
            BuiltinType::F32 => write!(f, "f32"),
            BuiltinType::F64 => write!(f, "f64"),
            BuiltinType::Char => write!(f, "char"),
            BuiltinType::Unit => write!(f, "unit"),
            BuiltinType::String => write!(f, "string"),
            BuiltinType::Error => write!(f, "error"),
            BuiltinType::Bytes => write!(f, "bytes"),
        },
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeClass {
    Copy,
    Linear,
    View,
}

#[derive(Clone, Debug)]
pub enum TypeDefKind {
    Struct(StructDef),
    Enum(EnumDef),
}

#[derive(Clone, Debug)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
    pub vis: Visibility,
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub fields: Vec<StructField>,
    pub is_copy: bool,
    pub layout: LayoutInfo,
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub variants: Vec<(String, Vec<Type>)>,
    pub is_copy: bool,
    pub layout: LayoutInfo,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReprInt {
    I8,
    I16,
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
}

impl ReprInt {
    pub fn llvm_int_type(self) -> &'static str {
        match self {
            Self::I8 | Self::U8 => "i8",
            Self::I16 | Self::U16 => "i16",
            Self::I32 | Self::U32 => "i32",
            Self::I64 | Self::Isize | Self::U64 | Self::Usize => "i64",
        }
    }

    pub fn is_signed(self) -> bool {
        matches!(
            self,
            Self::I8 | Self::I16 | Self::I32 | Self::I64 | Self::Isize
        )
    }
}

#[derive(Clone, Debug, Default)]
pub struct LayoutInfo {
    pub repr_c: bool,
    pub repr_transparent: bool,
    pub repr_int: Option<ReprInt>,
    pub repr_other: Option<String>,
    pub pack: Option<u32>,
    pub bitfield: bool,
}

#[derive(Clone, Debug, Default)]
pub struct TypeDefs {
    defs: HashMap<String, TypeDefKind>,
    aliases: HashMap<String, Type>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MapKeySpec {
    pub kind: i32,
    pub runtime_ty: Type,
    pub needs_callbacks: bool,
}

impl TypeDefs {
    pub fn insert(&mut self, name: String, def: TypeDefKind) {
        self.defs.insert(name, def);
    }

    pub fn get(&self, name: &str) -> Option<&TypeDefKind> {
        self.defs.get(name)
    }

    pub fn insert_alias(&mut self, name: String, ty: Type) {
        self.aliases.insert(name, ty);
    }

    pub fn get_alias(&self, name: &str) -> Option<&Type> {
        self.aliases.get(name)
    }

    pub fn all_names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.defs.keys().cloned().collect();
        names.extend(self.aliases.keys().cloned());
        names.sort();
        names.dedup();
        names
    }

    pub fn names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.defs.keys().cloned().collect();
        names.sort();
        names
    }

    pub fn map_runtime_key_kind(&self, ty: &Type) -> Option<i32> {
        self.map_key_spec(ty).map(|spec| spec.kind)
    }

    fn map_runtime_key_kind_inner(
        &self,
        ty: &Type,
        visiting: &mut HashSet<String>,
    ) -> Option<i32> {
        if let Some(kind) = ty.map_runtime_key_kind() {
            return Some(kind);
        }
        match ty {
            Type::Array(_, _) | Type::Tuple(_) => {
                if self.is_bytewise_map_key_inner(ty, visiting) {
                    Some(4)
                } else {
                    None
                }
            }
            Type::Result(ok, err) => {
                if self.is_bytewise_map_key_inner(ok, visiting)
                    && self.is_bytewise_map_key_inner(err, visiting)
                {
                    Some(4)
                } else {
                    None
                }
            }
            Type::Named(name) => {
                if !visiting.insert(name.clone()) {
                    return None;
                }
                let out = if let Some(alias) = self.get_alias(name) {
                    self.map_runtime_key_kind_inner(alias, visiting)
                } else {
                    match self.get(name) {
                        Some(TypeDefKind::Enum(def)) => {
                            let fieldless = def.variants.iter().all(|(_, fields)| fields.is_empty());
                            if fieldless {
                                if let Some(repr) = def.layout.repr_int {
                                    Some(if repr.is_signed() { 1 } else { 2 })
                                } else {
                                    Some(2)
                                }
                            } else if def.variants.iter().all(|(_, fields)| {
                                fields
                                    .iter()
                                    .all(|field| self.is_bytewise_map_key_inner(field, visiting))
                            }) {
                                Some(4)
                            } else {
                                None
                            }
                        }
                        Some(TypeDefKind::Struct(def))
                            if def
                                .fields
                                .iter()
                                .all(|field| self.is_bytewise_map_key_inner(&field.ty, visiting)) =>
                        {
                            Some(4)
                        }
                        _ => None,
                    }
                };
                visiting.remove(name);
                out
            }
            _ => None,
        }
    }

    fn is_bytewise_map_key_inner(&self, ty: &Type, visiting: &mut HashSet<String>) -> bool {
        if matches!(
            ty,
            Type::Builtin(
                BuiltinType::Bool
                    | BuiltinType::Char
                    | BuiltinType::F32
                    | BuiltinType::F64
                    | BuiltinType::String
                    | BuiltinType::Error
            )
        ) {
            return true;
        }
        if matches!(ty, Type::Alias(_) | Type::Shared(_) | Type::Chan(_)) {
            return true;
        }
        if matches!(ty, Type::FnPtr { .. }) {
            return true;
        }
        if ty.int_info().is_some() {
            return true;
        }
        match ty {
            Type::Array(inner, _) => self.is_bytewise_map_key_inner(inner, visiting),
            Type::Tuple(items) => items
                .iter()
                .all(|item| self.is_bytewise_map_key_inner(item, visiting)),
            Type::Result(ok, err) => {
                self.is_bytewise_map_key_inner(ok, visiting)
                    && self.is_bytewise_map_key_inner(err, visiting)
            }
            Type::Named(name) => {
                if !visiting.insert(name.clone()) {
                    return false;
                }
                let out = if let Some(alias) = self.get_alias(name) {
                    self.is_bytewise_map_key_inner(alias, visiting)
                } else {
                    match self.get(name) {
                        Some(TypeDefKind::Struct(def)) => def
                            .fields
                            .iter()
                            .all(|field| self.is_bytewise_map_key_inner(&field.ty, visiting)),
                        Some(TypeDefKind::Enum(def)) => {
                            def.variants.iter().all(|(_, fields)| {
                                fields
                                    .iter()
                                    .all(|field| self.is_bytewise_map_key_inner(field, visiting))
                            })
                        }
                        _ => false,
                    }
                };
                visiting.remove(name);
                out
            }
            _ => false,
        }
    }

    #[inline]
    pub fn can_be_map_key(&self, ty: &Type) -> bool {
        self.map_key_spec(ty).is_some()
    }

    #[inline]
    pub fn map_runtime_key_type(&self, ty: &Type) -> Option<Type> {
        self.map_key_spec(ty).map(|spec| spec.runtime_ty)
    }

    pub fn map_key_spec(&self, ty: &Type) -> Option<MapKeySpec> {
        let mut visiting = HashSet::new();
        let kind = self.map_runtime_key_kind_inner(ty, &mut visiting)?;
        let runtime_ty = match kind {
            1 => Type::Builtin(BuiltinType::I64),
            2 => Type::Builtin(BuiltinType::U64),
            3 => Type::Builtin(BuiltinType::String),
            4 => self.resolve_map_runtime_key_passthrough_type(ty)?,
            _ => return None,
        };
        Some(MapKeySpec {
            kind,
            runtime_ty,
            needs_callbacks: kind == 4,
        })
    }

    fn resolve_map_runtime_key_passthrough_type(&self, ty: &Type) -> Option<Type> {
        fn resolve_inner(
            defs: &TypeDefs,
            ty: &Type,
            visiting: &mut HashSet<String>,
        ) -> Option<Type> {
            match ty {
                Type::Named(name) => {
                    if !visiting.insert(name.clone()) {
                        return None;
                    }
                    let out = if let Some(alias) = defs.get_alias(name) {
                        resolve_inner(defs, alias, visiting)
                    } else {
                        Some(Type::Named(name.clone()))
                    };
                    visiting.remove(name);
                    out
                }
                _ => Some(ty.clone()),
            }
        }
        let mut visiting = HashSet::new();
        resolve_inner(self, ty, &mut visiting)
    }

    pub fn classify(&self, ty: &Type) -> Option<TypeClass> {
        match ty {
            Type::Ref(_) | Type::MutRef(_) => Some(TypeClass::View),
            Type::Builtin(b) => match b {
                BuiltinType::Bool
                | BuiltinType::I8
                | BuiltinType::I16
                | BuiltinType::I32
                | BuiltinType::I64
                | BuiltinType::Isize
                | BuiltinType::U8
                | BuiltinType::U16
                | BuiltinType::U32
                | BuiltinType::U64
                | BuiltinType::Usize
                | BuiltinType::F32
                | BuiltinType::F64
                | BuiltinType::Char
                | BuiltinType::Unit
                | BuiltinType::String
                | BuiltinType::Error => Some(TypeClass::Copy),
                BuiltinType::Bytes => Some(TypeClass::Linear),
            },
            Type::FnPtr { .. } => Some(TypeClass::Copy),
            Type::Own(_) => Some(TypeClass::Linear),
            Type::Alias(_) => Some(TypeClass::Copy),
            Type::Shared(_) | Type::Interface => Some(TypeClass::Copy),
            // Channels are reference types; treat as Copy like Go.
            Type::Slice(_) | Type::Map(_, _) => Some(TypeClass::Linear),
            Type::Array(inner, _) => self.classify(inner),
            Type::Chan(_) => Some(TypeClass::Copy),
            Type::Tuple(items) => {
                let mut class = TypeClass::Copy;
                for item in items {
                    match self.classify(item)? {
                        TypeClass::View => return Some(TypeClass::View),
                        TypeClass::Linear => class = TypeClass::Linear,
                        TypeClass::Copy => {}
                    }
                }
                Some(class)
            }
            Type::Result(ok, err) => {
                let mut class = TypeClass::Copy;
                for item in [ok.as_ref(), err.as_ref()] {
                    match self.classify(item)? {
                        TypeClass::View => return Some(TypeClass::View),
                        TypeClass::Linear => class = TypeClass::Linear,
                        TypeClass::Copy => {}
                    }
                }
                Some(class)
            }
            Type::Iter(_) => Some(TypeClass::Copy),
            Type::Named(name) => match self.defs.get(name) {
                Some(TypeDefKind::Struct(def)) => Some(if def.is_copy {
                    TypeClass::Copy
                } else {
                    TypeClass::Linear
                }),
                Some(TypeDefKind::Enum(def)) => Some(if def.is_copy {
                    TypeClass::Copy
                } else {
                    TypeClass::Linear
                }),
                None => None,
            },
        }
    }

    pub fn contains_view(&self, ty: &Type) -> bool {
        let mut visiting = HashSet::new();
        self.contains_view_inner(ty, &mut visiting)
    }

    fn contains_view_inner(&self, ty: &Type, visiting: &mut HashSet<String>) -> bool {
        match ty {
            Type::Ref(_) | Type::MutRef(_) => true,
            Type::Slice(inner)
            | Type::Array(inner, _)
            | Type::Chan(inner)
            | Type::Own(inner)
            | Type::Alias(inner)
            | Type::Shared(inner)
            | Type::Iter(inner) => self.contains_view_inner(inner, visiting),
            Type::Map(key, value) => {
                self.contains_view_inner(key, visiting) || self.contains_view_inner(value, visiting)
            }
            Type::Tuple(items) => items.iter().any(|t| self.contains_view_inner(t, visiting)),
            Type::Result(ok, err) => {
                self.contains_view_inner(ok, visiting) || self.contains_view_inner(err, visiting)
            }
            Type::Named(name) => {
                if let Some(alias) = self.get_alias(name) {
                    return self.contains_view_inner(alias, visiting);
                }
                if visiting.contains(name) {
                    return false;
                }
                visiting.insert(name.clone());
                let res = match self.get(name) {
                    Some(TypeDefKind::Struct(def)) => def
                        .fields
                        .iter()
                        .any(|field| self.contains_view_inner(&field.ty, visiting)),
                    Some(TypeDefKind::Enum(def)) => def.variants.iter().any(|(_, tys)| {
                        tys.iter().any(|ty| self.contains_view_inner(ty, visiting))
                    }),
                    None => false,
                };
                visiting.remove(name);
                res
            }
            _ => false,
        }
    }

    #[inline]
    pub fn is_copy_type(&self, ty: &Type) -> bool {
        matches!(self.classify(ty), Some(TypeClass::Copy))
    }

    pub fn interface_cast_supported(&self, ty: &Type) -> bool {
        let mut visiting = HashSet::new();
        self.interface_cast_supported_inner(ty, &mut visiting)
    }

    fn interface_cast_supported_inner(&self, ty: &Type, visiting: &mut HashSet<String>) -> bool {
        match ty {
            Type::Interface => true,
            Type::Builtin(
                BuiltinType::Bool
                    | BuiltinType::I8
                    | BuiltinType::I16
                    | BuiltinType::I32
                    | BuiltinType::I64
                    | BuiltinType::Isize
                    | BuiltinType::U8
                    | BuiltinType::U16
                    | BuiltinType::U32
                    | BuiltinType::U64
                    | BuiltinType::Usize
                    | BuiltinType::F32
                    | BuiltinType::F64
                    | BuiltinType::Char
                    | BuiltinType::Unit
                    | BuiltinType::String
                    | BuiltinType::Error,
            ) => true,
            Type::FnPtr { .. } => true,
            Type::Tuple(items) => items
                .iter()
                .all(|it| self.interface_cast_supported_inner(it, visiting)),
            Type::Result(ok, err) => {
                self.interface_cast_supported_inner(ok, visiting)
                    && self.interface_cast_supported_inner(err, visiting)
            }
            Type::Array(inner, _) => self.interface_cast_supported_inner(inner, visiting),
            Type::Named(name) => {
                if visiting.contains(name) {
                    return true;
                }
                visiting.insert(name.clone());
                let supported = if let Some(alias) = self.get_alias(name) {
                    self.interface_cast_supported_inner(alias, visiting)
                } else {
                    match self.get(name) {
                        Some(TypeDefKind::Struct(def)) => def
                            .fields
                            .iter()
                            .all(|field| self.interface_cast_supported_inner(&field.ty, visiting)),
                        Some(TypeDefKind::Enum(def)) => def.variants.iter().all(|(_, fields)| {
                            fields
                                .iter()
                                .all(|field_ty| self.interface_cast_supported_inner(field_ty, visiting))
                        }),
                        None => false,
                    }
                };
                visiting.remove(name);
                supported
            }
            Type::Ref(_)
            | Type::MutRef(_)
            | Type::Slice(_)
            | Type::Map(_, _)
            | Type::Chan(_)
            | Type::Own(_)
            | Type::Alias(_)
            | Type::Shared(_)
            | Type::Iter(_)
            | Type::Builtin(BuiltinType::Bytes) => false,
        }
    }

    pub fn can_explicit_cast(&self, from: &Type, to: &Type) -> bool {
        if from == to {
            return true;
        }
        if matches!(to, Type::Interface) {
            return self.interface_cast_supported(from);
        }
        if matches!(from, Type::Interface) {
            return self.interface_cast_supported(to);
        }
        if from.is_integer() && to.is_integer() {
            return true;
        }
        if from.is_integer() && to.is_float() {
            return true;
        }
        if from.is_float() && to.is_integer() {
            return true;
        }
        if from.is_float() && to.is_float() {
            return true;
        }
        false
    }
}

#[inline]
pub fn type_eq(a: &Type, b: &Type) -> bool {
    a == b
}

#[inline]
pub fn contains_view(defs: &TypeDefs, ty: &Type) -> bool {
    defs.contains_view(ty)
}

#[inline]
pub fn is_copy_type(defs: &TypeDefs, ty: &Type) -> bool {
    defs.is_copy_type(ty)
}

#[inline]
pub fn is_map_key_type(ty: &Type) -> bool {
    ty.can_be_map_key()
}

#[inline]
pub fn is_float_type(ty: &Type) -> bool {
    ty.is_float()
}

#[inline]
pub fn int_info(ty: &Type) -> Option<(bool, u8)> {
    ty.int_info()
}

#[inline]
pub fn is_integer_type(ty: &Type) -> bool {
    ty.is_integer()
}

#[inline]
pub fn is_numeric_type(ty: &Type) -> bool {
    ty.is_numeric()
}

#[inline]
pub fn interface_cast_supported(defs: &TypeDefs, ty: &Type) -> bool {
    defs.interface_cast_supported(ty)
}

#[inline]
pub fn can_explicit_cast(defs: &TypeDefs, from: &Type, to: &Type) -> bool {
    defs.can_explicit_cast(from, to)
}

pub fn promote_int_types(a: &Type, b: &Type) -> Option<Type> {
    if a == b {
        return Some(a.clone());
    }
    let (a_signed, a_bits) = a.int_info()?;
    let (b_signed, b_bits) = b.int_info()?;
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

pub fn builtin_from_name(name: &str) -> Option<Type> {
    let b = match name {
        "bool" => BuiltinType::Bool,
        "i8" => BuiltinType::I8,
        "i16" => BuiltinType::I16,
        "i32" => BuiltinType::I32,
        "i64" => BuiltinType::I64,
        "isize" => BuiltinType::Isize,
        "u8" => BuiltinType::U8,
        "u16" => BuiltinType::U16,
        "u32" => BuiltinType::U32,
        "u64" => BuiltinType::U64,
        "usize" => BuiltinType::Usize,
        "f32" => BuiltinType::F32,
        "f64" => BuiltinType::F64,
        "char" => BuiltinType::Char,
        "unit" => BuiltinType::Unit,
        "string" => BuiltinType::String,
        "error" => BuiltinType::Error,
        "bytes" => BuiltinType::Bytes,
        _ => return None,
    };
    Some(Type::Builtin(b))
}

pub fn builtin_names() -> &'static [&'static str] {
    &[
        "bool", "i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64", "usize", "f32",
        "f64", "char", "unit", "string", "error", "bytes",
    ]
}
