use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    Bool,
    I32,
    I64,
    U32,
    U64,
    F32,
    F64,
    Char,
    Unit,
    String,
    Error,
    Bytes,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
    Slice(Box<Type>),
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
            BuiltinType::I32 => write!(f, "i32"),
            BuiltinType::I64 => write!(f, "i64"),
            BuiltinType::U32 => write!(f, "u32"),
            BuiltinType::U64 => write!(f, "u64"),
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
pub struct StructDef {
    pub fields: Vec<(String, Type)>,
    pub is_copy: bool,
    pub layout: LayoutInfo,
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub variants: Vec<(String, Vec<Type>)>,
    pub is_copy: bool,
    pub layout: LayoutInfo,
}

#[derive(Clone, Debug, Default)]
pub struct LayoutInfo {
    pub repr_c: bool,
    pub pack: Option<u32>,
    pub bitfield: bool,
}

#[derive(Clone, Debug, Default)]
pub struct TypeDefs {
    defs: HashMap<String, TypeDefKind>,
}

impl TypeDefs {
    pub fn insert(&mut self, name: String, def: TypeDefKind) {
        self.defs.insert(name, def);
    }

    pub fn get(&self, name: &str) -> Option<&TypeDefKind> {
        self.defs.get(name)
    }

    pub fn names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.defs.keys().cloned().collect();
        names.sort();
        names
    }

    pub fn classify(&self, ty: &Type) -> Option<TypeClass> {
        match ty {
            Type::Ref(_) | Type::MutRef(_) => Some(TypeClass::View),
            Type::Builtin(b) => match b {
                BuiltinType::Bool
                | BuiltinType::I32
                | BuiltinType::I64
                | BuiltinType::U32
                | BuiltinType::U64
                | BuiltinType::F32
                | BuiltinType::F64
                | BuiltinType::Char
                | BuiltinType::Unit
                | BuiltinType::String
                | BuiltinType::Error => Some(TypeClass::Copy),
                BuiltinType::Bytes => Some(TypeClass::Linear),
            },
            Type::FnPtr { .. } => Some(TypeClass::Copy),
            Type::Shared(_) | Type::Interface => Some(TypeClass::Copy),
            // Channels are reference types; treat as Copy like Go.
            Type::Slice(_) | Type::Map(_, _) => Some(TypeClass::Linear),
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
}

pub fn builtin_from_name(name: &str) -> Option<Type> {
    let b = match name {
        "bool" => BuiltinType::Bool,
        "i32" => BuiltinType::I32,
        "i64" => BuiltinType::I64,
        "u32" => BuiltinType::U32,
        "u64" => BuiltinType::U64,
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
        "bool", "i32", "i64", "u32", "u64", "f32", "f64", "char", "unit", "string", "error",
        "bytes",
    ]
}
