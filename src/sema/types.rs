use std::collections::HashMap;

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
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub variants: Vec<(String, Vec<Type>)>,
    pub is_copy: bool,
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
            Type::Shared(_) | Type::Interface => Some(TypeClass::Copy),
            Type::Slice(_) | Type::Map(_, _) | Type::Chan(_) => Some(TypeClass::Linear),
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
