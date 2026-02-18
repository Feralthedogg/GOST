#[derive(Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Debug)]
pub struct FileAst {
    pub package: String,
    pub imports: Vec<ImportSpec>,
    pub items: Vec<Item>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

impl Visibility {
    pub fn is_public(self) -> bool {
        matches!(self, Visibility::Public)
    }
}

#[derive(Clone, Debug)]
pub struct ImportSpec {
    pub path: String,
    pub alias: Option<String>,
    pub only: Option<Vec<String>>,
}

#[derive(Clone, Debug)]
pub enum Item {
    Function(Function),
    ExternGlobal(ExternGlobal),
    TypeAlias(TypeAlias),
    Global(GlobalVar),
    Const(ConstItem),
    Struct(StructDef),
    Enum(EnumDef),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub vis: Visibility,
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
    pub is_variadic: bool,
    pub ret_type: Option<TypeAst>,
    pub is_extern: bool,
    pub is_unsafe: bool,
    pub extern_abi: Option<String>,
    pub body: Block,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ExternGlobal {
    pub vis: Visibility,
    pub name: String,
    pub ty: TypeAst,
    pub extern_abi: Option<String>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TypeAlias {
    pub vis: Visibility,
    pub name: String,
    pub ty: TypeAst,
    pub is_trait: bool,
    pub trait_methods: Vec<TraitMethod>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct GlobalVar {
    pub vis: Visibility,
    pub name: String,
    pub ty: Option<TypeAst>,
    pub init: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ConstItem {
    pub vis: Visibility,
    pub name: String,
    pub ty: Option<TypeAst>,
    pub init: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: String,
    pub ty: TypeAst,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TypeParam {
    pub name: String,
    pub bounds: Vec<TypeAst>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ClosureParam {
    pub name: String,
    pub ty: Option<TypeAst>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub vis: Visibility,
    pub name: String,
    pub fields: Vec<Field>,
    pub is_copy: bool,
    pub layout: LayoutAttr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub vis: Visibility,
    pub name: String,
    pub ty: TypeAst,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TraitMethod {
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
    pub is_variadic: bool,
    pub ret_type: TypeAst,
    pub default_body: Option<Block>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub vis: Visibility,
    pub name: String,
    pub variants: Vec<Variant>,
    pub is_copy: bool,
    pub layout: LayoutAttr,
    pub span: Span,
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

#[derive(Clone, Debug, Default)]
pub struct LayoutAttr {
    pub repr_c: bool,
    pub repr_transparent: bool,
    pub repr_int: Option<ReprInt>,
    pub repr_other: Option<String>,
    pub pack: Option<u32>,
    pub bitfield: bool,
}

impl LayoutAttr {
    pub fn has_any(&self) -> bool {
        self.repr_c
            || self.repr_transparent
            || self.repr_int.is_some()
            || self.repr_other.is_some()
            || self.pack.is_some()
            || self.bitfield
    }
}

#[derive(Clone, Debug)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<TypeAst>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub tail: Option<Expr>,
    pub span: Span,
}

pub type ExprId = usize;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShlAssign,
    ShrAssign,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Let {
        name: String,
        ty: Option<TypeAst>,
        init: Expr,
        span: Span,
    },
    Const {
        name: String,
        ty: Option<TypeAst>,
        init: Expr,
        span: Span,
    },
    Assign {
        op: AssignOp,
        target: Expr,
        value: Expr,
        span: Span,
    },
    Expr {
        expr: Expr,
        span: Span,
    },
    Return {
        expr: Option<Expr>,
        span: Span,
    },
    Break {
        label: Option<String>,
        span: Span,
    },
    Continue {
        label: Option<String>,
        span: Span,
    },
    While {
        label: Option<String>,
        cond: Expr,
        body: Block,
        span: Span,
    },
    Loop {
        label: Option<String>,
        body: Block,
        span: Span,
    },
    ForIn {
        label: Option<String>,
        name: String,
        index: Option<String>,
        iter: Expr,
        body: Block,
        span: Span,
    },
    ForRange {
        label: Option<String>,
        name: String,
        index: Option<String>,
        start: Expr,
        end: Expr,
        inclusive: bool,
        body: Block,
        span: Span,
    },
    Select {
        arms: Vec<SelectArm>,
        span: Span,
    },
    Go {
        expr: Expr,
        span: Span,
    },
    Defer {
        expr: Expr,
        span: Span,
    },
}

#[derive(Clone, Debug)]
pub struct SelectArm {
    pub kind: SelectArmKind,
    pub body: BlockOrExpr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum SelectArmKind {
    Send {
        chan: Expr,
        value: Expr,
    },
    Recv {
        chan: Expr,
        bind: Option<(String, String)>,
    },
    After {
        ms: Expr,
    },
    Default,
}

#[derive(Clone, Debug)]
pub enum BlockOrExpr {
    Block(Box<Block>),
    Expr(Expr),
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub id: ExprId,
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Bool(bool),
    Int(String),
    Float(String),
    Char(char),
    String(String),
    Nil,
    Ident(String),
    StructLit {
        name: String,
        fields: Vec<(String, Expr)>,
    },
    ArrayLit(Vec<Expr>),
    Tuple(Vec<Expr>),
    Block(Box<Block>),
    UnsafeBlock(Box<Block>),
    If {
        cond: Box<Expr>,
        then_block: Box<Block>,
        else_block: Option<Box<Block>>,
    },
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Closure {
        params: Vec<ClosureParam>,
        body: Box<BlockOrExpr>,
    },
    Call {
        callee: Box<Expr>,
        type_args: Vec<TypeAst>,
        args: Vec<Expr>,
    },
    Field {
        base: Box<Expr>,
        name: String,
    },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Cast {
        expr: Box<Expr>,
        ty: TypeAst,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Borrow {
        is_mut: bool,
        expr: Box<Expr>,
    },
    Deref {
        expr: Box<Expr>,
    },
    Try {
        expr: Box<Expr>,
    },
    Send {
        chan: Box<Expr>,
        value: Box<Expr>,
    },
    Recv {
        chan: Box<Expr>,
    },
    Close {
        chan: Box<Expr>,
    },
    After {
        ms: Box<Expr>,
    },
}

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: BlockOrExpr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Wildcard,
    Bool(bool),
    Int(String),
    Ident(String),
    Or(Vec<Pattern>),
    Variant {
        enum_name: String,
        variant: String,
        binds: Vec<String>,
    },
}

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    NotEq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

#[derive(Clone, Debug)]
pub struct TypeAst {
    pub kind: TypeAstKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum TypeAstKind {
    Named(String),
    Ref(Box<TypeAst>),
    MutRef(Box<TypeAst>),
    Own(Box<TypeAst>),
    Alias(Box<TypeAst>),
    Slice(Box<TypeAst>),
    Array(Box<TypeAst>, usize),
    Map(Box<TypeAst>, Box<TypeAst>),
    Result(Box<TypeAst>, Box<TypeAst>),
    Chan(Box<TypeAst>),
    Shared(Box<TypeAst>),
    Interface,
    Tuple(Vec<TypeAst>),
    FnPtr {
        params: Vec<TypeAst>,
        ret: Box<TypeAst>,
        is_variadic: bool,
    },
    Closure {
        params: Vec<TypeAst>,
        ret: Box<TypeAst>,
        is_variadic: bool,
    },
}
