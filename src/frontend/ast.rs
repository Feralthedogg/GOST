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
    pub imports: Vec<String>,
    pub items: Vec<Item>,
}

#[derive(Clone, Debug)]
pub enum Item {
    Function(Function),
    ExternGlobal(ExternGlobal),
    Struct(StructDef),
    Enum(EnumDef),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
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
    pub name: String,
    pub ty: TypeAst,
    pub extern_abi: Option<String>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: String,
    pub ty: TypeAst,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<Field>,
    pub is_copy: bool,
    pub layout: LayoutAttr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: String,
    pub ty: TypeAst,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub name: String,
    pub variants: Vec<Variant>,
    pub is_copy: bool,
    pub layout: LayoutAttr,
    pub span: Span,
}

#[derive(Clone, Debug, Default)]
pub struct LayoutAttr {
    pub repr_c: bool,
    pub pack: Option<u32>,
    pub bitfield: bool,
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

#[derive(Clone, Debug)]
pub enum Stmt {
    Let {
        name: String,
        ty: Option<TypeAst>,
        init: Expr,
        span: Span,
    },
    Assign {
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
        span: Span,
    },
    Continue {
        span: Span,
    },
    While {
        cond: Expr,
        body: Block,
        span: Span,
    },
    Loop {
        body: Block,
        span: Span,
    },
    ForIn {
        name: String,
        iter: Expr,
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
    Send { chan: Expr, value: Expr },
    Recv { chan: Expr, bind: Option<(String, String)> },
    After { ms: Expr },
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
    pub body: BlockOrExpr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Wildcard,
    Bool(bool),
    Int(String),
    Ident(String),
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
    Slice(Box<TypeAst>),
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
}
