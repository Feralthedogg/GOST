use crate::frontend::ast::{Block, Expr, ExprId, Pattern, SelectArm};
use crate::sema::types::{Type, TypeClass};
use std::collections::HashMap;

pub mod lower;
pub mod passes;

pub type BlockId = usize;
pub type LocalId = usize;
pub type ScopeId = usize;

#[derive(Clone, Debug)]
pub enum CleanupItem {
    DeferCall { call: Expr },
    DropLocal { local: LocalId },
    DropName { name: String, ty: Type },
}

#[derive(Clone, Debug)]
pub struct ScopeFrame {
    pub parent: Option<ScopeId>,
    pub depth: isize,
    pub items: Vec<CleanupItem>,
}

#[derive(Clone, Debug, Default)]
pub struct MirModule {
    pub functions: Vec<MirFunction>,
}

#[derive(Clone, Debug)]
pub struct MirFunction {
    pub name: String,
    pub ret_ty: Type,
    pub param_count: usize,
    pub locals: Vec<Local>,
    pub expr_types: HashMap<ExprId, Type>,
    pub blocks: Vec<BasicBlock>,
    pub block_depths: Vec<isize>,
    pub scopes: Vec<ScopeFrame>,
    pub block_scopes: Vec<Option<ScopeId>>,
    pub block_exit_scopes: Vec<Option<ScopeId>>,
    pub entry: BlockId,
}

#[derive(Clone, Debug)]
pub struct Local {
    pub name: Option<String>,
    pub ty: Type,
    pub class: TypeClass,
}

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub stmts: Vec<MirStmt>,
    pub term: Terminator,
}

#[derive(Clone, Debug)]
pub enum MirStmt {
    EnterScope { scope: ScopeId },
    ExitScope { scope: ScopeId },
    ForIn {
        name: String,
        iter: Expr,
        body: Block,
    },
    Select {
        arms: Vec<SelectArm>,
    },
    Go {
        expr: Expr,
    },
    Let {
        local: LocalId,
        init: Expr,
    },
    Assign {
        target: Expr,
        value: Expr,
    },
    Expr {
        expr: Expr,
    },
    DeferCall {
        call: Expr,
    },
    Eval {
        expr: Expr,
        out: Vec<LocalId>,
    },
    Drop {
        local: LocalId,
    },
    DropName {
        name: String,
        ty: Type,
    },
}

#[derive(Clone, Debug)]
pub enum Terminator {
    Goto(BlockId),
    If {
        cond: Expr,
        then_bb: BlockId,
        else_bb: BlockId,
    },
    Match {
        scrutinee: Expr,
        arms: Vec<(Pattern, BlockId)>,
        default: Option<BlockId>,
    },
    Return {
        value: Option<Expr>,
    },
    ReturnError {
        err: Expr,
    },
}

impl BasicBlock {
    pub fn new(term: Terminator) -> Self {
        Self {
            stmts: Vec::new(),
            term,
        }
    }
}
