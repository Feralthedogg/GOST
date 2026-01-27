use super::ast::*;
use super::diagnostic::Diagnostics;
use super::lexer::{Keyword, Symbol, Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    idx: usize,
    pub diags: Diagnostics,
    next_expr_id: ExprId,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self::new_with_expr_id(tokens, 0)
    }

    pub fn new_with_expr_id(tokens: Vec<Token>, next_expr_id: ExprId) -> Self {
        Self {
            tokens,
            idx: 0,
            diags: Diagnostics::default(),
            next_expr_id,
        }
    }

    pub fn next_expr_id(&self) -> ExprId {
        self.next_expr_id
    }

    fn new_expr(&mut self, kind: ExprKind, span: Span) -> Expr {
        let id = self.next_expr_id;
        self.next_expr_id += 1;
        Expr { id, kind, span }
    }

    pub fn parse_file(&mut self) -> Option<FileAst> {
        let package = match self.parse_package() {
            Some(name) => name,
            None => return None,
        };
        self.consume_semis();
        let mut imports = Vec::new();
        while self.at_keyword(Keyword::Import) {
            self.bump();
            match self.parse_string_lit() {
                Some(s) => imports.push(s),
                None => self.diags.push("expected string literal after import", self.peek_span()),
            }
            self.consume_semis();
        }
        let mut items = Vec::new();
        while !self.at_eof() {
            if self.at_symbol(Symbol::Semi) {
                self.bump();
                continue;
            }
            if self.at_keyword(Keyword::Fn) {
                if let Some(func) = self.parse_function() {
                    items.push(Item::Function(func));
                }
                continue;
            }
            if self.at_keyword(Keyword::Copy) {
                if self.peek_is_keyword(Keyword::Struct) {
                    if let Some(def) = self.parse_struct_def() {
                        items.push(Item::Struct(def));
                    }
                } else if self.peek_is_keyword(Keyword::Enum) {
                    if let Some(def) = self.parse_enum_def() {
                        items.push(Item::Enum(def));
                    }
                } else {
                    self.error_here("expected struct or enum after copy");
                    self.bump();
                }
                continue;
            }
            if self.at_keyword(Keyword::Struct) {
                if let Some(def) = self.parse_struct_def() {
                    items.push(Item::Struct(def));
                }
                continue;
            }
            if self.at_keyword(Keyword::Enum) {
                if let Some(def) = self.parse_enum_def() {
                    items.push(Item::Enum(def));
                }
                continue;
            }
            self.error_here("expected item");
            self.bump();
        }
        Some(FileAst {
            package,
            imports,
            items,
        })
    }

    fn parse_package(&mut self) -> Option<String> {
        if !self.at_keyword(Keyword::Module) {
            self.error_here("file must start with `module`");
            return None;
        }
        self.bump();
        match self.bump().kind {
            TokenKind::Ident(name) => Some(name),
            _ => {
                self.error_here("expected module name");
                None
            }
        }
    }

    fn parse_function(&mut self) -> Option<Function> {
        let start = self.peek_span().unwrap_or(Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        });
        self.expect_keyword(Keyword::Fn);
        let name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected function name");
                return None;
            }
        };
        self.expect_symbol(Symbol::LParen);
        let mut params = Vec::new();
        if !self.at_symbol(Symbol::RParen) {
            loop {
                let param_start = self.peek_span().unwrap_or(start.clone());
                let param_name = match self.bump().kind {
                    TokenKind::Ident(name) => name,
                    _ => {
                        self.error_here("expected parameter name");
                        return None;
                    }
                };
                self.expect_symbol(Symbol::Colon);
                let ty = self.parse_type()?;
                params.push(Param {
                    name: param_name,
                    ty,
                    span: param_start,
                });
                if self.at_symbol(Symbol::Comma) {
                    self.bump();
                    if self.at_symbol(Symbol::RParen) {
                        break;
                    }
                } else {
                    break;
                }
            }
        }
        self.expect_symbol(Symbol::RParen);
        let ret_type = if self.at_symbol(Symbol::Arrow) {
            self.bump();
            Some(self.parse_type()?)
        } else {
            None
        };
        let body = self.parse_block()?;
        let end = body.span.clone();
        Some(Function {
            name,
            params,
            ret_type,
            body,
            span: Span {
                start: start.start,
                end: end.end,
                line: start.line,
                column: start.column,
            },
        })
    }

    fn parse_struct_def(&mut self) -> Option<StructDef> {
        let start = self.peek_span().unwrap_or(Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        });
        let mut is_copy = false;
        if self.at_keyword(Keyword::Copy) {
            is_copy = true;
            self.bump();
        }
        self.expect_keyword(Keyword::Struct);
        let name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected struct name");
                return None;
            }
        };
        self.expect_symbol(Symbol::LBrace);
        let mut fields = Vec::new();
        while !self.at_symbol(Symbol::RBrace) && !self.at_eof() {
            let field_start = self.peek_span().unwrap_or(start.clone());
            let field_name = match self.bump().kind {
                TokenKind::Ident(name) => name,
                _ => {
                    self.error_here("expected field name");
                    return None;
                }
            };
            self.expect_symbol(Symbol::Colon);
            let ty = self.parse_type()?;
            self.expect_symbol(Symbol::Semi);
            fields.push(Field {
                name: field_name,
                ty,
                span: field_start,
            });
        }
        self.expect_symbol(Symbol::RBrace);
        Some(StructDef {
            name,
            fields,
            is_copy,
            span: start,
        })
    }

    fn parse_enum_def(&mut self) -> Option<EnumDef> {
        let start = self.peek_span().unwrap_or(Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        });
        let mut is_copy = false;
        if self.at_keyword(Keyword::Copy) {
            is_copy = true;
            self.bump();
        }
        self.expect_keyword(Keyword::Enum);
        let name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected enum name");
                return None;
            }
        };
        self.expect_symbol(Symbol::LBrace);
        let mut variants = Vec::new();
        while !self.at_symbol(Symbol::RBrace) && !self.at_eof() {
            let var_start = self.peek_span().unwrap_or(start.clone());
            let var_name = match self.bump().kind {
                TokenKind::Ident(name) => name,
                _ => {
                    self.error_here("expected variant name");
                    return None;
                }
            };
            let mut fields = Vec::new();
            if self.at_symbol(Symbol::LParen) {
                self.bump();
                if !self.at_symbol(Symbol::RParen) {
                    loop {
                        fields.push(self.parse_type()?);
                        if self.at_symbol(Symbol::Comma) {
                            self.bump();
                            if self.at_symbol(Symbol::RParen) {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                }
                self.expect_symbol(Symbol::RParen);
            }
            self.expect_symbol(Symbol::Semi);
            variants.push(Variant {
                name: var_name,
                fields,
                span: var_start,
            });
        }
        self.expect_symbol(Symbol::RBrace);
        Some(EnumDef {
            name,
            variants,
            is_copy,
            span: start,
        })
    }

    fn parse_block(&mut self) -> Option<Block> {
        let start = self.peek_span().unwrap_or(Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        });
        self.expect_symbol(Symbol::LBrace);
        let mut stmts = Vec::new();
        let mut tail = None;
        while !self.at_symbol(Symbol::RBrace) && !self.at_eof() {
            if self.at_symbol(Symbol::Semi) {
                self.bump();
                continue;
            }
            if self.at_keyword(Keyword::Return) {
                let span = self.bump().span;
                let expr = if self.at_symbol(Symbol::Semi) || self.at_symbol(Symbol::RBrace) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                if self.at_symbol(Symbol::Semi) {
                    self.bump();
                }
                stmts.push(Stmt::Return { expr, span });
                continue;
            }
            if self.at_keyword(Keyword::Break) {
                let span = self.bump().span;
                if self.at_symbol(Symbol::Semi) {
                    self.bump();
                }
                stmts.push(Stmt::Break { span });
                continue;
            }
            if self.at_keyword(Keyword::Continue) {
                let span = self.bump().span;
                if self.at_symbol(Symbol::Semi) {
                    self.bump();
                }
                stmts.push(Stmt::Continue { span });
                continue;
            }
            if self.at_keyword(Keyword::Let) {
                let stmt = self.parse_let_stmt()?;
                stmts.push(stmt);
                continue;
            }
            if self.at_keyword(Keyword::For) {
                let stmt = self.parse_for_stmt()?;
                stmts.push(stmt);
                continue;
            }
            if self.at_keyword(Keyword::Select) {
                let stmt = self.parse_select_stmt()?;
                stmts.push(stmt);
                continue;
            }
            if self.at_keyword(Keyword::Go) {
                let span = self.bump().span;
                let expr = self.parse_expr()?;
                if self.at_symbol(Symbol::Semi) {
                    self.bump();
                }
                stmts.push(Stmt::Go { expr, span });
                continue;
            }
            if self.at_keyword(Keyword::Defer) {
                let span = self.bump().span;
                let expr = self.parse_expr()?;
                if self.at_symbol(Symbol::Semi) {
                    self.bump();
                }
                stmts.push(Stmt::Defer { expr, span });
                continue;
            }
            let expr = self.parse_expr()?;
            if self.at_symbol(Symbol::Eq) {
                let span = self.bump().span;
                let value = self.parse_expr()?;
                if self.at_symbol(Symbol::Semi) {
                    self.bump();
                }
                stmts.push(Stmt::Assign {
                    target: expr,
                    value,
                    span,
                });
                continue;
            }
            if self.at_symbol(Symbol::Semi) {
                let span = self.bump().span;
                stmts.push(Stmt::Expr { expr, span });
                continue;
            }
            tail = Some(expr);
            break;
        }
        self.expect_symbol(Symbol::RBrace);
        let end = self.peek_span().unwrap_or(start.clone());
        Some(Block {
            stmts,
            tail,
            span: Span {
                start: start.start,
                end: end.end,
                line: start.line,
                column: start.column,
            },
        })
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        let span = self.bump().span;
        let name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected identifier after let");
                return None;
            }
        };
        let ty = if self.at_symbol(Symbol::Colon) {
            self.bump();
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect_symbol(Symbol::Eq);
        let init = self.parse_expr()?;
        if self.at_symbol(Symbol::Semi) {
            self.bump();
        }
        Some(Stmt::Let {
            name,
            ty,
            init,
            span,
        })
    }

    fn parse_for_stmt(&mut self) -> Option<Stmt> {
        let span = self.bump().span;
        let name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected iterator variable");
                return None;
            }
        };
        self.expect_keyword(Keyword::In);
        let iter = self.parse_expr()?;
        let body = self.parse_block()?;
        Some(Stmt::ForIn {
            name,
            iter,
            body,
            span,
        })
    }

    fn parse_select_stmt(&mut self) -> Option<Stmt> {
        let span = self.bump().span;
        self.expect_symbol(Symbol::LBrace);
        let mut arms = Vec::new();
        while !self.at_symbol(Symbol::RBrace) && !self.at_eof() {
            if self.at_keyword(Keyword::Default) {
                let arm_span = self.bump().span;
                self.expect_symbol(Symbol::FatArrow);
                let body = self.parse_block_or_expr()?;
                if self.at_symbol(Symbol::Comma) {
                    self.bump();
                }
                arms.push(SelectArm {
                    kind: SelectArmKind::Default,
                    body,
                    span: arm_span,
                });
                continue;
            }
            self.expect_keyword(Keyword::Case);
            let arm_span = self.peek_span().unwrap_or(span.clone());
            let mut kind = if self.at_keyword(Keyword::Send) {
                self.bump();
                self.expect_symbol(Symbol::LParen);
                let chan = self.parse_expr()?;
                self.expect_symbol(Symbol::Comma);
                let value = self.parse_expr()?;
                self.expect_symbol(Symbol::RParen);
                SelectArmKind::Send { chan, value }
            } else if self.at_keyword(Keyword::Recv) {
                self.bump();
                self.expect_symbol(Symbol::LParen);
                let chan = self.parse_expr()?;
                self.expect_symbol(Symbol::RParen);
                SelectArmKind::Recv { chan, bind: None }
            } else if self.at_keyword(Keyword::After) {
                self.bump();
                self.expect_symbol(Symbol::LParen);
                let ms = self.parse_expr()?;
                self.expect_symbol(Symbol::RParen);
                SelectArmKind::After { ms }
            } else {
                self.error_here("expected send/recv/after/default in select arm");
                return None;
            };
            self.expect_symbol(Symbol::FatArrow);
            if let SelectArmKind::Recv { chan, .. } = kind {
                let bind = if self.at_symbol(Symbol::Pipe) {
                    self.bump();
                    let name1 = match self.bump().kind {
                        TokenKind::Ident(name) => name,
                        _ => {
                            self.error_here("expected binding name");
                            return None;
                        }
                    };
                    self.expect_symbol(Symbol::Comma);
                    let name2 = match self.bump().kind {
                        TokenKind::Ident(name) => name,
                        _ => {
                            self.error_here("expected binding name");
                            return None;
                        }
                    };
                    self.expect_symbol(Symbol::Pipe);
                    Some((name1, name2))
                } else {
                    None
                };
                kind = SelectArmKind::Recv { chan, bind };
            }
            let body = self.parse_block_or_expr()?;
            if self.at_symbol(Symbol::Comma) {
                self.bump();
            }
            arms.push(SelectArm {
                kind,
                body,
                span: arm_span,
            });
        }
        self.expect_symbol(Symbol::RBrace);
        Some(Stmt::Select { arms, span })
    }

    fn parse_block_or_expr(&mut self) -> Option<BlockOrExpr> {
        if self.at_symbol(Symbol::LBrace) {
            Some(BlockOrExpr::Block(Box::new(self.parse_block()?)))
        } else {
            Some(BlockOrExpr::Expr(self.parse_expr()?))
        }
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        self.parse_binary_expr(0)
    }

    fn parse_binary_expr(&mut self, min_prec: u8) -> Option<Expr> {
        let mut left = self.parse_unary_expr()?;
        loop {
            let (prec, op) = match self.peek_binary_op() {
                Some(pair) => pair,
                None => break,
            };
            if prec < min_prec {
                break;
            }
            self.bump();
            let right = self.parse_binary_expr(prec + 1)?;
            let span = left.span.clone();
            left = self.new_expr(
                ExprKind::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                span,
            );
        }
        Some(left)
    }

    fn parse_unary_expr(&mut self) -> Option<Expr> {
        if self.at_symbol(Symbol::Bang) {
            let span = self.bump().span;
            let expr = self.parse_unary_expr()?;
            return Some(self.new_expr(
                ExprKind::Unary {
                    op: UnaryOp::Not,
                    expr: Box::new(expr),
                },
                span,
            ));
        }
        if self.at_symbol(Symbol::Minus) {
            let span = self.bump().span;
            let expr = self.parse_unary_expr()?;
            return Some(self.new_expr(
                ExprKind::Unary {
                    op: UnaryOp::Neg,
                    expr: Box::new(expr),
                },
                span,
            ));
        }
        if self.at_symbol(Symbol::Amp) {
            let span = self.bump().span;
            let is_mut = if self.at_keyword(Keyword::Mut) {
                self.bump();
                true
            } else {
                false
            };
            let expr = self.parse_unary_expr()?;
            return Some(self.new_expr(
                ExprKind::Borrow {
                    is_mut,
                    expr: Box::new(expr),
                },
                span,
            ));
        }
        if self.at_symbol(Symbol::Star) {
            let span = self.bump().span;
            let expr = self.parse_unary_expr()?;
            return Some(self.new_expr(
                ExprKind::Deref { expr: Box::new(expr) },
                span,
            ));
        }
        let mut expr = self.parse_postfix_expr()?;
        if self.at_symbol(Symbol::Question) {
            let span = self.bump().span;
            expr = self.new_expr(
                ExprKind::Try {
                    expr: Box::new(expr),
                },
                span,
            );
        }
        Some(expr)
    }

    fn parse_postfix_expr(&mut self) -> Option<Expr> {
        let mut expr = self.parse_primary_expr()?;
        loop {
            if self.at_symbol(Symbol::LBracket) && self.is_builtin_generic_callee(&expr) {
                let span = self.peek_span().unwrap_or(expr.span.clone());
                let type_args = self.parse_type_args()?;
                self.expect_symbol(Symbol::LParen);
                let mut args = Vec::new();
                if !self.at_symbol(Symbol::RParen) {
                    loop {
                        args.push(self.parse_expr()?);
                        if self.at_symbol(Symbol::Comma) {
                            self.bump();
                            if self.at_symbol(Symbol::RParen) {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                }
                self.expect_symbol(Symbol::RParen);
                expr = self.new_expr(
                    ExprKind::Call {
                        callee: Box::new(expr),
                        type_args,
                        args,
                    },
                    span,
                );
                continue;
            }
            if self.at_symbol(Symbol::LParen) {
                let span = self.bump().span;
                let mut args = Vec::new();
                if !self.at_symbol(Symbol::RParen) {
                    loop {
                        args.push(self.parse_expr()?);
                        if self.at_symbol(Symbol::Comma) {
                            self.bump();
                            if self.at_symbol(Symbol::RParen) {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                }
                self.expect_symbol(Symbol::RParen);
                expr = self.new_expr(
                    ExprKind::Call {
                        callee: Box::new(expr),
                        type_args: Vec::new(),
                        args,
                    },
                    span,
                );
                continue;
            }
            if self.at_symbol(Symbol::Dot) {
                let span = self.bump().span;
                let name = match self.bump().kind {
                    TokenKind::Ident(name) => name,
                    _ => {
                        self.error_here("expected field name");
                        return None;
                    }
                };
                expr = self.new_expr(
                    ExprKind::Field {
                        base: Box::new(expr),
                        name,
                    },
                    span,
                );
                continue;
            }
            if self.at_symbol(Symbol::LBracket) {
                let span = self.bump().span;
                let index = self.parse_expr()?;
                self.expect_symbol(Symbol::RBracket);
                expr = self.new_expr(
                    ExprKind::Index {
                        base: Box::new(expr),
                        index: Box::new(index),
                    },
                    span,
                );
                continue;
            }
            break;
        }
        Some(expr)
    }

    fn parse_primary_expr(&mut self) -> Option<Expr> {
        let span = self.peek_span().unwrap_or(Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        });
        if self.at_symbol(Symbol::LBrace) {
            let block = self.parse_block()?;
            return Some(self.new_expr(
                ExprKind::Block(Box::new(block.clone())),
                block.span,
            ));
        }
        if self.at_keyword(Keyword::If) {
            self.bump();
            let cond = self.parse_expr()?;
            let then_block = Box::new(self.parse_block()?);
            let else_block = if self.at_keyword(Keyword::Else) {
                self.bump();
                Some(Box::new(self.parse_block()?))
            } else {
                None
            };
            return Some(self.new_expr(
                ExprKind::If {
                    cond: Box::new(cond),
                    then_block,
                    else_block,
                },
                span,
            ));
        }
        if self.at_keyword(Keyword::Match) {
            self.bump();
            let scrutinee = self.parse_expr()?;
            self.expect_symbol(Symbol::LBrace);
            let mut arms = Vec::new();
            while !self.at_symbol(Symbol::RBrace) && !self.at_eof() {
                let arm_span = self.peek_span().unwrap_or(span.clone());
                let pattern = self.parse_pattern()?;
                self.expect_symbol(Symbol::FatArrow);
                let body = self.parse_block_or_expr()?;
                self.expect_symbol(Symbol::Comma);
                arms.push(MatchArm {
                    pattern,
                    body,
                    span: arm_span,
                });
            }
            self.expect_symbol(Symbol::RBrace);
            return Some(self.new_expr(
                ExprKind::Match {
                    scrutinee: Box::new(scrutinee),
                    arms,
                },
                span,
            ));
        }
        match self.bump().kind {
            TokenKind::IntLit(value) => Some(self.new_expr(ExprKind::Int(value), span)),
            TokenKind::FloatLit(value) => Some(self.new_expr(ExprKind::Float(value), span)),
            TokenKind::StringLit(value) => Some(self.new_expr(ExprKind::String(value), span)),
            TokenKind::CharLit(value) => Some(self.new_expr(ExprKind::Char(value), span)),
            TokenKind::Keyword(Keyword::True) => Some(self.new_expr(ExprKind::Bool(true), span)),
            TokenKind::Keyword(Keyword::False) => Some(self.new_expr(ExprKind::Bool(false), span)),
            TokenKind::Keyword(Keyword::Nil) => Some(self.new_expr(ExprKind::Nil, span)),
            TokenKind::Ident(name) => Some(self.new_expr(ExprKind::Ident(name), span)),
            TokenKind::Symbol(Symbol::LParen) => {
                if self.at_symbol(Symbol::RParen) {
                    self.bump();
                    return Some(self.new_expr(ExprKind::Tuple(Vec::new()), span));
                }
                let first = self.parse_expr()?;
                if self.at_symbol(Symbol::Comma) {
                    self.bump();
                    let mut items = vec![first];
                    if !self.at_symbol(Symbol::RParen) {
                        loop {
                            items.push(self.parse_expr()?);
                            if self.at_symbol(Symbol::Comma) {
                                self.bump();
                                if self.at_symbol(Symbol::RParen) {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect_symbol(Symbol::RParen);
                    return Some(self.new_expr(ExprKind::Tuple(items), span));
                }
                self.expect_symbol(Symbol::RParen);
                Some(first)
            }
            TokenKind::Keyword(Keyword::Send) => {
                self.expect_symbol(Symbol::LParen);
                let chan = self.parse_expr()?;
                self.expect_symbol(Symbol::Comma);
                let value = self.parse_expr()?;
                self.expect_symbol(Symbol::RParen);
                Some(self.new_expr(
                    ExprKind::Send {
                        chan: Box::new(chan),
                        value: Box::new(value),
                    },
                    span,
                ))
            }
            TokenKind::Keyword(Keyword::Recv) => {
                self.expect_symbol(Symbol::LParen);
                let chan = self.parse_expr()?;
                self.expect_symbol(Symbol::RParen);
                Some(self.new_expr(
                    ExprKind::Recv {
                        chan: Box::new(chan),
                    },
                    span,
                ))
            }
            TokenKind::Keyword(Keyword::Close) => {
                self.expect_symbol(Symbol::LParen);
                let chan = self.parse_expr()?;
                self.expect_symbol(Symbol::RParen);
                Some(self.new_expr(
                    ExprKind::Close {
                        chan: Box::new(chan),
                    },
                    span,
                ))
            }
            TokenKind::Keyword(Keyword::After) => {
                self.expect_symbol(Symbol::LParen);
                let ms = self.parse_expr()?;
                self.expect_symbol(Symbol::RParen);
                Some(self.new_expr(
                    ExprKind::After { ms: Box::new(ms) },
                    span,
                ))
            }
            TokenKind::Unknown(ch) => {
                self.diags
                    .push(format!("unexpected character `{}`", ch), Some(span));
                None
            }
            _ => {
                self.error_here("unexpected token");
                None
            }
        }
    }

    fn parse_type_args(&mut self) -> Option<Vec<TypeAst>> {
        self.expect_symbol(Symbol::LBracket);
        let mut args = Vec::new();
        if !self.at_symbol(Symbol::RBracket) {
            loop {
                args.push(self.parse_type()?);
                if self.at_symbol(Symbol::Comma) {
                    self.bump();
                    if self.at_symbol(Symbol::RBracket) {
                        break;
                    }
                } else {
                    break;
                }
            }
        }
        self.expect_symbol(Symbol::RBracket);
        Some(args)
    }

    fn parse_pattern(&mut self) -> Option<Pattern> {
        let token = self.bump();
        match token.kind {
            TokenKind::Ident(name) if name == "_" => Some(Pattern::Wildcard),
            TokenKind::Ident(name) => {
                if self.at_symbol(Symbol::Dot) {
                    self.bump();
                    let variant = match self.bump().kind {
                        TokenKind::Ident(name) => name,
                        _ => {
                            self.error_here("expected variant name");
                            return None;
                        }
                    };
                    let mut binds = Vec::new();
                    if self.at_symbol(Symbol::LParen) {
                        self.bump();
                        if !self.at_symbol(Symbol::RParen) {
                            loop {
                                match self.bump().kind {
                                    TokenKind::Ident(name) => binds.push(name),
                                    _ => {
                                        self.error_here("expected binding name");
                                        return None;
                                    }
                                }
                                if self.at_symbol(Symbol::Comma) {
                                    self.bump();
                                    if self.at_symbol(Symbol::RParen) {
                                        break;
                                    }
                                } else {
                                    break;
                                }
                            }
                        }
                        self.expect_symbol(Symbol::RParen);
                    }
                    Some(Pattern::Variant {
                        enum_name: name,
                        variant,
                        binds,
                    })
                } else {
                    Some(Pattern::Ident(name))
                }
            }
            TokenKind::IntLit(value) => Some(Pattern::Int(value)),
            TokenKind::Keyword(Keyword::True) => Some(Pattern::Bool(true)),
            TokenKind::Keyword(Keyword::False) => Some(Pattern::Bool(false)),
            _ => {
                self.error_here("unexpected pattern");
                None
            }
        }
    }

    fn parse_type(&mut self) -> Option<TypeAst> {
        let span = self.peek_span().unwrap_or(Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        });
        if self.at_symbol(Symbol::LBracket) {
            self.bump();
            self.expect_symbol(Symbol::RBracket);
            let inner = self.parse_type()?;
            return Some(TypeAst {
                kind: TypeAstKind::Slice(Box::new(inner)),
                span,
            });
        }
        if self.at_symbol(Symbol::LParen) {
            self.bump();
            let mut items = Vec::new();
            if !self.at_symbol(Symbol::RParen) {
                loop {
                    items.push(self.parse_type()?);
                    if self.at_symbol(Symbol::Comma) {
                        self.bump();
                        if self.at_symbol(Symbol::RParen) {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            }
            self.expect_symbol(Symbol::RParen);
            return Some(TypeAst {
                kind: TypeAstKind::Tuple(items),
                span,
            });
        }
        if self.at_keyword(Keyword::Interface) {
            self.bump();
            return Some(TypeAst {
                kind: TypeAstKind::Interface,
                span,
            });
        }
        if self.at_ident("ref") {
            self.bump();
            self.expect_symbol(Symbol::LBracket);
            let inner = self.parse_type()?;
            self.expect_symbol(Symbol::RBracket);
            return Some(TypeAst {
                kind: TypeAstKind::Ref(Box::new(inner)),
                span,
            });
        }
        if self.at_ident("mutref") {
            self.bump();
            self.expect_symbol(Symbol::LBracket);
            let inner = self.parse_type()?;
            self.expect_symbol(Symbol::RBracket);
            return Some(TypeAst {
                kind: TypeAstKind::MutRef(Box::new(inner)),
                span,
            });
        }
        if self.at_ident("shared") {
            self.bump();
            self.expect_symbol(Symbol::LBracket);
            let inner = self.parse_type()?;
            self.expect_symbol(Symbol::RBracket);
            return Some(TypeAst {
                kind: TypeAstKind::Shared(Box::new(inner)),
                span,
            });
        }
        if self.at_ident("chan") {
            self.bump();
            self.expect_symbol(Symbol::LBracket);
            let inner = self.parse_type()?;
            self.expect_symbol(Symbol::RBracket);
            return Some(TypeAst {
                kind: TypeAstKind::Chan(Box::new(inner)),
                span,
            });
        }
        if self.at_ident("map") {
            self.bump();
            self.expect_symbol(Symbol::LBracket);
            let key = self.parse_type()?;
            self.expect_symbol(Symbol::Comma);
            let value = self.parse_type()?;
            self.expect_symbol(Symbol::RBracket);
            return Some(TypeAst {
                kind: TypeAstKind::Map(Box::new(key), Box::new(value)),
                span,
            });
        }
        if let TokenKind::Ident(name) = self.bump().kind {
            Some(TypeAst {
                kind: TypeAstKind::Named(name),
                span,
            })
        } else {
            self.error_here("expected type");
            None
        }
    }

    fn parse_string_lit(&mut self) -> Option<String> {
        match self.bump().kind {
            TokenKind::StringLit(s) => Some(s),
            _ => None,
        }
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.idx).unwrap_or_else(|| {
            self.tokens
                .last()
                .expect("parser always has eof token")
        })
    }

    fn bump(&mut self) -> Token {
        let token = self.peek().clone();
        if !matches!(token.kind, TokenKind::Eof) {
            self.idx += 1;
        }
        token
    }

    fn at_eof(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Eof)
    }

    fn at_symbol(&self, symbol: Symbol) -> bool {
        matches!(&self.peek().kind, TokenKind::Symbol(sym) if *sym == symbol)
    }

    fn at_keyword(&self, keyword: Keyword) -> bool {
        matches!(&self.peek().kind, TokenKind::Keyword(kw) if *kw == keyword)
    }

    fn peek_is_keyword(&self, keyword: Keyword) -> bool {
        matches!(
            self.tokens.get(self.idx + 1).map(|t| &t.kind),
            Some(TokenKind::Keyword(kw)) if *kw == keyword
        )
    }

    fn at_ident(&self, name: &str) -> bool {
        matches!(self.peek().kind, TokenKind::Ident(ref s) if s == name)
    }

    fn is_builtin_generic_callee(&self, expr: &Expr) -> bool {
        let name = match &expr.kind {
            ExprKind::Ident(name) => name.as_str(),
            _ => return false,
        };
        matches!(
            name,
            "make_chan"
                | "make_slice"
                | "slice_len"
                | "slice_get_copy"
                | "slice_set"
                | "slice_ref"
                | "slice_mutref"
                | "slice_push"
                | "slice_pop"
                | "shared_new"
                | "shared_get"
                | "shared_get_mut"
                | "make_map"
                | "map_get"
                | "map_set"
                | "map_del"
                | "map_len"
        )
    }

    fn consume_semis(&mut self) {
        while self.at_symbol(Symbol::Semi) {
            self.bump();
        }
    }

    fn expect_symbol(&mut self, symbol: Symbol) {
        if !self.at_symbol(symbol) {
            self.error_here("unexpected token");
        } else {
            self.bump();
        }
    }

    fn expect_keyword(&mut self, keyword: Keyword) {
        if !self.at_keyword(keyword) {
            self.error_here("unexpected token");
        } else {
            self.bump();
        }
    }

    fn peek_span(&self) -> Option<Span> {
        Some(self.peek().span.clone())
    }

    fn error_here(&mut self, message: &str) {
        self.diags.push(message, self.peek_span());
    }

    fn peek_binary_op(&self) -> Option<(u8, BinaryOp)> {
        let op = match self.peek().kind {
            TokenKind::Symbol(Symbol::OrOr) => (1, BinaryOp::Or),
            TokenKind::Symbol(Symbol::AndAnd) => (2, BinaryOp::And),
            TokenKind::Symbol(Symbol::EqEq) => (3, BinaryOp::Eq),
            TokenKind::Symbol(Symbol::NotEq) => (3, BinaryOp::NotEq),
            TokenKind::Symbol(Symbol::Lt) => (4, BinaryOp::Lt),
            TokenKind::Symbol(Symbol::Lte) => (4, BinaryOp::Lte),
            TokenKind::Symbol(Symbol::Gt) => (4, BinaryOp::Gt),
            TokenKind::Symbol(Symbol::Gte) => (4, BinaryOp::Gte),
            TokenKind::Symbol(Symbol::Plus) => (5, BinaryOp::Add),
            TokenKind::Symbol(Symbol::Minus) => (5, BinaryOp::Sub),
            TokenKind::Symbol(Symbol::Star) => (6, BinaryOp::Mul),
            TokenKind::Symbol(Symbol::Slash) => (6, BinaryOp::Div),
            TokenKind::Symbol(Symbol::Percent) => (6, BinaryOp::Rem),
            _ => return None,
        };
        Some(op)
    }
}
