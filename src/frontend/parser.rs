use super::ast::*;
use super::diagnostic::Diagnostics;
use super::lexer::{Keyword, Symbol, Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    idx: usize,
    pub diags: Diagnostics,
    next_expr_id: ExprId,
    allow_struct_lit: bool,
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
            allow_struct_lit: true,
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

    fn parse_expr_no_struct_lit(&mut self) -> Option<Expr> {
        let prev = self.allow_struct_lit;
        self.allow_struct_lit = false;
        let expr = self.parse_expr();
        self.allow_struct_lit = prev;
        expr
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
            let layout = self.parse_layout_modifiers();
            let has_layout = layout.repr_c || layout.pack.is_some() || layout.bitfield;
            if has_layout
                && !self.at_keyword(Keyword::Copy)
                && !self.at_keyword(Keyword::Struct)
                && !self.at_keyword(Keyword::Enum)
            {
                self.diags.push(
                    "layout modifiers apply only to struct/enum items",
                    self.peek_span(),
                );
            }
            let mut is_unsafe = false;
            let mut start_override: Option<Span> = None;
            if self.at_keyword(Keyword::Unsafe) {
                let start = self.bump().span;
                is_unsafe = true;
                start_override = Some(start.clone());
                if !self.at_keyword(Keyword::Extern) && !self.at_keyword(Keyword::Fn) {
                    self.diags
                        .push("expected fn or extern after unsafe", Some(start));
                    continue;
                }
            }
            if self.at_keyword(Keyword::Extern) {
                let start = start_override.unwrap_or_else(|| {
                    self.peek_span().unwrap_or(Span {
                        start: 0,
                        end: 0,
                        line: 1,
                        column: 1,
                    })
                });
                self.bump();
                let abi = if matches!(self.peek().kind, TokenKind::StringLit(_)) {
                    self.parse_string_lit().unwrap_or_else(|| "C".to_string())
                } else {
                    "C".to_string()
                };
                if self.at_keyword(Keyword::Unsafe) {
                    if is_unsafe {
                        self.error_here("duplicate unsafe");
                    }
                    self.bump();
                    is_unsafe = true;
                }
                if self.at_keyword(Keyword::Fn) {
                    if let Some(func) =
                        self.parse_function(true, is_unsafe, Some(abi), Some(start))
                    {
                        items.push(Item::Function(func));
                    }
                } else if self.at_keyword(Keyword::Let) {
                    if is_unsafe {
                        self.error_here("extern global cannot be unsafe");
                    }
                    if let Some(global) = self.parse_extern_global(Some(abi), Some(start)) {
                        items.push(Item::ExternGlobal(global));
                    }
                } else {
                    self.error_here("expected fn or let after extern");
                    self.bump();
                }
                continue;
            }
            if self.at_keyword(Keyword::Fn) {
                if let Some(func) = self.parse_function(false, is_unsafe, None, start_override) {
                    items.push(Item::Function(func));
                }
                continue;
            }
            if self.at_keyword(Keyword::Copy) {
                if self.peek_is_keyword(Keyword::Struct) {
                    if let Some(def) = self.parse_struct_def(layout.clone()) {
                        items.push(Item::Struct(def));
                    }
                } else if self.peek_is_keyword(Keyword::Enum) {
                    if let Some(def) = self.parse_enum_def(layout.clone()) {
                        items.push(Item::Enum(def));
                    }
                } else {
                    self.error_here("expected struct or enum after copy");
                    self.bump();
                }
                continue;
            }
            if self.at_keyword(Keyword::Struct) {
                if let Some(def) = self.parse_struct_def(layout.clone()) {
                    items.push(Item::Struct(def));
                }
                continue;
            }
            if self.at_keyword(Keyword::Enum) {
                if let Some(def) = self.parse_enum_def(layout.clone()) {
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

    fn parse_function(
        &mut self,
        is_extern: bool,
        is_unsafe: bool,
        extern_abi: Option<String>,
        start_override: Option<Span>,
    ) -> Option<Function> {
        let start = start_override.unwrap_or_else(|| self.peek_span().unwrap_or(Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        }));
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
        let mut is_variadic = false;
        if !self.at_symbol(Symbol::RParen) {
            loop {
                if self.at_variadic_marker() {
                    self.consume_variadic_marker();
                    is_variadic = true;
                    break;
                }
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
                    if self.at_variadic_marker() {
                        self.consume_variadic_marker();
                        is_variadic = true;
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
        let body = if is_extern {
            if !self.at_symbol(Symbol::Semi) {
                self.error_here("expected `;` after extern fn declaration");
                return None;
            }
            let semi = self.bump().span;
            Block {
                stmts: Vec::new(),
                tail: None,
                span: semi.clone(),
            }
        } else {
            self.parse_block()?
        };
        let end = body.span.clone();
        Some(Function {
            name,
            params,
            is_variadic,
            ret_type,
            is_extern,
            is_unsafe,
            extern_abi,
            body,
            span: Span {
                start: start.start,
                end: end.end,
                line: start.line,
                column: start.column,
            },
        })
    }

    fn parse_extern_global(
        &mut self,
        extern_abi: Option<String>,
        start_override: Option<Span>,
    ) -> Option<ExternGlobal> {
        let start = start_override.unwrap_or_else(|| self.peek_span().unwrap_or(Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        }));
        self.expect_keyword(Keyword::Let);
        let name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected global name");
                return None;
            }
        };
        self.expect_symbol(Symbol::Colon);
        let ty = self.parse_type()?;
        if !self.at_symbol(Symbol::Semi) {
            self.error_here("expected `;` after extern global declaration");
            return None;
        }
        let semi = self.bump().span;
        Some(ExternGlobal {
            name,
            ty,
            extern_abi,
            span: Span {
                start: start.start,
                end: semi.end,
                line: start.line,
                column: start.column,
            },
        })
    }

    fn parse_struct_def(&mut self, mut layout: LayoutAttr) -> Option<StructDef> {
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
        let extra_layout = self.parse_layout_modifiers();
        layout.repr_c |= extra_layout.repr_c;
        layout.bitfield |= extra_layout.bitfield;
        if extra_layout.pack.is_some() {
            layout.pack = extra_layout.pack;
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
            layout,
            span: start,
        })
    }

    fn parse_enum_def(&mut self, mut layout: LayoutAttr) -> Option<EnumDef> {
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
        let extra_layout = self.parse_layout_modifiers();
        layout.repr_c |= extra_layout.repr_c;
        layout.bitfield |= extra_layout.bitfield;
        if extra_layout.pack.is_some() {
            layout.pack = extra_layout.pack;
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
            layout,
            span: start,
        })
    }

    fn parse_layout_modifiers(&mut self) -> LayoutAttr {
        let mut layout = LayoutAttr::default();
        loop {
            if self.at_ident("repr") {
                let repr_span = self.bump().span;
                self.expect_symbol(Symbol::LParen);
                let repr_name = match self.bump().kind {
                    TokenKind::Ident(name) => name,
                    TokenKind::StringLit(name) => name,
                    _ => {
                        self.error_here("expected repr name");
                        String::new()
                    }
                };
                self.expect_symbol(Symbol::RParen);
                if repr_name.eq_ignore_ascii_case("c") {
                    layout.repr_c = true;
                } else {
                    self.diags.push(
                        format!("unsupported repr `{}` (only repr(C) is supported)", repr_name),
                        Some(repr_span),
                    );
                }
                continue;
            }
            if self.at_ident("pack") {
                let pack_span = self.bump().span;
                self.expect_symbol(Symbol::LParen);
                let n = match self.bump().kind {
                    TokenKind::IntLit(v) => v.parse::<u32>().ok(),
                    _ => None,
                };
                self.expect_symbol(Symbol::RParen);
                match n {
                    Some(v) if v > 0 => layout.pack = Some(v),
                    _ => self
                        .diags
                        .push("pack(N) expects positive integer N", Some(pack_span)),
                }
                continue;
            }
            if self.at_ident("bitfield") {
                self.bump();
                layout.bitfield = true;
                continue;
            }
            break;
        }
        layout
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
            if self.at_label_stmt_start() {
                if let Some(stmt) = self.parse_label_stmt() {
                    stmts.push(stmt);
                    continue;
                }
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
            if self.at_keyword(Keyword::While) {
                let stmt = self.parse_while_stmt()?;
                stmts.push(stmt);
                continue;
            }
            if self.at_keyword(Keyword::Loop) {
                let stmt = self.parse_loop_stmt()?;
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
            if !self.at_symbol(Symbol::RBrace)
                && matches!(
                    expr.kind,
                    ExprKind::If { .. }
                        | ExprKind::Match { .. }
                        | ExprKind::Block(_)
                        | ExprKind::UnsafeBlock(_)
                )
            {
                let span = expr.span.clone();
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

    fn at_label_stmt_start(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Ident(_))
            && matches!(
                self.tokens.get(self.idx + 1).map(|t| &t.kind),
                Some(TokenKind::Symbol(Symbol::Colon))
            )
    }

    fn parse_label_stmt(&mut self) -> Option<Stmt> {
        let start = self.peek_span().unwrap_or(Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        });
        let label_name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected label name");
                return None;
            }
        };
        self.expect_symbol(Symbol::Colon);
        let callee = self.new_expr(ExprKind::Ident("asm_label".to_string()), start.clone());
        let label_arg = self.new_expr(ExprKind::String(label_name), start.clone());
        let expr = self.new_expr(
            ExprKind::Call {
                callee: Box::new(callee),
                type_args: Vec::new(),
                args: vec![label_arg],
            },
            start.clone(),
        );
        Some(Stmt::Expr { expr, span: start })
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
        let iter = self.parse_expr_no_struct_lit()?;
        let body = self.parse_block()?;
        Some(Stmt::ForIn {
            name,
            iter,
            body,
            span,
        })
    }

    fn parse_while_stmt(&mut self) -> Option<Stmt> {
        let span = self.bump().span;
        let cond = self.parse_expr_no_struct_lit()?;
        let body = self.parse_block()?;
        Some(Stmt::While { cond, body, span })
    }

    fn parse_loop_stmt(&mut self) -> Option<Stmt> {
        let span = self.bump().span;
        let body = self.parse_block()?;
        Some(Stmt::Loop { body, span })
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
            if self.at_symbol(Symbol::PipeGt) {
                let prec = 5u8;
                if prec < min_prec {
                    break;
                }
                let span = self.bump().span;
                let right = self.parse_binary_expr(prec + 1)?;
                left = self.build_pipe_expr(left, right, span)?;
                continue;
            }
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

    fn build_pipe_expr(&mut self, lhs: Expr, rhs: Expr, span: Span) -> Option<Expr> {
        match rhs.kind {
            ExprKind::Call {
                callee,
                type_args,
                mut args,
            } => {
                let mut new_args = Vec::with_capacity(args.len() + 1);
                new_args.push(lhs);
                new_args.append(&mut args);
                Some(self.new_expr(
                    ExprKind::Call {
                        callee,
                        type_args,
                        args: new_args,
                    },
                    span,
                ))
            }
            _ => Some(self.new_expr(
                ExprKind::Call {
                    callee: Box::new(rhs),
                    type_args: Vec::new(),
                    args: vec![lhs],
                },
                span,
            )),
        }
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
        if self.at_ident("asm")
            && (self.peek_is_ident("volatile") || self.peek_is_ident("goto"))
        {
            return self.parse_colon_inline_asm_expr();
        }
        if self.at_keyword(Keyword::Unsafe) {
            self.bump();
            if !self.at_symbol(Symbol::LBrace) {
                self.error_here("expected block after unsafe");
                return None;
            }
            let block = self.parse_block()?;
            return Some(self.new_expr(
                ExprKind::UnsafeBlock(Box::new(block.clone())),
                block.span,
            ));
        }
        if self.at_symbol(Symbol::LBrace) {
            let block = self.parse_block()?;
            return Some(self.new_expr(
                ExprKind::Block(Box::new(block.clone())),
                block.span,
            ));
        }
        if self.at_keyword(Keyword::If) {
            self.bump();
            let cond = self.parse_expr_no_struct_lit()?;
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
            let scrutinee = self.parse_expr_no_struct_lit()?;
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
            TokenKind::Ident(name) => {
                if self.allow_struct_lit && self.at_symbol(Symbol::LBrace) {
                    return self.parse_struct_lit(name, span);
                }
                Some(self.new_expr(ExprKind::Ident(name), span))
            }
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

    fn parse_colon_inline_asm_expr(&mut self) -> Option<Expr> {
        let start = self.peek_span().unwrap_or(Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        });
        if !self.at_ident("asm") {
            self.error_here("expected `asm`");
            return None;
        }
        self.bump();

        let mut is_volatile = false;
        let mut is_goto = false;
        let mut saw_qual = false;
        loop {
            if self.at_ident("volatile") {
                is_volatile = true;
                saw_qual = true;
                self.bump();
                continue;
            }
            if self.at_ident("goto") {
                is_goto = true;
                saw_qual = true;
                self.bump();
                continue;
            }
            break;
        }
        if !saw_qual {
            self.error_here("expected `volatile` or `goto` after asm");
            return None;
        }

        self.expect_symbol(Symbol::LParen);
        let template_raw = match self.parse_string_lit() {
            Some(s) => s,
            None => {
                self.error_here("asm template must be string literal");
                return None;
            }
        };

        let mut outputs: Vec<(Option<String>, String, Expr)> = Vec::new();
        let mut inputs: Vec<(Option<String>, String, Expr)> = Vec::new();
        let mut clobbers: Vec<String> = Vec::new();
        let mut labels: Vec<String> = Vec::new();

        if self.at_symbol(Symbol::Colon) {
            self.bump();
            outputs = self.parse_colon_asm_operand_section()?;
            if self.at_symbol(Symbol::Colon) {
                self.bump();
                inputs = self.parse_colon_asm_operand_section()?;
            }
            if self.at_symbol(Symbol::Colon) {
                self.bump();
                clobbers = self.parse_colon_asm_clobber_section()?;
            }
            if self.at_symbol(Symbol::Colon) {
                self.bump();
                labels = self.parse_colon_asm_label_section()?;
            }
        }

        self.expect_symbol(Symbol::RParen);

        let mut operand_names: Vec<Option<String>> = Vec::with_capacity(outputs.len() + inputs.len());
        for (name, _, _) in &outputs {
            operand_names.push(name.clone());
        }
        for (name, _, _) in &inputs {
            operand_names.push(name.clone());
        }
        let operand_count = operand_names.len();
        let rewritten_template = rewrite_gcc_asm_template(
            &template_raw,
            &labels,
            &operand_names,
            operand_count,
        );
        let mut constraints =
            build_asm_constraints(&outputs, &inputs, &clobbers);
        if is_goto || !labels.is_empty() {
            for _ in 0..labels.len() {
                if !constraints.is_empty() {
                    constraints.push(',');
                }
                constraints.push_str("!i");
            }
            let mut args = Vec::new();
            args.push(self.new_expr(
                ExprKind::String(rewritten_template),
                start.clone(),
            ));
            args.push(self.new_expr(
                ExprKind::String(constraints),
                start.clone(),
            ));
            args.push(self.new_expr(
                ExprKind::String(labels.join(",")),
                start.clone(),
            ));
            for (_, _, expr) in &outputs {
                args.push(expr.clone());
            }
            for (_, _, expr) in &inputs {
                args.push(expr.clone());
            }
            let callee = self.new_expr(ExprKind::Ident("asm_goto".to_string()), start.clone());
            let call_expr = self.new_expr(
                ExprKind::Call {
                    callee: Box::new(callee),
                    type_args: Vec::new(),
                    args,
                },
                start.clone(),
            );
            return Some(self.lower_colon_asm_outputs(call_expr, &outputs, &start));
        }

        let mut call_args = Vec::new();
        call_args.push(self.new_expr(
            ExprKind::String(rewritten_template),
            start.clone(),
        ));
        call_args.push(self.new_expr(
            ExprKind::String(constraints),
            start.clone(),
        ));
        for (_, _, expr) in &outputs {
            call_args.push(expr.clone());
        }
        for (_, _, expr) in &inputs {
            call_args.push(expr.clone());
        }
        let callee_name = if is_volatile {
            "asm_volatile"
        } else {
            "asm"
        };
        let callee = self.new_expr(ExprKind::Ident(callee_name.to_string()), start.clone());

        if outputs.is_empty() {
            let unit_type = TypeAst {
                kind: TypeAstKind::Named("unit".to_string()),
                span: start.clone(),
            };
            return Some(self.new_expr(
                ExprKind::Call {
                    callee: Box::new(callee),
                    type_args: vec![unit_type],
                    args: call_args,
                },
                start,
            ));
        }
        let call_expr = self.new_expr(
            ExprKind::Call {
                callee: Box::new(callee),
                type_args: Vec::new(),
                args: call_args,
            },
            start.clone(),
        );
        Some(self.lower_colon_asm_outputs(call_expr, &outputs, &start))
    }

    fn lower_colon_asm_outputs(
        &mut self,
        call_expr: Expr,
        outputs: &[(Option<String>, String, Expr)],
        span: &Span,
    ) -> Expr {
        if outputs.is_empty() {
            return call_expr;
        }
        if outputs.len() == 1 {
            let assign = Stmt::Assign {
                target: outputs[0].2.clone(),
                value: call_expr,
                span: span.clone(),
            };
            let block = Block {
                stmts: vec![assign],
                tail: None,
                span: span.clone(),
            };
            return self.new_expr(ExprKind::Block(Box::new(block)), span.clone());
        }
        let tmp_name = format!("__asm_out_{}_{}", span.start, self.next_expr_id);
        let mut stmts = vec![Stmt::Let {
            name: tmp_name.clone(),
            ty: None,
            init: call_expr,
            span: span.clone(),
        }];
        for (idx, (_, _, target)) in outputs.iter().enumerate() {
            let base = self.new_expr(ExprKind::Ident(tmp_name.clone()), span.clone());
            let index = self.new_expr(ExprKind::Int(idx.to_string()), span.clone());
            let value = self.new_expr(
                ExprKind::Index {
                    base: Box::new(base),
                    index: Box::new(index),
                },
                span.clone(),
            );
            stmts.push(Stmt::Assign {
                target: target.clone(),
                value,
                span: span.clone(),
            });
        }
        let block = Block {
            stmts,
            tail: None,
            span: span.clone(),
        };
        self.new_expr(ExprKind::Block(Box::new(block)), span.clone())
    }

    fn parse_colon_asm_operand_section(&mut self) -> Option<Vec<(Option<String>, String, Expr)>> {
        let mut items = Vec::new();
        if self.at_symbol(Symbol::Colon) || self.at_symbol(Symbol::RParen) {
            return Some(items);
        }
        loop {
            let mut name: Option<String> = None;
            if self.at_symbol(Symbol::LBracket) {
                self.bump();
                match self.bump().kind {
                    TokenKind::Ident(s) => name = Some(s),
                    _ => {
                        self.error_here("expected asm operand name");
                        return None;
                    }
                }
                self.expect_symbol(Symbol::RBracket);
            }
            let constraint = match self.parse_string_lit() {
                Some(s) => s,
                None => {
                    self.error_here("expected asm operand constraint string");
                    return None;
                }
            };
            self.expect_symbol(Symbol::LParen);
            let expr = self.parse_expr()?;
            self.expect_symbol(Symbol::RParen);
            items.push((name, constraint, expr));
            if self.at_symbol(Symbol::Comma) {
                self.bump();
                if self.at_symbol(Symbol::Colon) || self.at_symbol(Symbol::RParen) {
                    break;
                }
            } else {
                break;
            }
        }
        Some(items)
    }

    fn parse_colon_asm_clobber_section(&mut self) -> Option<Vec<String>> {
        let mut out = Vec::new();
        if self.at_symbol(Symbol::Colon) || self.at_symbol(Symbol::RParen) {
            return Some(out);
        }
        loop {
            let clobber = match self.parse_string_lit() {
                Some(s) => s,
                None => {
                    self.error_here("expected asm clobber string");
                    return None;
                }
            };
            out.push(clobber);
            if self.at_symbol(Symbol::Comma) {
                self.bump();
                if self.at_symbol(Symbol::Colon) || self.at_symbol(Symbol::RParen) {
                    break;
                }
            } else {
                break;
            }
        }
        Some(out)
    }

    fn parse_colon_asm_label_section(&mut self) -> Option<Vec<String>> {
        let mut out = Vec::new();
        if self.at_symbol(Symbol::RParen) {
            return Some(out);
        }
        loop {
            let label = match self.bump().kind {
                TokenKind::Ident(s) => s,
                _ => {
                    self.error_here("expected asm goto label");
                    return None;
                }
            };
            out.push(label);
            if self.at_symbol(Symbol::Comma) {
                self.bump();
                if self.at_symbol(Symbol::RParen) {
                    break;
                }
            } else {
                break;
            }
        }
        Some(out)
    }

    fn parse_struct_lit(&mut self, name: String, start_span: Span) -> Option<Expr> {
        let _lbrace = self.expect_symbol(Symbol::LBrace);
        let mut fields: Vec<(String, Expr)> = Vec::new();
        while !self.at_symbol(Symbol::RBrace) && !self.at_eof() {
            let field_name = match self.bump().kind {
                TokenKind::Ident(n) => n,
                _ => {
                    self.error_here("expected field name");
                    return None;
                }
            };
            self.expect_symbol(Symbol::Eq);
            let expr = self.parse_expr()?;
            fields.push((field_name, expr));
            if self.at_symbol(Symbol::Comma) {
                self.bump();
                if self.at_symbol(Symbol::RBrace) {
                    break;
                }
            } else {
                break;
            }
        }
        let rbrace_span = self.peek_span().unwrap_or(start_span.clone());
        self.expect_symbol(Symbol::RBrace);
        let span = Span {
            start: start_span.start,
            end: rbrace_span.end,
            line: start_span.line,
            column: start_span.column,
        };
        Some(self.new_expr(
            ExprKind::StructLit { name, fields },
            span,
        ))
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
        if self.at_keyword(Keyword::Fn) {
            self.bump();
            self.expect_symbol(Symbol::LParen);
            let mut params = Vec::new();
            let mut is_variadic = false;
            if !self.at_symbol(Symbol::RParen) {
                loop {
                    if self.at_variadic_marker() {
                        self.consume_variadic_marker();
                        is_variadic = true;
                        break;
                    }
                    params.push(self.parse_type()?);
                    if self.at_symbol(Symbol::Comma) {
                        self.bump();
                        if self.at_symbol(Symbol::RParen) {
                            break;
                        }
                        if self.at_variadic_marker() {
                            self.consume_variadic_marker();
                            is_variadic = true;
                            break;
                        }
                    } else {
                        break;
                    }
                }
            }
            self.expect_symbol(Symbol::RParen);
            let ret = if self.at_symbol(Symbol::Arrow) {
                self.bump();
                self.parse_type()?
            } else {
                TypeAst {
                    kind: TypeAstKind::Named("unit".to_string()),
                    span: span.clone(),
                }
            };
            return Some(TypeAst {
                kind: TypeAstKind::FnPtr {
                    params,
                    ret: Box::new(ret),
                    is_variadic,
                },
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
        if self.at_ident("Result") || self.at_ident("result") {
            self.bump();
            self.expect_symbol(Symbol::LBracket);
            let ok = self.parse_type()?;
            self.expect_symbol(Symbol::Comma);
            let err = self.parse_type()?;
            self.expect_symbol(Symbol::RBracket);
            return Some(TypeAst {
                kind: TypeAstKind::Result(Box::new(ok), Box::new(err)),
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

    fn peek_is_ident(&self, name: &str) -> bool {
        matches!(
            self.tokens.get(self.idx + 1).map(|t| &t.kind),
            Some(TokenKind::Ident(s)) if s == name
        )
    }

    fn at_ident(&self, name: &str) -> bool {
        matches!(self.peek().kind, TokenKind::Ident(ref s) if s == name)
    }

    fn at_variadic_marker(&self) -> bool {
        matches!(
            self.tokens.get(self.idx).map(|t| &t.kind),
            Some(TokenKind::Symbol(Symbol::Dot))
        ) && matches!(
            self.tokens.get(self.idx + 1).map(|t| &t.kind),
            Some(TokenKind::Symbol(Symbol::Dot))
        ) && matches!(
            self.tokens.get(self.idx + 2).map(|t| &t.kind),
            Some(TokenKind::Symbol(Symbol::Dot))
        )
    }

    fn consume_variadic_marker(&mut self) {
        if self.at_variadic_marker() {
            self.bump();
            self.bump();
            self.bump();
        } else {
            self.error_here("expected `...`");
        }
    }

    fn is_builtin_generic_callee(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Ident(name) => matches!(
                name.as_str(),
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
                    | "asm"
                    | "asm_pure"
                    | "asm_volatile"
            ),
            ExprKind::Field { base, name } => {
                if let ExprKind::Ident(base_name) = &base.kind {
                    (base_name == "Result" || base_name == "result")
                        && (name == "Ok" || name == "Err" || name == "ok" || name == "err")
                } else {
                    false
                }
            }
            _ => false,
        }
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
            TokenKind::Symbol(Symbol::Plus) => (6, BinaryOp::Add),
            TokenKind::Symbol(Symbol::Minus) => (6, BinaryOp::Sub),
            TokenKind::Symbol(Symbol::Star) => (7, BinaryOp::Mul),
            TokenKind::Symbol(Symbol::Slash) => (7, BinaryOp::Div),
            TokenKind::Symbol(Symbol::Percent) => (7, BinaryOp::Rem),
            _ => return None,
        };
        Some(op)
    }
}

fn normalize_clobber_constraint(clobber: &str) -> String {
    let mut raw = clobber.trim().to_string();
    if raw.starts_with("~{") && raw.ends_with('}') {
        return raw;
    }
    if raw.starts_with('%') {
        raw = raw[1..].to_string();
    }
    format!("~{{{}}}", raw)
}

fn build_asm_constraints(
    outputs: &[(Option<String>, String, Expr)],
    inputs: &[(Option<String>, String, Expr)],
    clobbers: &[String],
) -> String {
    let mut parts: Vec<String> = Vec::new();
    for (_, c, _) in outputs {
        parts.push(c.clone());
    }
    for (_, c, _) in inputs {
        parts.push(c.clone());
    }
    for c in clobbers {
        parts.push(normalize_clobber_constraint(c));
    }
    parts.join(",")
}

fn rewrite_gcc_asm_template(
    template: &str,
    labels: &[String],
    operand_names: &[Option<String>],
    operand_count: usize,
) -> String {
    let mut out = String::new();
    let chars: Vec<char> = template.chars().collect();
    let mut i = 0usize;
    while i < chars.len() {
        if chars[i] != '%' {
            out.push(chars[i]);
            i += 1;
            continue;
        }
        if i + 1 >= chars.len() {
            out.push('%');
            break;
        }
        let next = chars[i + 1];
        if next == '%' {
            out.push('%');
            i += 2;
            continue;
        }
        if next == 'l' {
            let mut j = i + 2;
            if j < chars.len() && chars[j] == '[' {
                j += 1;
                let name_start = j;
                while j < chars.len() && chars[j] != ']' {
                    j += 1;
                }
                if j < chars.len() && chars[j] == ']' {
                    let name: String = chars[name_start..j].iter().collect();
                    if let Some(idx) = labels.iter().position(|s| s == &name) {
                        out.push_str(&format!("${{{}:l}}", operand_count + idx));
                    } else {
                        out.push_str(&format!("%l[{}]", name));
                    }
                    i = j + 1;
                    continue;
                }
            }
            let digit_start = j;
            while j < chars.len() && chars[j].is_ascii_digit() {
                j += 1;
            }
            if j > digit_start {
                let num: String = chars[digit_start..j].iter().collect();
                let idx = num.parse::<usize>().unwrap_or(0);
                out.push_str(&format!("${{{}:l}}", operand_count + idx));
                i = j;
                continue;
            }
        }
        if next == '[' {
            let mut j = i + 2;
            let name_start = j;
            while j < chars.len() && chars[j] != ']' {
                j += 1;
            }
            if j < chars.len() && chars[j] == ']' {
                let name: String = chars[name_start..j].iter().collect();
                if let Some(idx) = operand_names
                    .iter()
                    .position(|opt| opt.as_deref() == Some(name.as_str()))
                {
                    out.push('$');
                    out.push_str(&idx.to_string());
                    i = j + 1;
                    continue;
                }
            }
        }
        if next.is_ascii_digit() {
            let mut j = i + 1;
            while j < chars.len() && chars[j].is_ascii_digit() {
                j += 1;
            }
            let num: String = chars[i + 1..j].iter().collect();
            out.push('$');
            out.push_str(&num);
            i = j;
            continue;
        }
        out.push('%');
        i += 1;
    }
    out
}
