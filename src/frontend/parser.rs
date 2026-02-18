use super::ast::*;
use super::diagnostic::Diagnostics;
use super::lexer::{Keyword, Lexer, Symbol, Token, TokenKind};
use super::symbols::{mangle_impl_method, module_symbol_key};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, PartialEq, Eq)]
enum ParsedRepr {
    C,
    Transparent,
    Int(ReprInt),
    Other(String),
}

#[derive(Clone, Debug)]
struct ImplTraitRecord {
    trait_name: String,
    recv_name: String,
    methods: Vec<TraitMethod>,
    span: Span,
}

pub struct Parser {
    tokens: Vec<Token>,
    idx: usize,
    pub diags: Diagnostics,
    next_expr_id: ExprId,
    allow_struct_lit: bool,
    current_package: String,
    module_disambiguator: String,
    impl_trait_records: Vec<ImplTraitRecord>,
}

impl Parser {
    fn parse_callable_type(&mut self, span: Span, is_closure: bool) -> Option<TypeAst> {
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
        let kind = if is_closure {
            TypeAstKind::Closure {
                params,
                ret: Box::new(ret),
                is_variadic,
            }
        } else {
            TypeAstKind::FnPtr {
                params,
                ret: Box::new(ret),
                is_variadic,
            }
        };
        Some(TypeAst { kind, span })
    }

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
            current_package: String::new(),
            module_disambiguator: String::new(),
            impl_trait_records: Vec::new(),
        }
    }

    pub fn next_expr_id(&self) -> ExprId {
        self.next_expr_id
    }

    pub fn set_module_disambiguator(&mut self, value: impl Into<String>) {
        self.module_disambiguator = value.into();
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
        let package = self.parse_package()?;
        self.current_package = package.clone();
        self.impl_trait_records.clear();
        self.consume_semis();
        let mut imports = Vec::new();
        while self.at_keyword(Keyword::Import) {
            if let Some(spec) = self.parse_import_spec() {
                imports.push(spec);
            }
            self.consume_semis();
        }
        let mut items = Vec::new();
        while !self.at_eof() {
            if self.at_symbol(Symbol::Semi) {
                self.bump();
                continue;
            }
            let vis = self.parse_item_visibility();
            let layout = self.parse_layout_modifiers();
            self.consume_semis();
            let has_layout = layout.has_any();
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
                if !self.at_keyword(Keyword::Extern)
                    && !self.at_keyword(Keyword::Fn)
                    && !self.at_keyword(Keyword::Impl)
                {
                    self.diags
                        .push("expected fn, extern, or impl after unsafe", Some(start));
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
                        self.parse_function(vis, true, is_unsafe, Some(abi), Some(start))
                    {
                        items.push(Item::Function(func));
                    }
                } else if self.at_keyword(Keyword::Let) {
                    if is_unsafe {
                        self.error_here("extern global cannot be unsafe");
                    }
                    if let Some(global) = self.parse_extern_global(vis, Some(abi), Some(start)) {
                        items.push(Item::ExternGlobal(global));
                    }
                } else {
                    self.error_here("expected fn or let after extern");
                    self.bump();
                }
                continue;
            }
            if self.at_keyword(Keyword::Fn) {
                if let Some(func) = self.parse_function(vis, false, is_unsafe, None, start_override)
                {
                    items.push(Item::Function(func));
                }
                continue;
            }
            if self.at_keyword(Keyword::Const) {
                if let Some(c) = self.parse_const_item(vis, start_override) {
                    items.push(Item::Const(c));
                }
                continue;
            }
            if self.at_keyword(Keyword::Type) {
                if let Some(alias) = self.parse_type_alias(vis, start_override) {
                    items.push(Item::TypeAlias(alias));
                }
                continue;
            }
            if self.at_keyword(Keyword::Trait) {
                if let Some(alias) = self.parse_trait_alias_item(vis, start_override) {
                    items.push(Item::TypeAlias(alias));
                }
                continue;
            }
            if self.at_keyword(Keyword::Impl) {
                if let Some(methods) = self.parse_impl_block(vis, start_override) {
                    items.extend(methods);
                }
                continue;
            }
            if self.at_keyword(Keyword::Let) {
                if let Some(global) = self.parse_global_var(vis, start_override) {
                    items.push(Item::Global(global));
                }
                continue;
            }
            if self.at_keyword(Keyword::Copy) {
                if self.peek_is_keyword(Keyword::Struct) {
                    if let Some(def) = self.parse_struct_def(vis, layout.clone()) {
                        items.push(Item::Struct(def));
                    }
                } else if self.peek_is_keyword(Keyword::Enum) {
                    if let Some(def) = self.parse_enum_def(vis, layout.clone()) {
                        items.push(Item::Enum(def));
                    }
                } else {
                    self.error_here("expected struct or enum after copy");
                    self.bump();
                }
                continue;
            }
            if self.at_keyword(Keyword::Struct) {
                if let Some(def) = self.parse_struct_def(vis, layout.clone()) {
                    items.push(Item::Struct(def));
                }
                continue;
            }
            if self.at_keyword(Keyword::Enum) {
                if let Some(def) = self.parse_enum_def(vis, layout.clone()) {
                    items.push(Item::Enum(def));
                }
                continue;
            }
            self.error_here("expected item");
            self.bump();
        }
        self.synthesize_trait_default_impl_methods(&mut items);
        self.validate_trait_impl_records(&items);
        self.validate_type_param_bounds(&items);
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

    fn parse_item_visibility(&mut self) -> Visibility {
        if self.at_keyword(Keyword::Pub) {
            self.bump();
            Visibility::Public
        } else if self.at_keyword(Keyword::Private) {
            self.bump();
            Visibility::Private
        } else {
            Visibility::Public
        }
    }

    fn parse_import_spec(&mut self) -> Option<ImportSpec> {
        self.expect_keyword(Keyword::Import);
        let path = match self.parse_string_lit() {
            Some(s) => s,
            None => {
                self.diags
                    .push("expected string literal after import", self.peek_span());
                return None;
            }
        };
        let alias = if self.at_keyword(Keyword::As) {
            self.bump();
            match self.bump().kind {
                TokenKind::Ident(name) => Some(name),
                _ => {
                    self.error_here("expected alias identifier after `as`");
                    None
                }
            }
        } else {
            None
        };
        let only = if self.at_symbol(Symbol::LBrace) {
            self.bump();
            let mut names = Vec::new();
            if !self.at_symbol(Symbol::RBrace) {
                loop {
                    match self.bump().kind {
                        TokenKind::Ident(name) => names.push(name),
                        _ => {
                            self.error_here("expected imported item name");
                            break;
                        }
                    }
                    if self.at_symbol(Symbol::Comma) {
                        self.bump();
                        if self.at_symbol(Symbol::RBrace) {
                            break;
                        }
                        continue;
                    }
                    break;
                }
            }
            self.expect_symbol(Symbol::RBrace);
            Some(names)
        } else {
            None
        };
        Some(ImportSpec { path, alias, only })
    }

    fn parse_function(
        &mut self,
        vis: Visibility,
        is_extern: bool,
        is_unsafe: bool,
        extern_abi: Option<String>,
        start_override: Option<Span>,
    ) -> Option<Function> {
        let start = start_override.unwrap_or_else(|| {
            self.peek_span().unwrap_or(Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            })
        });
        self.expect_keyword(Keyword::Fn);
        let name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected function name");
                return None;
            }
        };
        let type_params = self.parse_type_param_names()?;
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
            vis,
            name,
            type_params,
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
        vis: Visibility,
        extern_abi: Option<String>,
        start_override: Option<Span>,
    ) -> Option<ExternGlobal> {
        let start = start_override.unwrap_or_else(|| {
            self.peek_span().unwrap_or(Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            })
        });
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
            vis,
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

    fn parse_type_alias(
        &mut self,
        vis: Visibility,
        start_override: Option<Span>,
    ) -> Option<TypeAlias> {
        let start = start_override.unwrap_or_else(|| {
            self.peek_span().unwrap_or(Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            })
        });
        self.expect_keyword(Keyword::Type);
        let name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected alias name");
                return None;
            }
        };
        self.expect_symbol(Symbol::Eq);
        let ty = self.parse_type()?;
        if !self.at_symbol(Symbol::Semi) {
            self.error_here("expected `;` after type alias");
            return None;
        }
        let semi = self.bump().span;
        Some(TypeAlias {
            vis,
            name,
            ty,
            is_trait: false,
            trait_methods: Vec::new(),
            span: Span {
                start: start.start,
                end: semi.end,
                line: start.line,
                column: start.column,
            },
        })
    }

    fn parse_trait_alias_item(
        &mut self,
        vis: Visibility,
        start_override: Option<Span>,
    ) -> Option<TypeAlias> {
        let start = start_override.unwrap_or_else(|| {
            self.peek_span().unwrap_or(Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            })
        });
        self.expect_keyword(Keyword::Trait);
        let name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected trait name");
                return None;
            }
        };
        let methods = self.parse_trait_method_list();
        if self.at_symbol(Symbol::Semi) {
            self.bump();
        }
        Some(TypeAlias {
            vis,
            name,
            ty: TypeAst {
                kind: TypeAstKind::Interface,
                span: start.clone(),
            },
            is_trait: true,
            trait_methods: methods,
            span: start,
        })
    }

    fn parse_impl_block(
        &mut self,
        default_vis: Visibility,
        start_override: Option<Span>,
    ) -> Option<Vec<Item>> {
        let start = start_override.unwrap_or_else(|| {
            self.peek_span().unwrap_or(Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            })
        });
        self.expect_keyword(Keyword::Impl);
        let head_name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected impl target type name");
                return None;
            }
        };
        let mut trait_name: Option<String> = None;
        let recv_name = if self.at_keyword(Keyword::For) {
            self.bump();
            trait_name = Some(head_name);
            match self.bump().kind {
                TokenKind::Ident(name) => name,
                _ => {
                    self.error_here("expected type name after `for`");
                    return None;
                }
            }
        } else {
            head_name
        };
        let recv_ty = TypeAst {
            kind: TypeAstKind::Named(recv_name.clone()),
            span: start.clone(),
        };
        self.expect_symbol(Symbol::LBrace);
        let mut out = Vec::new();
        let mut impl_methods = Vec::new();
        while !self.at_symbol(Symbol::RBrace) && !self.at_eof() {
            self.consume_semis();
            if self.at_symbol(Symbol::RBrace) {
                break;
            }
            let vis = if self.at_keyword(Keyword::Pub) || self.at_keyword(Keyword::Private) {
                self.parse_item_visibility()
            } else {
                default_vis
            };
            let mut is_unsafe = false;
            if self.at_keyword(Keyword::Unsafe) {
                self.bump();
                is_unsafe = true;
            }
            if !self.at_keyword(Keyword::Fn) {
                self.error_here("expected `fn` inside impl block");
                self.bump();
                continue;
            }
            let mut func = self.parse_function(vis, false, is_unsafe, None, None)?;
            if func.params.first().map(|p| p.name.as_str()) != Some("self") {
                func.params.insert(
                    0,
                    Param {
                        name: "self".to_string(),
                        ty: recv_ty.clone(),
                        span: func.span.clone(),
                    },
                );
            }
            if trait_name.is_some() {
                impl_methods.push(self.canonical_method_sig(&func));
            }
            let module_key = module_symbol_key(&self.current_package, &self.module_disambiguator);
            func.name = mangle_impl_method(&module_key, &recv_name, &func.name);
            out.push(Item::Function(func));
            self.consume_semis();
        }
        self.expect_symbol(Symbol::RBrace);
        if let Some(trait_name) = trait_name {
            self.impl_trait_records.push(ImplTraitRecord {
                trait_name,
                recv_name,
                methods: impl_methods,
                span: start,
            });
        }
        Some(out)
    }

    fn parse_trait_method_list(&mut self) -> Vec<TraitMethod> {
        let mut methods = Vec::new();
        if !self.at_symbol(Symbol::LBrace) {
            return methods;
        }
        let trait_span = self.peek_span().unwrap_or(Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        });
        self.bump();
        while !self.at_symbol(Symbol::RBrace) && !self.at_eof() {
            self.consume_semis();
            if self.at_symbol(Symbol::RBrace) {
                break;
            }
            if self.at_keyword(Keyword::Pub) || self.at_keyword(Keyword::Private) {
                self.parse_item_visibility();
            }
            if !self.at_keyword(Keyword::Fn) {
                self.error_here("expected `fn` inside trait body");
                self.bump();
                continue;
            }
            let method_start = self.peek_span().unwrap_or(trait_span.clone());
            self.bump();
            let method_name = match self.bump().kind {
                TokenKind::Ident(name) => name,
                _ => {
                    self.error_here("expected trait method name");
                    break;
                }
            };
            let type_params = match self.parse_type_param_names() {
                Some(v) => v,
                None => break,
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
                    let param_start = self.peek_span().unwrap_or(method_start.clone());
                    let param_name = match self.bump().kind {
                        TokenKind::Ident(name) => name,
                        _ => {
                            self.error_here("expected parameter name");
                            break;
                        }
                    };
                    self.expect_symbol(Symbol::Colon);
                    let Some(ty) = self.parse_type() else {
                        break;
                    };
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
                match self.parse_type() {
                    Some(v) => v,
                    None => Self::unit_type_ast(&method_start),
                }
            } else {
                Self::unit_type_ast(&method_start)
            };
            let default_body = if self.at_symbol(Symbol::LBrace) {
                let Some(block) = self.parse_block() else {
                    break;
                };
                Some(block)
            } else {
                None
            };
            if self.at_symbol(Symbol::Semi) {
                self.bump();
            }
            methods.push(TraitMethod {
                name: method_name,
                type_params,
                params: Self::canonical_method_params(params),
                is_variadic,
                ret_type,
                default_body,
                span: method_start,
            });
            self.consume_semis();
        }
        self.expect_symbol(Symbol::RBrace);
        methods
    }

    fn canonical_method_sig(&self, func: &Function) -> TraitMethod {
        TraitMethod {
            name: func.name.clone(),
            type_params: func.type_params.clone(),
            params: Self::canonical_method_params(func.params.clone()),
            is_variadic: func.is_variadic,
            ret_type: func
                .ret_type
                .clone()
                .unwrap_or_else(|| Self::unit_type_ast(&func.span)),
            default_body: None,
            span: func.span.clone(),
        }
    }

    fn canonical_method_params(mut params: Vec<Param>) -> Vec<Param> {
        if params.first().map(|p| p.name.as_str()) == Some("self") {
            params.remove(0);
        }
        params
    }

    fn unit_type_ast(span: &Span) -> TypeAst {
        TypeAst {
            kind: TypeAstKind::Named("unit".to_string()),
            span: span.clone(),
        }
    }

    fn validate_trait_impl_records(&mut self, items: &[Item]) {
        if self.impl_trait_records.is_empty() {
            return;
        }
        let mut trait_map: HashMap<String, Vec<TraitMethod>> = HashMap::new();
        for item in items {
            if let Item::TypeAlias(alias) = item
                && alias.is_trait
            {
                trait_map.insert(alias.name.clone(), alias.trait_methods.clone());
            }
        }
        for record in &self.impl_trait_records {
            let Some(required_methods) = trait_map.get(&record.trait_name) else {
                self.diags.push(
                    format!(
                        "unknown trait `{}` in `impl {} for {}`",
                        record.trait_name, record.trait_name, record.recv_name
                    ),
                    Some(record.span.clone()),
                );
                continue;
            };
            let mut provided_map: HashMap<&str, &TraitMethod> = HashMap::new();
            for method in &record.methods {
                if provided_map.insert(method.name.as_str(), method).is_some() {
                    self.diags.push(
                        format!(
                            "duplicate method `{}` in `impl {} for {}`",
                            method.name, record.trait_name, record.recv_name
                        ),
                        Some(method.span.clone()),
                    );
                }
            }
            for required in required_methods {
                let Some(provided) = provided_map.get(required.name.as_str()) else {
                    if required.default_body.is_none() {
                        self.diags.push(
                            format!(
                                "missing method `{}` in `impl {} for {}`",
                                required.name, record.trait_name, record.recv_name
                            ),
                            Some(record.span.clone()),
                        );
                    }
                    continue;
                };
                if !self.trait_method_signature_eq(required, provided) {
                    self.diags.push(
                        format!(
                            "method `{}` signature mismatch in `impl {} for {}`",
                            required.name, record.trait_name, record.recv_name
                        ),
                        Some(provided.span.clone()),
                    );
                }
            }
            for provided in &record.methods {
                if !required_methods.iter().any(|m| m.name == provided.name) {
                    self.diags.push(
                        format!(
                            "method `{}` is not declared in trait `{}`",
                            provided.name, record.trait_name
                        ),
                        Some(provided.span.clone()),
                    );
                }
            }
        }
    }

    fn validate_type_param_bounds(&mut self, items: &[Item]) {
        let mut trait_names: HashSet<String> = HashSet::new();
        for item in items {
            if let Item::TypeAlias(alias) = item
                && alias.is_trait
            {
                trait_names.insert(alias.name.clone());
            }
        }
        let mut validate_bounds = |type_params: &[TypeParam]| {
            for type_param in type_params {
                for bound in &type_param.bounds {
                    match &bound.kind {
                        TypeAstKind::Named(name)
                            if name == "interface" || trait_names.contains(name) => {}
                        TypeAstKind::Named(name) => {
                            self.diags.push(
                                format!(
                                    "unknown generic bound `{}` on type parameter `{}`",
                                    name, type_param.name
                                ),
                                Some(bound.span.clone()),
                            );
                        }
                        _ => {
                            self.diags.push(
                                format!(
                                    "generic bound on `{}` must be a trait name",
                                    type_param.name
                                ),
                                Some(bound.span.clone()),
                            );
                        }
                    }
                }
            }
        };
        for item in items {
            match item {
                Item::Function(func) => validate_bounds(&func.type_params),
                Item::TypeAlias(alias) if alias.is_trait => {
                    for method in &alias.trait_methods {
                        validate_bounds(&method.type_params);
                    }
                }
                _ => {}
            }
        }
    }

    fn synthesize_trait_default_impl_methods(&mut self, items: &mut Vec<Item>) {
        if self.impl_trait_records.is_empty() {
            return;
        }
        let mut trait_map: HashMap<String, Vec<TraitMethod>> = HashMap::new();
        for item in items.iter() {
            if let Item::TypeAlias(alias) = item
                && alias.is_trait
            {
                trait_map.insert(alias.name.clone(), alias.trait_methods.clone());
            }
        }
        if trait_map.is_empty() {
            return;
        }

        let module_key = module_symbol_key(&self.current_package, &self.module_disambiguator);
        let mut synthesized = Vec::new();
        for record in &mut self.impl_trait_records {
            let Some(required_methods) = trait_map.get(&record.trait_name) else {
                continue;
            };
            let mut provided: HashSet<String> =
                record.methods.iter().map(|m| m.name.clone()).collect();
            for required in required_methods {
                if provided.contains(&required.name) {
                    continue;
                }
                let Some(default_body) = &required.default_body else {
                    continue;
                };
                let recv_ty = TypeAst {
                    kind: TypeAstKind::Named(record.recv_name.clone()),
                    span: required.span.clone(),
                };
                let mut params = Vec::with_capacity(required.params.len() + 1);
                params.push(Param {
                    name: "self".to_string(),
                    ty: recv_ty,
                    span: required.span.clone(),
                });
                params.extend(required.params.clone());
                synthesized.push(Item::Function(Function {
                    vis: Visibility::Private,
                    name: mangle_impl_method(&module_key, &record.recv_name, &required.name),
                    type_params: required.type_params.clone(),
                    params,
                    is_variadic: required.is_variadic,
                    ret_type: Some(required.ret_type.clone()),
                    is_extern: false,
                    is_unsafe: false,
                    extern_abi: None,
                    body: default_body.clone(),
                    span: required.span.clone(),
                }));
                record.methods.push(TraitMethod {
                    name: required.name.clone(),
                    type_params: required.type_params.clone(),
                    params: required.params.clone(),
                    is_variadic: required.is_variadic,
                    ret_type: required.ret_type.clone(),
                    default_body: None,
                    span: required.span.clone(),
                });
                provided.insert(required.name.clone());
            }
        }
        items.extend(synthesized);
    }

    fn trait_method_signature_eq(&self, expected: &TraitMethod, actual: &TraitMethod) -> bool {
        if expected.is_variadic != actual.is_variadic
            || expected.type_params.len() != actual.type_params.len()
            || expected.params.len() != actual.params.len()
        {
            return false;
        }

        // Compare trait/impl generic methods modulo parameter renaming
        // (e.g. trait fn f[T](x: T) and impl fn f[U](x: U)).
        let mut generic_map: HashMap<String, String> = HashMap::new();
        for (lhs, rhs) in expected.type_params.iter().zip(actual.type_params.iter()) {
            if lhs.bounds.len() != rhs.bounds.len() {
                return false;
            }
            generic_map.insert(rhs.name.clone(), lhs.name.clone());
        }
        for (lhs, rhs) in expected.type_params.iter().zip(actual.type_params.iter()) {
            for (lhs_bound, rhs_bound) in lhs.bounds.iter().zip(rhs.bounds.iter()) {
                if !Self::type_ast_eq_with_generic_map(lhs_bound, rhs_bound, &generic_map) {
                    return false;
                }
            }
        }

        expected
            .params
            .iter()
            .zip(actual.params.iter())
            .all(|(lhs, rhs)| Self::type_ast_eq_with_generic_map(&lhs.ty, &rhs.ty, &generic_map))
            && Self::type_ast_eq_with_generic_map(
                &expected.ret_type,
                &actual.ret_type,
                &generic_map,
            )
    }

    fn type_ast_eq_with_generic_map(
        lhs: &TypeAst,
        rhs: &TypeAst,
        rhs_generic_to_lhs: &HashMap<String, String>,
    ) -> bool {
        match (&lhs.kind, &rhs.kind) {
            (TypeAstKind::Named(a), TypeAstKind::Named(b)) => {
                a == b
                    || rhs_generic_to_lhs
                        .get(b)
                        .map(|mapped| mapped == a)
                        .unwrap_or(false)
            }
            (TypeAstKind::Ref(a), TypeAstKind::Ref(b))
            | (TypeAstKind::MutRef(a), TypeAstKind::MutRef(b))
            | (TypeAstKind::Own(a), TypeAstKind::Own(b))
            | (TypeAstKind::Alias(a), TypeAstKind::Alias(b))
            | (TypeAstKind::Slice(a), TypeAstKind::Slice(b))
            | (TypeAstKind::Chan(a), TypeAstKind::Chan(b))
            | (TypeAstKind::Shared(a), TypeAstKind::Shared(b)) => {
                Self::type_ast_eq_with_generic_map(a, b, rhs_generic_to_lhs)
            }
            (TypeAstKind::Array(a_ty, a_len), TypeAstKind::Array(b_ty, b_len)) => {
                a_len == b_len && Self::type_ast_eq_with_generic_map(a_ty, b_ty, rhs_generic_to_lhs)
            }
            (TypeAstKind::Map(a_key, a_val), TypeAstKind::Map(b_key, b_val))
            | (TypeAstKind::Result(a_key, a_val), TypeAstKind::Result(b_key, b_val)) => {
                Self::type_ast_eq_with_generic_map(a_key, b_key, rhs_generic_to_lhs)
                    && Self::type_ast_eq_with_generic_map(a_val, b_val, rhs_generic_to_lhs)
            }
            (TypeAstKind::Tuple(a_items), TypeAstKind::Tuple(b_items)) => {
                a_items.len() == b_items.len()
                    && a_items
                        .iter()
                        .zip(b_items.iter())
                        .all(|(a, b)| Self::type_ast_eq_with_generic_map(a, b, rhs_generic_to_lhs))
            }
            (
                TypeAstKind::FnPtr {
                    params: a_params,
                    ret: a_ret,
                    is_variadic: a_var,
                },
                TypeAstKind::FnPtr {
                    params: b_params,
                    ret: b_ret,
                    is_variadic: b_var,
                },
            ) => {
                a_var == b_var
                    && a_params.len() == b_params.len()
                    && a_params
                        .iter()
                        .zip(b_params.iter())
                        .all(|(a, b)| Self::type_ast_eq_with_generic_map(a, b, rhs_generic_to_lhs))
                    && Self::type_ast_eq_with_generic_map(a_ret, b_ret, rhs_generic_to_lhs)
            }
            (
                TypeAstKind::Closure {
                    params: a_params,
                    ret: a_ret,
                    is_variadic: a_var,
                },
                TypeAstKind::Closure {
                    params: b_params,
                    ret: b_ret,
                    is_variadic: b_var,
                },
            )
            | (
                TypeAstKind::FnPtr {
                    params: a_params,
                    ret: a_ret,
                    is_variadic: a_var,
                },
                TypeAstKind::Closure {
                    params: b_params,
                    ret: b_ret,
                    is_variadic: b_var,
                },
            )
            | (
                TypeAstKind::Closure {
                    params: a_params,
                    ret: a_ret,
                    is_variadic: a_var,
                },
                TypeAstKind::FnPtr {
                    params: b_params,
                    ret: b_ret,
                    is_variadic: b_var,
                },
            ) => {
                a_var == b_var
                    && a_params.len() == b_params.len()
                    && a_params
                        .iter()
                        .zip(b_params.iter())
                        .all(|(a, b)| Self::type_ast_eq_with_generic_map(a, b, rhs_generic_to_lhs))
                    && Self::type_ast_eq_with_generic_map(a_ret, b_ret, rhs_generic_to_lhs)
            }
            (TypeAstKind::Interface, TypeAstKind::Interface) => true,
            _ => false,
        }
    }

    fn parse_const_item(
        &mut self,
        vis: Visibility,
        start_override: Option<Span>,
    ) -> Option<ConstItem> {
        let start = start_override.unwrap_or_else(|| {
            self.peek_span().unwrap_or(Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            })
        });
        self.expect_keyword(Keyword::Const);
        let name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected const name");
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
        if !self.at_symbol(Symbol::Semi) {
            self.error_here("expected `;` after const item");
            return None;
        }
        let semi = self.bump().span;
        Some(ConstItem {
            vis,
            name,
            ty,
            init,
            span: Span {
                start: start.start,
                end: semi.end,
                line: start.line,
                column: start.column,
            },
        })
    }

    fn parse_global_var(
        &mut self,
        vis: Visibility,
        start_override: Option<Span>,
    ) -> Option<GlobalVar> {
        let start = start_override.unwrap_or_else(|| {
            self.peek_span().unwrap_or(Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            })
        });
        self.expect_keyword(Keyword::Let);
        let name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected global name");
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
        if !self.at_symbol(Symbol::Semi) {
            self.error_here("expected `;` after global let");
            return None;
        }
        let semi = self.bump().span;
        Some(GlobalVar {
            vis,
            name,
            ty,
            init,
            span: Span {
                start: start.start,
                end: semi.end,
                line: start.line,
                column: start.column,
            },
        })
    }

    fn parse_struct_def(&mut self, vis: Visibility, mut layout: LayoutAttr) -> Option<StructDef> {
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
        self.merge_layout_attrs(&mut layout, extra_layout, Some(start.clone()));
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
            let field_vis = if self.at_keyword(Keyword::Pub) || self.at_keyword(Keyword::Private) {
                self.parse_item_visibility()
            } else {
                Visibility::Public
            };
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
                vis: field_vis,
                name: field_name,
                ty,
                span: field_start,
            });
        }
        self.expect_symbol(Symbol::RBrace);
        Some(StructDef {
            vis,
            name,
            fields,
            is_copy,
            layout,
            span: start,
        })
    }

    fn parse_enum_def(&mut self, vis: Visibility, mut layout: LayoutAttr) -> Option<EnumDef> {
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
        self.merge_layout_attrs(&mut layout, extra_layout, Some(start.clone()));
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
            vis,
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
                let repr = Self::parse_repr_name(&repr_name);
                self.apply_layout_repr(&mut layout, repr, Some(repr_span));
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

    fn parse_repr_name(name: &str) -> ParsedRepr {
        match name.to_ascii_lowercase().as_str() {
            "c" => ParsedRepr::C,
            "transparent" => ParsedRepr::Transparent,
            "i8" => ParsedRepr::Int(ReprInt::I8),
            "i16" => ParsedRepr::Int(ReprInt::I16),
            "i32" => ParsedRepr::Int(ReprInt::I32),
            "i64" => ParsedRepr::Int(ReprInt::I64),
            "isize" => ParsedRepr::Int(ReprInt::Isize),
            "u8" => ParsedRepr::Int(ReprInt::U8),
            "u16" => ParsedRepr::Int(ReprInt::U16),
            "u32" => ParsedRepr::Int(ReprInt::U32),
            "u64" => ParsedRepr::Int(ReprInt::U64),
            "usize" => ParsedRepr::Int(ReprInt::Usize),
            _ => ParsedRepr::Other(name.to_string()),
        }
    }

    fn current_repr(layout: &LayoutAttr) -> Option<ParsedRepr> {
        if layout.repr_c {
            Some(ParsedRepr::C)
        } else if layout.repr_transparent {
            Some(ParsedRepr::Transparent)
        } else if let Some(other) = &layout.repr_other {
            Some(ParsedRepr::Other(other.clone()))
        } else {
            layout.repr_int.map(ParsedRepr::Int)
        }
    }

    fn apply_layout_repr(&mut self, layout: &mut LayoutAttr, repr: ParsedRepr, span: Option<Span>) {
        let existing = Self::current_repr(layout);
        if existing == Some(repr.clone()) {
            return;
        }
        if existing.is_some() {
            self.diags
                .push("conflicting repr modifiers are not allowed", span);
            return;
        }
        match repr {
            ParsedRepr::C => layout.repr_c = true,
            ParsedRepr::Transparent => layout.repr_transparent = true,
            ParsedRepr::Int(kind) => layout.repr_int = Some(kind),
            ParsedRepr::Other(other) => layout.repr_other = Some(other),
        }
    }

    fn merge_layout_attrs(
        &mut self,
        layout: &mut LayoutAttr,
        extra: LayoutAttr,
        span: Option<Span>,
    ) {
        if extra.repr_c {
            self.apply_layout_repr(layout, ParsedRepr::C, span.clone());
        }
        if extra.repr_transparent {
            self.apply_layout_repr(layout, ParsedRepr::Transparent, span.clone());
        }
        if let Some(kind) = extra.repr_int {
            self.apply_layout_repr(layout, ParsedRepr::Int(kind), span.clone());
        }
        if let Some(other) = extra.repr_other {
            self.apply_layout_repr(layout, ParsedRepr::Other(other), span.clone());
        }
        if extra.pack.is_some() {
            layout.pack = extra.pack;
        }
        layout.bitfield |= extra.bitfield;
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
            if self.at_label_stmt_start()
                && let Some(stmt) = self.parse_label_stmt()
            {
                stmts.push(stmt);
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
                let label = match self.peek().kind.clone() {
                    TokenKind::Ident(name) => {
                        self.bump();
                        Some(name)
                    }
                    _ => None,
                };
                if self.at_symbol(Symbol::Semi) {
                    self.bump();
                }
                stmts.push(Stmt::Break { label, span });
                continue;
            }
            if self.at_keyword(Keyword::Continue) {
                let span = self.bump().span;
                let label = match self.peek().kind.clone() {
                    TokenKind::Ident(name) => {
                        self.bump();
                        Some(name)
                    }
                    _ => None,
                };
                if self.at_symbol(Symbol::Semi) {
                    self.bump();
                }
                stmts.push(Stmt::Continue { label, span });
                continue;
            }
            if self.at_keyword(Keyword::Let) {
                let stmt = self.parse_let_stmt()?;
                stmts.push(stmt);
                continue;
            }
            if self.at_keyword(Keyword::Const) {
                let stmt = self.parse_const_stmt()?;
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
            if let Some(op) = self.peek_assign_op() {
                let span = self.bump().span;
                let value = self.parse_expr()?;
                if self.at_symbol(Symbol::Semi) {
                    self.bump();
                }
                stmts.push(Stmt::Assign {
                    op,
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
        if self.at_keyword(Keyword::For) {
            return self.parse_for_stmt_with_label(Some(label_name));
        }
        if self.at_keyword(Keyword::While) {
            let mut stmt = self.parse_while_stmt()?;
            if let Stmt::While { label, .. } = &mut stmt {
                *label = Some(label_name);
            }
            return Some(stmt);
        }
        if self.at_keyword(Keyword::Loop) {
            let mut stmt = self.parse_loop_stmt()?;
            if let Stmt::Loop { label, .. } = &mut stmt {
                *label = Some(label_name);
            }
            return Some(stmt);
        }
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

    fn parse_const_stmt(&mut self) -> Option<Stmt> {
        let span = self.bump().span;
        let name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected identifier after const");
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
        Some(Stmt::Const {
            name,
            ty,
            init,
            span,
        })
    }

    fn parse_for_stmt(&mut self) -> Option<Stmt> {
        self.parse_for_stmt_with_label(None)
    }

    fn parse_for_stmt_with_label(&mut self, label: Option<String>) -> Option<Stmt> {
        let span = self.bump().span;
        let first_name = match self.bump().kind {
            TokenKind::Ident(name) => name,
            _ => {
                self.error_here("expected iterator variable");
                return None;
            }
        };
        let second_name = if self.at_symbol(Symbol::Comma) {
            self.bump();
            match self.bump().kind {
                TokenKind::Ident(name) => Some(name),
                _ => {
                    self.error_here("expected value variable after `,`");
                    return None;
                }
            }
        } else {
            None
        };
        self.expect_keyword(Keyword::In);
        let start_or_iter = self.parse_expr_no_struct_lit()?;
        let range_inclusive = if self.at_symbol(Symbol::DotDot) {
            self.bump();
            Some(false)
        } else if self.at_symbol(Symbol::DotDotEq) {
            self.bump();
            Some(true)
        } else {
            None
        };
        if let Some(inclusive) = range_inclusive {
            let end_expr = self.parse_expr_no_struct_lit()?;
            let body = self.parse_block()?;
            let (name, index) = if let Some(value_name) = second_name {
                (value_name, Some(first_name))
            } else {
                (first_name, None)
            };
            return Some(Stmt::ForRange {
                label,
                name,
                index,
                start: start_or_iter,
                end: end_expr,
                inclusive,
                body,
                span,
            });
        }
        let iter = start_or_iter;
        let body = self.parse_block()?;
        let (name, index) = if let Some(value_name) = second_name {
            (value_name, Some(first_name))
        } else {
            (first_name, None)
        };
        Some(Stmt::ForIn {
            label,
            name,
            index,
            iter,
            body,
            span,
        })
    }

    fn parse_while_stmt(&mut self) -> Option<Stmt> {
        let span = self.bump().span;
        let cond = self.parse_expr_no_struct_lit()?;
        let body = self.parse_block()?;
        Some(Stmt::While {
            label: None,
            cond,
            body,
            span,
        })
    }

    fn parse_loop_stmt(&mut self) -> Option<Stmt> {
        let span = self.bump().span;
        let body = self.parse_block()?;
        Some(Stmt::Loop {
            label: None,
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
        if self.at_symbol(Symbol::Tilde) {
            let span = self.bump().span;
            let expr = self.parse_unary_expr()?;
            return Some(self.new_expr(
                ExprKind::Unary {
                    op: UnaryOp::BitNot,
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
                ExprKind::Deref {
                    expr: Box::new(expr),
                },
                span,
            ));
        }
        let mut expr = self.parse_postfix_expr()?;
        while self.at_keyword(Keyword::As) {
            let span = self.bump().span;
            let ty = self.parse_type()?;
            expr = self.new_expr(
                ExprKind::Cast {
                    expr: Box::new(expr),
                    ty,
                },
                span,
            );
        }
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
            if let Some(type_args) = self.try_parse_call_type_args() {
                let span = self.peek_span().unwrap_or(expr.span.clone());
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
        if self.at_ident("asm") && (self.peek_is_ident("volatile") || self.peek_is_ident("goto")) {
            return self.parse_colon_inline_asm_expr();
        }
        if self.at_keyword(Keyword::Unsafe) {
            self.bump();
            if !self.at_symbol(Symbol::LBrace) {
                self.error_here("expected block after unsafe");
                return None;
            }
            let block = self.parse_block()?;
            return Some(self.new_expr(ExprKind::UnsafeBlock(Box::new(block.clone())), block.span));
        }
        if self.at_symbol(Symbol::Pipe) {
            return self.parse_closure_expr();
        }
        if self.at_symbol(Symbol::LBracket) {
            return self.parse_bracket_literal_expr();
        }
        if self.at_symbol(Symbol::LBrace) {
            let block = self.parse_block()?;
            return Some(self.new_expr(ExprKind::Block(Box::new(block.clone())), block.span));
        }
        if self.at_keyword(Keyword::If) {
            self.bump();
            let cond = self.parse_expr_no_struct_lit()?;
            let then_block = Box::new(self.parse_block()?);
            let else_block = if self.at_keyword(Keyword::Else) {
                self.bump();
                if self.at_keyword(Keyword::If) {
                    // Desugar `else if ...` into `else { if ... }` so the AST shape stays stable.
                    let else_if = self.parse_primary_expr()?;
                    Some(Box::new(Block {
                        stmts: Vec::new(),
                        tail: Some(else_if.clone()),
                        span: else_if.span,
                    }))
                } else {
                    Some(Box::new(self.parse_block()?))
                }
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
                let guard = if self.at_keyword(Keyword::If) {
                    self.bump();
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                self.expect_symbol(Symbol::FatArrow);
                let body = self.parse_block_or_expr()?;
                arms.push(MatchArm {
                    pattern,
                    guard,
                    body,
                    span: arm_span,
                });
                if self.at_symbol(Symbol::Comma) {
                    self.bump();
                } else if !self.at_symbol(Symbol::RBrace) {
                    self.error_here("expected `,` or `}` after match arm");
                    return None;
                }
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
            TokenKind::StringLit(value) => self.parse_interpolated_string_expr(value, span),
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
                Some(self.new_expr(ExprKind::After { ms: Box::new(ms) }, span))
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

    fn parse_interpolation_expr_fragment(&mut self, raw: &str, span: &Span) -> Option<Expr> {
        let tokens = Lexer::new(raw).lex_all();
        let mut sub = Parser::new_with_expr_id(tokens, self.next_expr_id);
        sub.current_package = self.current_package.clone();
        sub.module_disambiguator = self.module_disambiguator.clone();
        let expr = sub.parse_expr();
        if !sub.at_eof() {
            sub.error_here("unexpected token inside string interpolation");
        }
        self.next_expr_id = sub.next_expr_id();
        if !sub.diags.is_empty() {
            for mut d in sub.diags.items {
                if d.span.is_none() {
                    d.span = Some(span.clone());
                }
                self.diags.push_diag(d);
            }
        }
        expr
    }

    fn parse_interpolated_string_expr(&mut self, value: String, span: Span) -> Option<Expr> {
        if !value.contains("${") {
            return Some(self.new_expr(ExprKind::String(value), span));
        }

        let chars: Vec<char> = value.chars().collect();
        let mut i = 0usize;
        let mut literal = String::new();
        let mut parts: Vec<Expr> = Vec::new();

        while i < chars.len() {
            if chars[i] == '$' && i + 1 < chars.len() && chars[i + 1] == '{' {
                if !literal.is_empty() {
                    parts.push(self.new_expr(ExprKind::String(literal.clone()), span.clone()));
                    literal.clear();
                }
                i += 2;
                let mut depth = 1i32;
                let mut frag = String::new();
                while i < chars.len() {
                    let ch = chars[i];
                    if ch == '{' {
                        depth += 1;
                        frag.push(ch);
                    } else if ch == '}' {
                        depth -= 1;
                        if depth == 0 {
                            i += 1;
                            break;
                        }
                        frag.push(ch);
                    } else {
                        frag.push(ch);
                    }
                    i += 1;
                }
                if depth != 0 {
                    self.diags.push(
                        "unterminated `${...}` in string interpolation",
                        Some(span.clone()),
                    );
                    return None;
                }
                let frag = frag.trim();
                if frag.is_empty() {
                    self.diags
                        .push("empty `${...}` in string interpolation", Some(span.clone()));
                    return None;
                }
                let expr = self.parse_interpolation_expr_fragment(frag, &span)?;
                parts.push(expr);
                continue;
            }
            literal.push(chars[i]);
            i += 1;
        }
        if !literal.is_empty() {
            parts.push(self.new_expr(ExprKind::String(literal), span.clone()));
        }
        if parts.is_empty() {
            return Some(self.new_expr(ExprKind::String(String::new()), span));
        }

        let mut iter = parts.into_iter();
        let mut out = iter.next()?;
        for part in iter {
            let callee = self.new_expr(ExprKind::Ident("string_concat".to_string()), span.clone());
            out = self.new_expr(
                ExprKind::Call {
                    callee: Box::new(callee),
                    type_args: Vec::new(),
                    args: vec![out, part],
                },
                span.clone(),
            );
        }
        Some(out)
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

        let mut operand_names: Vec<Option<String>> =
            Vec::with_capacity(outputs.len() + inputs.len());
        for (name, _, _) in &outputs {
            operand_names.push(name.clone());
        }
        for (name, _, _) in &inputs {
            operand_names.push(name.clone());
        }
        let operand_count = operand_names.len();
        let rewritten_template =
            rewrite_gcc_asm_template(&template_raw, &labels, &operand_names, operand_count);
        let mut constraints = build_asm_constraints(&outputs, &inputs, &clobbers);
        if is_goto || !labels.is_empty() {
            for _ in 0..labels.len() {
                if !constraints.is_empty() {
                    constraints.push(',');
                }
                constraints.push_str("!i");
            }
            let mut args = Vec::new();
            args.push(self.new_expr(ExprKind::String(rewritten_template), start.clone()));
            args.push(self.new_expr(ExprKind::String(constraints), start.clone()));
            args.push(self.new_expr(ExprKind::String(labels.join(",")), start.clone()));
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
        call_args.push(self.new_expr(ExprKind::String(rewritten_template), start.clone()));
        call_args.push(self.new_expr(ExprKind::String(constraints), start.clone()));
        for (_, _, expr) in &outputs {
            call_args.push(expr.clone());
        }
        for (_, _, expr) in &inputs {
            call_args.push(expr.clone());
        }
        let callee_name = if is_volatile { "asm_volatile" } else { "asm" };
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
                op: AssignOp::Assign,
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
                op: AssignOp::Assign,
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
        self.expect_symbol(Symbol::LBrace);
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
        Some(self.new_expr(ExprKind::StructLit { name, fields }, span))
    }

    fn parse_braced_expr_list(&mut self) -> Option<(Vec<Expr>, Span)> {
        let lbrace = self.peek_span().unwrap_or(Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        });
        self.expect_symbol(Symbol::LBrace);
        let mut items = Vec::new();
        if !self.at_symbol(Symbol::RBrace) {
            loop {
                items.push(self.parse_expr()?);
                if self.at_symbol(Symbol::Comma) {
                    self.bump();
                    if self.at_symbol(Symbol::RBrace) {
                        break;
                    }
                } else {
                    break;
                }
            }
        }
        let rbrace = self.peek_span().unwrap_or(lbrace.clone());
        self.expect_symbol(Symbol::RBrace);
        let span = Span {
            start: lbrace.start,
            end: rbrace.end,
            line: lbrace.line,
            column: lbrace.column,
        };
        Some((items, span))
    }

    fn lower_slice_literal_expr(&mut self, elem_ty: TypeAst, elems: Vec<Expr>, span: Span) -> Expr {
        let tmp_name = format!("__slice_lit_{}", self.next_expr_id);
        let slice_ty = TypeAst {
            kind: TypeAstKind::Slice(Box::new(elem_ty.clone())),
            span: span.clone(),
        };
        let len_lit = self.new_expr(ExprKind::Int("0".to_string()), span.clone());
        let cap_lit = self.new_expr(ExprKind::Int(elems.len().to_string()), span.clone());
        let make_callee = self.new_expr(ExprKind::Ident("make_slice".to_string()), span.clone());
        let make_call = self.new_expr(
            ExprKind::Call {
                callee: Box::new(make_callee),
                type_args: vec![elem_ty.clone()],
                args: vec![len_lit, cap_lit],
            },
            span.clone(),
        );
        let mut stmts = Vec::new();
        stmts.push(Stmt::Let {
            name: tmp_name.clone(),
            ty: Some(slice_ty),
            init: make_call,
            span: span.clone(),
        });
        for elem in elems {
            let push_callee =
                self.new_expr(ExprKind::Ident("slice_push".to_string()), span.clone());
            let tmp_ident = self.new_expr(ExprKind::Ident(tmp_name.clone()), span.clone());
            let borrow = self.new_expr(
                ExprKind::Borrow {
                    is_mut: true,
                    expr: Box::new(tmp_ident),
                },
                span.clone(),
            );
            let push_call = self.new_expr(
                ExprKind::Call {
                    callee: Box::new(push_callee),
                    type_args: vec![elem_ty.clone()],
                    args: vec![borrow, elem],
                },
                span.clone(),
            );
            stmts.push(Stmt::Expr {
                expr: push_call,
                span: span.clone(),
            });
        }
        let tail = self.new_expr(ExprKind::Ident(tmp_name), span.clone());
        self.new_expr(
            ExprKind::Block(Box::new(Block {
                stmts,
                tail: Some(tail),
                span: span.clone(),
            })),
            span,
        )
    }

    fn parse_bracket_literal_expr(&mut self) -> Option<Expr> {
        let start = self.peek_span().unwrap_or(Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        });

        let save_idx = self.idx;
        let save_diags = self.diags.items.len();
        self.bump(); // `[`
        if self.at_symbol(Symbol::RBracket) {
            self.bump(); // `]`
            if let Some(elem_ty) = self.parse_type()
                && self.at_symbol(Symbol::LBrace)
            {
                let (elems, body_span) = self.parse_braced_expr_list()?;
                let span = Span {
                    start: start.start,
                    end: body_span.end,
                    line: start.line,
                    column: start.column,
                };
                return Some(self.lower_slice_literal_expr(elem_ty, elems, span));
            }
        }
        self.idx = save_idx;
        self.diags.items.truncate(save_diags);

        self.expect_symbol(Symbol::LBracket);
        let mut items = Vec::new();
        if !self.at_symbol(Symbol::RBracket) {
            loop {
                items.push(self.parse_expr()?);
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
        let rbracket = self.peek_span().unwrap_or(start.clone());
        self.expect_symbol(Symbol::RBracket);
        let span = Span {
            start: start.start,
            end: rbracket.end,
            line: start.line,
            column: start.column,
        };
        Some(self.new_expr(ExprKind::ArrayLit(items), span))
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

    fn parse_type_param_names(&mut self) -> Option<Vec<TypeParam>> {
        if !self.at_symbol(Symbol::LBracket) {
            return Some(Vec::new());
        }
        self.bump();
        let mut params = Vec::new();
        if !self.at_symbol(Symbol::RBracket) {
            loop {
                let param_span = self.peek_span().unwrap_or(Span {
                    start: 0,
                    end: 0,
                    line: 1,
                    column: 1,
                });
                let name = match self.bump().kind {
                    TokenKind::Ident(name) => name,
                    _ => {
                        self.error_here("expected type parameter name");
                        return None;
                    }
                };
                let mut bounds = Vec::new();
                if self.at_symbol(Symbol::Colon) {
                    self.bump();
                    loop {
                        bounds.push(self.parse_type()?);
                        if self.at_symbol(Symbol::Plus) {
                            self.bump();
                            continue;
                        }
                        break;
                    }
                }
                params.push(TypeParam {
                    name,
                    bounds,
                    span: param_span,
                });
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
        Some(params)
    }

    fn try_parse_call_type_args(&mut self) -> Option<Vec<TypeAst>> {
        if !self.at_symbol(Symbol::LBracket) {
            return None;
        }
        let save_idx = self.idx;
        let save_diags = self.diags.items.len();
        let parsed = self.parse_type_args();
        match parsed {
            Some(type_args) if self.at_symbol(Symbol::LParen) => Some(type_args),
            _ => {
                self.idx = save_idx;
                self.diags.items.truncate(save_diags);
                None
            }
        }
    }

    fn parse_closure_expr(&mut self) -> Option<Expr> {
        let start = self.bump().span;
        let mut params = Vec::new();
        if !self.at_symbol(Symbol::Pipe) {
            loop {
                let pspan = self.peek_span().unwrap_or(start.clone());
                let name = match self.bump().kind {
                    TokenKind::Ident(name) => name,
                    _ => {
                        self.error_here("expected closure parameter name");
                        return None;
                    }
                };
                let ty = if self.at_symbol(Symbol::Colon) {
                    self.bump();
                    Some(self.parse_type()?)
                } else {
                    None
                };
                params.push(ClosureParam {
                    name,
                    ty,
                    span: pspan,
                });
                if self.at_symbol(Symbol::Comma) {
                    self.bump();
                    if self.at_symbol(Symbol::Pipe) {
                        break;
                    }
                } else {
                    break;
                }
            }
        }
        self.expect_symbol(Symbol::Pipe);
        let body = if self.at_symbol(Symbol::LBrace) {
            BlockOrExpr::Block(Box::new(self.parse_block()?))
        } else {
            BlockOrExpr::Expr(self.parse_expr()?)
        };
        let end = match &body {
            BlockOrExpr::Block(block) => block.span.clone(),
            BlockOrExpr::Expr(expr) => expr.span.clone(),
        };
        Some(self.new_expr(
            ExprKind::Closure {
                params,
                body: Box::new(body),
            },
            Span {
                start: start.start,
                end: end.end,
                line: start.line,
                column: start.column,
            },
        ))
    }

    fn parse_pattern(&mut self) -> Option<Pattern> {
        let mut patterns = Vec::new();
        patterns.push(self.parse_pattern_atom()?);
        while self.at_symbol(Symbol::Pipe) {
            self.bump();
            patterns.push(self.parse_pattern_atom()?);
        }
        if patterns.len() == 1 {
            return patterns.pop();
        }
        Some(Pattern::Or(patterns))
    }

    fn parse_pattern_atom(&mut self) -> Option<Pattern> {
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
            if self.at_symbol(Symbol::RBracket) {
                self.bump();
                let inner = self.parse_type()?;
                return Some(TypeAst {
                    kind: TypeAstKind::Slice(Box::new(inner)),
                    span,
                });
            }
            let len_raw = match self.bump().kind {
                TokenKind::IntLit(raw) => raw,
                _ => {
                    self.error_here("expected array length");
                    return None;
                }
            };
            let len = match len_raw.parse::<usize>() {
                Ok(v) => v,
                Err(_) => {
                    self.error_here("array length out of range");
                    return None;
                }
            };
            self.expect_symbol(Symbol::RBracket);
            let inner = self.parse_type()?;
            return Some(TypeAst {
                kind: TypeAstKind::Array(Box::new(inner), len),
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
        if self.at_ident("dyn") {
            self.bump();
            let trait_name = match self.bump().kind {
                TokenKind::Ident(name) => name,
                _ => {
                    self.error_here("expected trait name after `dyn`");
                    return None;
                }
            };
            return Some(TypeAst {
                kind: TypeAstKind::Named(trait_name),
                span,
            });
        }
        if self.at_keyword(Keyword::Interface) || self.at_keyword(Keyword::Trait) {
            self.bump();
            return Some(TypeAst {
                kind: TypeAstKind::Interface,
                span,
            });
        }
        if self.at_keyword(Keyword::Fn) {
            self.bump();
            return self.parse_callable_type(span, false);
        }
        if self.at_ident("closure") {
            self.bump();
            return self.parse_callable_type(span, true);
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
        if self.at_ident("own") {
            self.bump();
            self.expect_symbol(Symbol::LBracket);
            let inner = self.parse_type()?;
            self.expect_symbol(Symbol::RBracket);
            return Some(TypeAst {
                kind: TypeAstKind::Own(Box::new(inner)),
                span,
            });
        }
        if self.at_ident("alias") {
            self.bump();
            self.expect_symbol(Symbol::LBracket);
            let inner = self.parse_type()?;
            self.expect_symbol(Symbol::RBracket);
            return Some(TypeAst {
                kind: TypeAstKind::Alias(Box::new(inner)),
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
        self.tokens
            .get(self.idx)
            .unwrap_or_else(|| self.tokens.last().expect("parser always has eof token"))
    }

    fn bump(&mut self) -> Token {
        let token = self.peek().clone();
        if self.is_explicit_semi(&token) {
            self.diags.push(
                "explicit `;` is not allowed; end statements with newline",
                Some(token.span.clone()),
            );
        }
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

    fn is_explicit_semi(&self, token: &Token) -> bool {
        matches!(token.kind, TokenKind::Symbol(Symbol::Semi))
            // inserted semicolon has zero-length span, explicit source `;` has width
            && token.span.end > token.span.start
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
        let t0 = self.tokens.get(self.idx).map(|t| &t.kind);
        let t1 = self.tokens.get(self.idx + 1).map(|t| &t.kind);
        let t2 = self.tokens.get(self.idx + 2).map(|t| &t.kind);
        matches!(
            (t0, t1, t2),
            (
                Some(TokenKind::Symbol(Symbol::Dot)),
                Some(TokenKind::Symbol(Symbol::Dot)),
                Some(TokenKind::Symbol(Symbol::Dot))
            ) | (
                Some(TokenKind::Symbol(Symbol::DotDot)),
                Some(TokenKind::Symbol(Symbol::Dot)),
                _
            )
        )
    }

    fn consume_variadic_marker(&mut self) {
        if matches!(
            self.tokens.get(self.idx).map(|t| &t.kind),
            Some(TokenKind::Symbol(Symbol::DotDot))
        ) && matches!(
            self.tokens.get(self.idx + 1).map(|t| &t.kind),
            Some(TokenKind::Symbol(Symbol::Dot))
        ) {
            self.bump();
            self.bump();
        } else if self.at_variadic_marker() {
            self.bump();
            self.bump();
            self.bump();
        } else {
            self.error_here("expected `...`");
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
            TokenKind::Symbol(Symbol::Pipe) => (3, BinaryOp::BitOr),
            TokenKind::Symbol(Symbol::Caret) => (4, BinaryOp::BitXor),
            TokenKind::Symbol(Symbol::Amp) => (5, BinaryOp::BitAnd),
            TokenKind::Symbol(Symbol::EqEq) => (6, BinaryOp::Eq),
            TokenKind::Symbol(Symbol::NotEq) => (6, BinaryOp::NotEq),
            TokenKind::Symbol(Symbol::Lt) => (7, BinaryOp::Lt),
            TokenKind::Symbol(Symbol::Lte) => (7, BinaryOp::Lte),
            TokenKind::Symbol(Symbol::Gt) => (7, BinaryOp::Gt),
            TokenKind::Symbol(Symbol::Gte) => (7, BinaryOp::Gte),
            TokenKind::Symbol(Symbol::Shl) => (8, BinaryOp::Shl),
            TokenKind::Symbol(Symbol::Shr) => (8, BinaryOp::Shr),
            TokenKind::Symbol(Symbol::Plus) => (9, BinaryOp::Add),
            TokenKind::Symbol(Symbol::Minus) => (9, BinaryOp::Sub),
            TokenKind::Symbol(Symbol::Star) => (10, BinaryOp::Mul),
            TokenKind::Symbol(Symbol::Slash) => (10, BinaryOp::Div),
            TokenKind::Symbol(Symbol::Percent) => (10, BinaryOp::Rem),
            _ => return None,
        };
        Some(op)
    }

    fn peek_assign_op(&self) -> Option<AssignOp> {
        match self.peek().kind {
            TokenKind::Symbol(Symbol::Eq) => Some(AssignOp::Assign),
            TokenKind::Symbol(Symbol::PlusEq) => Some(AssignOp::AddAssign),
            TokenKind::Symbol(Symbol::MinusEq) => Some(AssignOp::SubAssign),
            TokenKind::Symbol(Symbol::StarEq) => Some(AssignOp::MulAssign),
            TokenKind::Symbol(Symbol::SlashEq) => Some(AssignOp::DivAssign),
            TokenKind::Symbol(Symbol::PercentEq) => Some(AssignOp::RemAssign),
            TokenKind::Symbol(Symbol::AmpEq) => Some(AssignOp::BitAndAssign),
            TokenKind::Symbol(Symbol::PipeEq) => Some(AssignOp::BitOrAssign),
            TokenKind::Symbol(Symbol::CaretEq) => Some(AssignOp::BitXorAssign),
            TokenKind::Symbol(Symbol::ShlEq) => Some(AssignOp::ShlAssign),
            TokenKind::Symbol(Symbol::ShrEq) => Some(AssignOp::ShrAssign),
            _ => None,
        }
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
