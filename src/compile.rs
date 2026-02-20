#![allow(clippy::too_many_arguments)]

// Purpose: Orchestrate full front-end compilation from source/module inputs to LLVM IR.
// Inputs/Outputs: Consumes entry source + module mode and returns LLVM IR text or diagnostics.
// Invariants: User-facing rejects must complete before backend; codegen receives lowered MIR only.
// Gotchas: Import loading has module-aware and legacy paths; ExprId allocation must stay globally unique.

use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

use crate::frontend::ast::{
    AssignOp, BinaryOp, Block, BlockOrExpr, ClosureParam, Expr, ExprKind, FileAst, Function,
    ImportSpec, Item, Param, Pattern, Stmt, TraitMethod, TypeAst, TypeAstKind, TypeParam, UnaryOp,
    Visibility,
};
use crate::frontend::diagnostic::{
    Diagnostic, Diagnostics, E_MONOMORPHIZATION_DEPTH_EXCEEDED, format_diagnostic,
};
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::symbols::{is_impl_mangled, logical_method_name, mangle_impl_method};
use crate::incremental::{BuildCtx, ENTRY_PACKAGE, trace as inc_trace, try_load_cached_llvm_fast};
use crate::pkg::resolve::{self, ModMode};
use crate::sema::analyze;

// Precondition: `path` points to an entry source file and module mode is already chosen by CLI.
// Postcondition: Success returns LLVM IR emitted from sema-validated, MIR-lowered program state.
// Side effects: Reads source/import/module metadata, may consult cache/network per resolver policy.
pub fn compile_to_llvm(
    path: &Path,
    mode: ModMode,
    offline: bool,
    want_fetch: bool,
) -> Result<String, String> {
    match try_load_cached_llvm_fast(path, mode, offline, want_fetch) {
        Ok(Some(cached)) => {
            inc_trace("compile_to_llvm: served from incremental fast-path LLVM cache");
            return Ok(cached);
        }
        Ok(None) => {}
        Err(err) => {
            inc_trace(&format!(
                "compile_to_llvm: fast-path cache probe failed, fallback to parse: {}",
                err
            ));
        }
    }
    let source = fs::read_to_string(path).map_err(|e| e.to_string())?;
    let mut next_expr_id = 0;
    let (file, next_after_main) = parse_source_with_ids(&source, next_expr_id, Some(path))
        .map_err(|e| format!("{}:\n{}", path.display(), e))?;
    next_expr_id = next_after_main;
    let mut build_ctx = BuildCtx::new(path, mode, offline, want_fetch);
    build_ctx.record_source_file(ENTRY_PACKAGE, path, &source, &file);
    let mut imported_items = Vec::new();
    let mut visited = std::collections::HashSet::new();
    let mut std_funcs = std::collections::HashSet::new();
    match resolve::try_load_ctx_from_entry(path.to_path_buf(), mode, offline, want_fetch) {
        Ok(Some(ctx)) => {
            inc_trace("compile_to_llvm: module-aware import resolver enabled");
            load_imports_mod(
                &ctx,
                &file.imports,
                &mut next_expr_id,
                &mut imported_items,
                &mut visited,
                &mut std_funcs,
                &mut build_ctx,
            )?;
        }
        Ok(None) => {
            inc_trace(
                "compile_to_llvm: module resolver unavailable, using legacy import resolution",
            );
            let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            load_imports(
                &manifest_dir,
                &file.imports,
                &mut next_expr_id,
                &mut imported_items,
                &mut visited,
                &mut std_funcs,
                &mut build_ctx,
            )?;
        }
        Err(err) => {
            inc_trace(&format!("compile_to_llvm: module resolver error: {}", err));
            let mut diags = Diagnostics::default();
            let d = Diagnostic::new(format!("{err}"), None).code("E2000");
            diags.push_diag(d);
            return Err(render_diags(&diags, &source, Some(path)));
        }
    }
    if is_std_entry(path) {
        for item in &file.items {
            if let Item::Function(func) = item {
                std_funcs.insert(func.name.clone());
            }
        }
    }
    match build_ctx.try_load_cached_llvm() {
        Ok(Some(cached)) => {
            inc_trace("compile_to_llvm: served from incremental LLVM cache");
            return Ok(cached);
        }
        Ok(None) => {}
        Err(err) => {
            inc_trace(&format!(
                "compile_to_llvm: cache lookup failed, fallback to full compile: {}",
                err
            ));
        }
    }
    let mut items = imported_items;
    items.extend(file.items.clone());
    let mut merged = FileAst {
        package: file.package.clone(),
        imports: file.imports.clone(),
        items,
    };
    lower_high_level_features(&mut merged, &mut next_expr_id)
        .map_err(|msg| render_compile_stage_error(msg, &source, Some(path)))?;
    let program = match analyze(&merged, &std_funcs) {
        Ok(program) => program,
        Err(diags) => return Err(render_diags(&diags, &source, Some(path))),
    };
    // Backend is MIR-only: all codegen must go through lowered MIR.
    let mut mir = crate::mir::lower::lower_program(&program).map_err(|e| e.to_string())?;
    for func in &mut mir.functions {
        crate::mir::passes::build_cleanup_chains(func).map_err(|e| e.to_string())?;
        crate::mir::passes::linear_check(func).map_err(|e| e.to_string())?;
        crate::mir::passes::verify_mir_strict(func).map_err(|e| e.to_string())?;
        crate::mir::passes::verify_backend_ready_mir(func).map_err(|e| e.to_string())?;
    }
    let module = crate::codegen::emit_llvm_from_mir(&program, &mir).map_err(|e| match e {
        crate::codegen::CodegenError::FrontendDiagnostic(msg) => msg,
        crate::codegen::CodegenError::InternalCompilerError(msg) => {
            if std::env::var("GOST_DEBUG_INTERNAL").ok().as_deref() == Some("1") {
                format!("internal compiler error (codegen): {msg}")
            } else {
                "internal compiler error (codegen)".to_string()
            }
        }
    })?;
    if let Err(err) = build_ctx.store_cached_llvm(&module.text) {
        inc_trace(&format!(
            "compile_to_llvm: cache store failed (ignored): {}",
            err
        ));
    }
    Ok(module.text)
}

fn is_std_entry(path: &Path) -> bool {
    let manifest_std = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("std");
    match (path.canonicalize(), manifest_std.canonicalize()) {
        (Ok(entry_abs), Ok(std_abs)) => entry_abs.starts_with(std_abs),
        _ => path
            .components()
            .any(|c| c.as_os_str().to_string_lossy().eq_ignore_ascii_case("std")),
    }
}

fn parse_source_with_ids(
    source: &str,
    next_expr_id: usize,
    file_path: Option<&Path>,
) -> Result<(FileAst, usize), String> {
    // Why: Import merge depends on globally unique ExprId values, so parser state is threaded through.
    // Why: Module disambiguator keeps diagnostics and generic naming stable across same-package files.
    let tokens = Lexer::new(source).lex_all();
    let mut parser = Parser::new_with_expr_id(tokens, next_expr_id);
    if let Some(path) = file_path {
        parser.set_module_disambiguator(path.to_string_lossy().into_owned());
    }
    let file = match parser.parse_file() {
        Some(file) => file,
        None => return Err(render_diags(&parser.diags, source, file_path)),
    };
    if !parser.diags.is_empty() {
        return Err(render_diags(&parser.diags, source, file_path));
    }
    Ok((file, parser.next_expr_id()))
}

fn load_imports(
    root: &Path,
    imports: &[ImportSpec],
    next_expr_id: &mut usize,
    items: &mut Vec<Item>,
    visited: &mut std::collections::HashSet<String>,
    std_funcs: &mut std::collections::HashSet<String>,
    build_ctx: &mut BuildCtx,
) -> Result<(), String> {
    // Precondition: `visited` is shared across the import walk to break cycles deterministically.
    // Postcondition: `items` receives imported declarations in a stable traversal order.
    // Side effects: Reads imported source files and mutates import accumulator state.
    for import in imports {
        if !visited.insert(import.path.clone()) {
            continue;
        }
        let is_std = import.path.starts_with("std/");
        let paths = resolve_import_paths(root, &import.path)?;
        for path in paths {
            let source = fs::read_to_string(&path).map_err(|e| e.to_string())?;
            let (file, next) = parse_source_with_ids(&source, *next_expr_id, Some(&path))
                .map_err(|e| format!("{}:\n{}", path.display(), e))?;
            if let Some(expected) = import.path.split('/').next_back()
                && file.package != expected
            {
                return Err(format!(
                    "import {}: package name mismatch (expected {}, found {})",
                    import.path, expected, file.package
                ));
            }
            *next_expr_id = next;
            build_ctx.record_source_file(&import.path, &path, &source, &file);
            let mut imported_items = file.items.clone();
            let nested_imports = file.imports.clone();
            if import.only.is_some() {
                imported_items.retain(|item| import_item_allowed(item, import));
            }
            if is_std {
                for item in &imported_items {
                    if let Item::Function(func) = item {
                        std_funcs.insert(func.name.clone());
                    }
                }
            }
            let wrappers = build_import_wrappers(&file, import, next_expr_id);
            *next_expr_id = next.max(*next_expr_id);
            items.extend(imported_items);
            items.extend(wrappers);
            load_imports(
                root,
                &nested_imports,
                next_expr_id,
                items,
                visited,
                std_funcs,
                build_ctx,
            )?;
        }
    }
    Ok(())
}

fn load_imports_mod(
    ctx: &resolve::ResolveCtx,
    imports: &[ImportSpec],
    next_expr_id: &mut usize,
    items: &mut Vec<Item>,
    visited: &mut std::collections::HashSet<String>,
    std_funcs: &mut std::collections::HashSet<String>,
    build_ctx: &mut BuildCtx,
) -> Result<(), String> {
    // Precondition: `ctx` was created from module resolution policy for this compile invocation.
    // Postcondition: Imported module files are merged with the same wrapper/alias rules as legacy path.
    // Side effects: Resolves packages via mod/cache layers and appends imported AST items.
    for import in imports {
        if !visited.insert(import.path.clone()) {
            continue;
        }
        let is_std = import.path.starts_with("std/");
        let pkg =
            resolve::resolve_import_to_package(ctx, &import.path).map_err(|e| e.to_string())?;
        for path in pkg.files {
            let source = fs::read_to_string(&path).map_err(|e| e.to_string())?;
            let (file, next) = parse_source_with_ids(&source, *next_expr_id, Some(&path))
                .map_err(|e| format!("{}:\n{}", path.display(), e))?;
            if let Some(expected) = import.path.split('/').next_back()
                && file.package != expected
            {
                return Err(format!(
                    "import {}: package name mismatch (expected {}, found {})",
                    import.path, expected, file.package
                ));
            }
            *next_expr_id = next;
            build_ctx.record_source_file(&import.path, &path, &source, &file);
            let mut imported_items = file.items.clone();
            if import.only.is_some() {
                imported_items.retain(|item| import_item_allowed(item, import));
            }
            if is_std {
                for item in &imported_items {
                    if let Item::Function(func) = item {
                        std_funcs.insert(func.name.clone());
                    }
                }
            }
            let nested_imports = file.imports.clone();
            let wrappers = build_import_wrappers(&file, import, next_expr_id);
            *next_expr_id = next.max(*next_expr_id);
            items.extend(imported_items);
            items.extend(wrappers);
            load_imports_mod(
                ctx,
                &nested_imports,
                next_expr_id,
                items,
                visited,
                std_funcs,
                build_ctx,
            )?;
        }
    }
    Ok(())
}

fn item_name(item: &Item) -> Option<&str> {
    match item {
        Item::Function(f) => Some(&f.name),
        Item::ExternGlobal(g) => Some(&g.name),
        Item::TypeAlias(a) => Some(&a.name),
        Item::Global(g) => Some(&g.name),
        Item::Const(c) => Some(&c.name),
        Item::Struct(s) => Some(&s.name),
        Item::Enum(e) => Some(&e.name),
    }
}

fn import_item_allowed(item: &Item, spec: &ImportSpec) -> bool {
    let Some(only) = &spec.only else {
        return true;
    };
    let Some(name) = item_name(item) else {
        return false;
    };
    only.iter().any(|n| n == name)
}

fn build_import_wrappers(file: &FileAst, spec: &ImportSpec, next_expr_id: &mut usize) -> Vec<Item> {
    let alias = spec.alias.clone().unwrap_or_else(|| file.package.clone());
    if alias.is_empty() {
        return Vec::new();
    }
    let mut wrappers = Vec::new();
    for item in &file.items {
        if !import_item_allowed(item, spec) {
            continue;
        }
        let Item::Function(func) = item else {
            continue;
        };
        if is_impl_mangled(&func.name) {
            continue;
        }
        if func.is_extern {
            continue;
        }
        if func.is_variadic {
            let mut aliased = func.clone();
            aliased.name = format!("{}.{}", alias, func.name);
            aliased.is_extern = false;
            aliased.extern_abi = None;
            refresh_block_ids(&mut aliased.body, next_expr_id);
            wrappers.push(Item::Function(aliased));
            continue;
        }
        let wrapper_type_args: Vec<TypeAst> = func
            .type_params
            .iter()
            .map(|param| TypeAst {
                kind: TypeAstKind::Named(param.name.clone()),
                span: func.span.clone(),
            })
            .collect();
        let mut args = Vec::with_capacity(func.params.len());
        for p in &func.params {
            let id = *next_expr_id;
            *next_expr_id += 1;
            args.push(Expr {
                id,
                kind: ExprKind::Ident(p.name.clone()),
                span: p.span.clone(),
            });
        }
        let callee_id = *next_expr_id;
        *next_expr_id += 1;
        let call_id = *next_expr_id;
        *next_expr_id += 1;
        let callee = Expr {
            id: callee_id,
            kind: ExprKind::Ident(func.name.clone()),
            span: func.span.clone(),
        };
        let call = Expr {
            id: call_id,
            kind: ExprKind::Call {
                callee: Box::new(callee),
                type_args: wrapper_type_args,
                args,
            },
            span: func.span.clone(),
        };
        let returns_unit = match &func.ret_type {
            None => true,
            Some(ty) => matches!(
                &ty.kind,
                crate::frontend::ast::TypeAstKind::Named(name) if name == "unit"
            ),
        };
        let body = if returns_unit {
            Block {
                stmts: vec![Stmt::Expr {
                    expr: call,
                    span: func.span.clone(),
                }],
                tail: None,
                span: func.span.clone(),
            }
        } else {
            Block {
                stmts: Vec::new(),
                tail: Some(call),
                span: func.span.clone(),
            }
        };
        wrappers.push(Item::Function(Function {
            vis: func.vis,
            name: format!("{}.{}", alias, func.name),
            type_params: func.type_params.clone(),
            params: func.params.clone(),
            is_variadic: false,
            ret_type: func.ret_type.clone(),
            is_extern: false,
            is_unsafe: func.is_unsafe,
            extern_abi: None,
            body,
            span: func.span.clone(),
        }));
    }
    wrappers
}

fn resolve_import_paths(root: &Path, import: &str) -> Result<Vec<PathBuf>, String> {
    let direct = root.join(import).with_extension("gs");
    if direct.is_file() {
        return Ok(vec![direct]);
    }
    let dir = root.join(import);
    if dir.is_dir() {
        let mut out = Vec::new();
        for entry in fs::read_dir(&dir).map_err(|e| e.to_string())? {
            let entry = entry.map_err(|e| e.to_string())?;
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("gs") {
                out.push(path);
            }
        }
        out.sort();
        if out.is_empty() {
            return Err(format!("import {}: no .gs files found", import));
        }
        return Ok(out);
    }
    Err(format!("import {} not found", import))
}

fn render_diags(diags: &Diagnostics, source: &str, file_path: Option<&Path>) -> String {
    let mut out = String::new();
    for diag in &diags.items {
        let name = file_path.map(|p| p.display().to_string());
        out.push_str(&format_diagnostic(diag, source, name.as_deref()));
        out.push('\n');
    }
    out
}

fn render_compile_stage_error(message: String, source: &str, file_path: Option<&Path>) -> String {
    let mut diag = Diagnostic::new(message.clone(), None);
    if message.starts_with("generic instantiation depth exceeded for `") {
        diag = diag
            .code(E_MONOMORPHIZATION_DEPTH_EXCEEDED)
            .help(format!(
                "recursive generic expansion exceeded the compiler limit ({})",
                MONOMORPHIZATION_TYPE_DEPTH_LIMIT
            ))
            .help(
                "reduce recursive type growth or introduce a non-recursive specialization boundary",
            );
    }
    let mut diags = Diagnostics::default();
    diags.push_diag(diag);
    render_diags(&diags, source, file_path)
}

fn lower_high_level_features(file: &mut FileAst, next_expr_id: &mut usize) -> Result<(), String> {
    // Why: Global init injection must run before monomorphization so generated assignments are rewritten too.
    inject_global_initializer_function(file, next_expr_id);
    monomorphize_and_rewrite(file)
}

fn inject_global_initializer_function(file: &mut FileAst, next_expr_id: &mut usize) {
    const GLOBAL_INIT_FN: &str = "__gost_global_init_user";
    let mut stmts = Vec::new();
    let mut first_span = None;
    for item in &file.items {
        let Item::Global(global) = item else {
            continue;
        };
        let mut value = global.init.clone();
        refresh_expr_ids(&mut value, next_expr_id);
        let target = fresh_ident_expr(global.name.clone(), global.span.clone(), next_expr_id);
        first_span.get_or_insert_with(|| global.span.clone());
        stmts.push(Stmt::Assign {
            op: AssignOp::Assign,
            target,
            value,
            span: global.span.clone(),
        });
    }
    if stmts.is_empty() {
        return;
    }
    if file
        .items
        .iter()
        .any(|item| matches!(item, Item::Function(func) if func.name == GLOBAL_INIT_FN))
    {
        return;
    }
    let span = first_span.unwrap_or(crate::frontend::ast::Span {
        start: 0,
        end: 0,
        line: 1,
        column: 1,
    });
    file.items.push(Item::Function(Function {
        vis: Visibility::Private,
        name: GLOBAL_INIT_FN.to_string(),
        type_params: Vec::new(),
        params: Vec::new(),
        is_variadic: false,
        ret_type: None,
        is_extern: false,
        is_unsafe: false,
        extern_abi: None,
        body: Block {
            stmts,
            tail: None,
            span: span.clone(),
        },
        span,
    }));
}

fn fresh_ident_expr(
    name: String,
    span: crate::frontend::ast::Span,
    next_expr_id: &mut usize,
) -> Expr {
    let id = *next_expr_id;
    *next_expr_id += 1;
    Expr {
        id,
        kind: ExprKind::Ident(name),
        span,
    }
}

fn refresh_block_ids(block: &mut Block, next_expr_id: &mut usize) {
    for stmt in &mut block.stmts {
        refresh_stmt_ids(stmt, next_expr_id);
    }
    if let Some(tail) = &mut block.tail {
        refresh_expr_ids(tail, next_expr_id);
    }
}

fn refresh_stmt_ids(stmt: &mut Stmt, next_expr_id: &mut usize) {
    match stmt {
        Stmt::Let { init, .. } | Stmt::Const { init, .. } => {
            refresh_expr_ids(init, next_expr_id);
        }
        Stmt::Assign { target, value, .. } => {
            refresh_expr_ids(target, next_expr_id);
            refresh_expr_ids(value, next_expr_id);
        }
        Stmt::Expr { expr, .. } | Stmt::Go { expr, .. } | Stmt::Defer { expr, .. } => {
            refresh_expr_ids(expr, next_expr_id);
        }
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                refresh_expr_ids(expr, next_expr_id);
            }
        }
        Stmt::While { cond, body, .. } => {
            refresh_expr_ids(cond, next_expr_id);
            refresh_block_ids(body, next_expr_id);
        }
        Stmt::Loop { body, .. } => refresh_block_ids(body, next_expr_id),
        Stmt::ForIn { iter, body, .. } => {
            refresh_expr_ids(iter, next_expr_id);
            refresh_block_ids(body, next_expr_id);
        }
        Stmt::ForRange {
            start, end, body, ..
        } => {
            refresh_expr_ids(start, next_expr_id);
            refresh_expr_ids(end, next_expr_id);
            refresh_block_ids(body, next_expr_id);
        }
        Stmt::Select { arms, .. } => {
            for arm in arms {
                match &mut arm.kind {
                    crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                        refresh_expr_ids(chan, next_expr_id);
                        refresh_expr_ids(value, next_expr_id);
                    }
                    crate::frontend::ast::SelectArmKind::Recv { chan, .. } => {
                        refresh_expr_ids(chan, next_expr_id);
                    }
                    crate::frontend::ast::SelectArmKind::After { ms } => {
                        refresh_expr_ids(ms, next_expr_id);
                    }
                    crate::frontend::ast::SelectArmKind::Default => {}
                }
                match &mut arm.body {
                    BlockOrExpr::Block(block) => refresh_block_ids(block, next_expr_id),
                    BlockOrExpr::Expr(expr) => refresh_expr_ids(expr, next_expr_id),
                }
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
}

fn refresh_expr_ids(expr: &mut Expr, next_expr_id: &mut usize) {
    expr.id = *next_expr_id;
    *next_expr_id += 1;
    match &mut expr.kind {
        ExprKind::StructLit { fields, .. } => {
            for (_, field_expr) in fields {
                refresh_expr_ids(field_expr, next_expr_id);
            }
        }
        ExprKind::ArrayLit(items) | ExprKind::Tuple(items) => {
            for item in items {
                refresh_expr_ids(item, next_expr_id);
            }
        }
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
            refresh_block_ids(block, next_expr_id);
        }
        ExprKind::If {
            cond,
            then_block,
            else_block,
        } => {
            refresh_expr_ids(cond, next_expr_id);
            refresh_block_ids(then_block, next_expr_id);
            if let Some(block) = else_block {
                refresh_block_ids(block, next_expr_id);
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            refresh_expr_ids(scrutinee, next_expr_id);
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    refresh_expr_ids(guard, next_expr_id);
                }
                match &mut arm.body {
                    BlockOrExpr::Block(block) => refresh_block_ids(block, next_expr_id),
                    BlockOrExpr::Expr(expr) => refresh_expr_ids(expr, next_expr_id),
                }
            }
        }
        ExprKind::Closure { body, .. } => match body.as_mut() {
            BlockOrExpr::Block(block) => refresh_block_ids(block, next_expr_id),
            BlockOrExpr::Expr(expr) => refresh_expr_ids(expr, next_expr_id),
        },
        ExprKind::Call {
            callee,
            type_args: _,
            args,
        } => {
            refresh_expr_ids(callee, next_expr_id);
            for arg in args {
                refresh_expr_ids(arg, next_expr_id);
            }
        }
        ExprKind::Field { base, .. } => refresh_expr_ids(base, next_expr_id),
        ExprKind::Index { base, index } => {
            refresh_expr_ids(base, next_expr_id);
            refresh_expr_ids(index, next_expr_id);
        }
        ExprKind::Unary { expr: inner, .. }
        | ExprKind::Cast { expr: inner, .. }
        | ExprKind::Borrow { expr: inner, .. }
        | ExprKind::Deref { expr: inner }
        | ExprKind::Try { expr: inner }
        | ExprKind::Recv { chan: inner }
        | ExprKind::Close { chan: inner }
        | ExprKind::After { ms: inner } => refresh_expr_ids(inner, next_expr_id),
        ExprKind::Binary { left, right, .. } => {
            refresh_expr_ids(left, next_expr_id);
            refresh_expr_ids(right, next_expr_id);
        }
        ExprKind::Send { chan, value } => {
            refresh_expr_ids(chan, next_expr_id);
            refresh_expr_ids(value, next_expr_id);
        }
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil
        | ExprKind::Ident(_) => {}
    }
}

const MONOMORPHIZATION_TYPE_DEPTH_LIMIT: usize = 64;

fn monomorphize_and_rewrite(file: &mut FileAst) -> Result<(), String> {
    // Precondition: Function/type declarations are parsed and sema will run after this rewrite stage.
    // Postcondition: Generic/template calls are concretized so backend sees only lowered callable forms.
    // Side effects: Replaces `file.items` with rewritten/instantiated function set and drops dead templates.
    let trait_methods = collect_trait_method_requirements(&file.items);
    let mut non_functions = Vec::new();
    let mut regular_functions = Vec::new();
    let mut templates: HashMap<String, Function> = HashMap::new();

    for item in std::mem::take(&mut file.items) {
        match item {
            Item::Function(func) if !func.type_params.is_empty() => {
                templates.insert(func.name.clone(), func);
            }
            Item::Function(func) => regular_functions.push(func),
            other => non_functions.push(other),
        }
    }

    let mut known_functions: HashMap<String, Function> = HashMap::new();
    for func in &regular_functions {
        known_functions.insert(func.name.clone(), func.clone());
    }
    for (name, func) in &templates {
        known_functions.insert(name.clone(), func.clone());
    }

    let closure_factory_names: HashSet<String> = regular_functions
        .iter()
        .filter_map(|func| match callable_return_source(func) {
            Some(CallableReturnSource::Closure { .. }) => Some(func.name.clone()),
            _ => None,
        })
        .collect();

    let mut queue: VecDeque<Function> = regular_functions.into_iter().collect();
    let mut lowered_functions = Vec::new();
    let mut instances: HashMap<String, String> = HashMap::new();

    while let Some(mut func) = queue.pop_front() {
        let ret_expected = func.ret_type.clone();
        let mut initial_locals = HashSet::new();
        let mut initial_local_types = HashMap::new();
        for param in &func.params {
            initial_locals.insert(param.name.clone());
            initial_local_types.insert(param.name.clone(), param.ty.clone());
        }
        rewrite_block_high_level(
            &mut func.body,
            &templates,
            &trait_methods,
            &mut instances,
            &mut queue,
            &known_functions,
            &initial_locals,
            &initial_local_types,
            ret_expected.as_ref(),
        )?;
        lowered_functions.push(func);
    }

    let called = collect_direct_called_functions(&lowered_functions);
    lowered_functions.retain(|func| {
        if is_impl_mangled(&func.name) {
            return true;
        }
        if func.name == "main" || called.contains(&func.name) {
            return true;
        }
        if closure_factory_names.contains(&func.name) {
            return false;
        }
        let returns_callable = func.ret_type.as_ref().is_some_and(is_callable_type_ast);
        !(returns_callable && function_contains_closure_expr(func))
    });
    file.items = non_functions;
    for func in lowered_functions {
        file.items.push(Item::Function(func));
    }
    Ok(())
}

fn collect_direct_called_functions(functions: &[Function]) -> HashSet<String> {
    let mut out = HashSet::new();
    for func in functions {
        collect_called_functions_in_block(&func.body, &mut out);
    }
    out
}

fn collect_called_functions_in_block(block: &Block, out: &mut HashSet<String>) {
    for stmt in &block.stmts {
        collect_called_functions_in_stmt(stmt, out);
    }
    if let Some(tail) = &block.tail {
        collect_called_functions_in_expr(tail, out);
    }
}

fn collect_called_functions_in_stmt(stmt: &Stmt, out: &mut HashSet<String>) {
    match stmt {
        Stmt::Let { init, .. } | Stmt::Const { init, .. } => {
            collect_called_functions_in_expr(init, out)
        }
        Stmt::Assign { target, value, .. } => {
            collect_called_functions_in_expr(target, out);
            collect_called_functions_in_expr(value, out);
        }
        Stmt::Expr { expr, .. } | Stmt::Go { expr, .. } | Stmt::Defer { expr, .. } => {
            collect_called_functions_in_expr(expr, out);
        }
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                collect_called_functions_in_expr(expr, out);
            }
        }
        Stmt::While { cond, body, .. } => {
            collect_called_functions_in_expr(cond, out);
            collect_called_functions_in_block(body, out);
        }
        Stmt::Loop { body, .. } => collect_called_functions_in_block(body, out),
        Stmt::ForIn { iter, body, .. } => {
            collect_called_functions_in_expr(iter, out);
            collect_called_functions_in_block(body, out);
        }
        Stmt::ForRange {
            start, end, body, ..
        } => {
            collect_called_functions_in_expr(start, out);
            collect_called_functions_in_expr(end, out);
            collect_called_functions_in_block(body, out);
        }
        Stmt::Select { arms, .. } => {
            for arm in arms {
                match &arm.kind {
                    crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                        collect_called_functions_in_expr(chan, out);
                        collect_called_functions_in_expr(value, out);
                    }
                    crate::frontend::ast::SelectArmKind::Recv { chan, .. } => {
                        collect_called_functions_in_expr(chan, out);
                    }
                    crate::frontend::ast::SelectArmKind::After { ms } => {
                        collect_called_functions_in_expr(ms, out);
                    }
                    crate::frontend::ast::SelectArmKind::Default => {}
                }
                match &arm.body {
                    BlockOrExpr::Block(block) => collect_called_functions_in_block(block, out),
                    BlockOrExpr::Expr(expr) => collect_called_functions_in_expr(expr, out),
                }
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
}

fn collect_called_functions_in_expr(expr: &Expr, out: &mut HashSet<String>) {
    match &expr.kind {
        ExprKind::Call { callee, args, .. } => {
            match &callee.kind {
                ExprKind::Ident(name) => {
                    out.insert(name.clone());
                }
                ExprKind::Field { base, name } => {
                    if let ExprKind::Ident(pkg) = &base.kind {
                        out.insert(format!("{}.{}", pkg, name));
                    }
                }
                _ => {}
            }
            collect_called_functions_in_expr(callee, out);
            for arg in args {
                collect_called_functions_in_expr(arg, out);
            }
        }
        ExprKind::StructLit { fields, .. } => {
            for (_, field_expr) in fields {
                collect_called_functions_in_expr(field_expr, out);
            }
        }
        ExprKind::ArrayLit(items) | ExprKind::Tuple(items) => {
            for item in items {
                collect_called_functions_in_expr(item, out);
            }
        }
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
            collect_called_functions_in_block(block, out);
        }
        ExprKind::If {
            cond,
            then_block,
            else_block,
        } => {
            collect_called_functions_in_expr(cond, out);
            collect_called_functions_in_block(then_block, out);
            if let Some(block) = else_block {
                collect_called_functions_in_block(block, out);
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            collect_called_functions_in_expr(scrutinee, out);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_called_functions_in_expr(guard, out);
                }
                match &arm.body {
                    BlockOrExpr::Block(block) => collect_called_functions_in_block(block, out),
                    BlockOrExpr::Expr(expr) => collect_called_functions_in_expr(expr, out),
                }
            }
        }
        ExprKind::Closure { body, .. } => match body.as_ref() {
            BlockOrExpr::Block(block) => collect_called_functions_in_block(block, out),
            BlockOrExpr::Expr(expr) => collect_called_functions_in_expr(expr, out),
        },
        ExprKind::Field { base, .. }
        | ExprKind::Unary { expr: base, .. }
        | ExprKind::Cast { expr: base, .. }
        | ExprKind::Borrow { expr: base, .. }
        | ExprKind::Deref { expr: base }
        | ExprKind::Try { expr: base }
        | ExprKind::Recv { chan: base }
        | ExprKind::Close { chan: base }
        | ExprKind::After { ms: base } => collect_called_functions_in_expr(base, out),
        ExprKind::Index { base, index } => {
            collect_called_functions_in_expr(base, out);
            collect_called_functions_in_expr(index, out);
        }
        ExprKind::Binary { left, right, .. } => {
            collect_called_functions_in_expr(left, out);
            collect_called_functions_in_expr(right, out);
        }
        ExprKind::Send { chan, value } => {
            collect_called_functions_in_expr(chan, out);
            collect_called_functions_in_expr(value, out);
        }
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil
        | ExprKind::Ident(_) => {}
    }
}

fn function_contains_closure_expr(func: &Function) -> bool {
    block_contains_closure_expr(&func.body)
}

fn block_contains_closure_expr(block: &Block) -> bool {
    for stmt in &block.stmts {
        if stmt_contains_closure_expr(stmt) {
            return true;
        }
    }
    block.tail.as_ref().is_some_and(expr_contains_closure_expr)
}

fn stmt_contains_closure_expr(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Let { init, .. } | Stmt::Const { init, .. } => expr_contains_closure_expr(init),
        Stmt::Assign { target, value, .. } => {
            expr_contains_closure_expr(target) || expr_contains_closure_expr(value)
        }
        Stmt::Expr { expr, .. } | Stmt::Go { expr, .. } | Stmt::Defer { expr, .. } => {
            expr_contains_closure_expr(expr)
        }
        Stmt::Return { expr, .. } => expr.as_ref().is_some_and(expr_contains_closure_expr),
        Stmt::While { cond, body, .. } => {
            expr_contains_closure_expr(cond) || block_contains_closure_expr(body)
        }
        Stmt::Loop { body, .. } => block_contains_closure_expr(body),
        Stmt::ForIn { iter, body, .. } => {
            expr_contains_closure_expr(iter) || block_contains_closure_expr(body)
        }
        Stmt::ForRange {
            start, end, body, ..
        } => {
            expr_contains_closure_expr(start)
                || expr_contains_closure_expr(end)
                || block_contains_closure_expr(body)
        }
        Stmt::Select { arms, .. } => arms.iter().any(|arm| {
            let kind_has = match &arm.kind {
                crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                    expr_contains_closure_expr(chan) || expr_contains_closure_expr(value)
                }
                crate::frontend::ast::SelectArmKind::Recv { chan, .. } => {
                    expr_contains_closure_expr(chan)
                }
                crate::frontend::ast::SelectArmKind::After { ms } => expr_contains_closure_expr(ms),
                crate::frontend::ast::SelectArmKind::Default => false,
            };
            kind_has
                || match &arm.body {
                    BlockOrExpr::Block(block) => block_contains_closure_expr(block),
                    BlockOrExpr::Expr(expr) => expr_contains_closure_expr(expr),
                }
        }),
        Stmt::Break { .. } | Stmt::Continue { .. } => false,
    }
}

fn expr_contains_closure_expr(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Closure { .. } => true,
        ExprKind::Call { callee, args, .. } => {
            expr_contains_closure_expr(callee) || args.iter().any(expr_contains_closure_expr)
        }
        ExprKind::StructLit { fields, .. } => fields
            .iter()
            .any(|(_, field_expr)| expr_contains_closure_expr(field_expr)),
        ExprKind::ArrayLit(items) | ExprKind::Tuple(items) => {
            items.iter().any(expr_contains_closure_expr)
        }
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => block_contains_closure_expr(block),
        ExprKind::If {
            cond,
            then_block,
            else_block,
        } => {
            expr_contains_closure_expr(cond)
                || block_contains_closure_expr(then_block)
                || else_block
                    .as_ref()
                    .is_some_and(|block| block_contains_closure_expr(block))
        }
        ExprKind::Match { scrutinee, arms } => {
            expr_contains_closure_expr(scrutinee)
                || arms.iter().any(|arm| {
                    arm.guard.as_ref().is_some_and(expr_contains_closure_expr)
                        || match &arm.body {
                            BlockOrExpr::Block(block) => block_contains_closure_expr(block),
                            BlockOrExpr::Expr(expr) => expr_contains_closure_expr(expr),
                        }
                })
        }
        ExprKind::Field { base, .. }
        | ExprKind::Unary { expr: base, .. }
        | ExprKind::Cast { expr: base, .. }
        | ExprKind::Borrow { expr: base, .. }
        | ExprKind::Deref { expr: base }
        | ExprKind::Try { expr: base }
        | ExprKind::Recv { chan: base }
        | ExprKind::Close { chan: base }
        | ExprKind::After { ms: base } => expr_contains_closure_expr(base),
        ExprKind::Index { base, index } => {
            expr_contains_closure_expr(base) || expr_contains_closure_expr(index)
        }
        ExprKind::Binary { left, right, .. } => {
            expr_contains_closure_expr(left) || expr_contains_closure_expr(right)
        }
        ExprKind::Send { chan, value } => {
            expr_contains_closure_expr(chan) || expr_contains_closure_expr(value)
        }
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil
        | ExprKind::Ident(_) => false,
    }
}

fn rewrite_block_high_level(
    block: &mut Block,
    templates: &HashMap<String, Function>,
    trait_methods: &HashMap<String, Vec<TraitMethod>>,
    instances: &mut HashMap<String, String>,
    queue: &mut VecDeque<Function>,
    known_functions: &HashMap<String, Function>,
    inherited_locals: &HashSet<String>,
    inherited_local_types: &HashMap<String, TypeAst>,
    ret_expected: Option<&TypeAst>,
) -> Result<(), String> {
    let closure_env = HashMap::new();
    let fn_alias_env = HashMap::new();
    rewrite_block_high_level_in_env(
        block,
        templates,
        trait_methods,
        instances,
        queue,
        known_functions,
        &closure_env,
        &fn_alias_env,
        inherited_locals,
        inherited_local_types,
        ret_expected,
    )
}

#[derive(Clone)]
struct ClosureBinding {
    params: Vec<ClosureParam>,
    body: BlockOrExpr,
    span: crate::frontend::ast::Span,
    expr_id: crate::frontend::ast::ExprId,
    enclosing_locals: HashSet<String>,
}

fn rewrite_block_high_level_in_env(
    block: &mut Block,
    templates: &HashMap<String, Function>,
    trait_methods: &HashMap<String, Vec<TraitMethod>>,
    instances: &mut HashMap<String, String>,
    queue: &mut VecDeque<Function>,
    known_functions: &HashMap<String, Function>,
    inherited_closures: &HashMap<String, ClosureBinding>,
    inherited_fn_aliases: &HashMap<String, String>,
    inherited_locals: &HashSet<String>,
    inherited_local_types: &HashMap<String, TypeAst>,
    ret_expected: Option<&TypeAst>,
) -> Result<(), String> {
    let mut closure_env = inherited_closures.clone();
    let mut fn_alias_env = inherited_fn_aliases.clone();
    let mut local_env = inherited_locals.clone();
    let mut local_type_env = inherited_local_types.clone();
    let mut rewritten_stmts = Vec::with_capacity(block.stmts.len());
    for mut stmt in std::mem::take(&mut block.stmts) {
        rewrite_stmt_high_level(
            &mut stmt,
            templates,
            trait_methods,
            instances,
            queue,
            known_functions,
            &closure_env,
            &fn_alias_env,
            &local_env,
            &local_type_env,
            ret_expected,
        )?;
        if let Some(prelude) = extract_closure_init_prelude(&mut stmt) {
            for prep_stmt in prelude {
                rewritten_stmts.push(prep_stmt.clone());
                let _ = update_closure_env_for_stmt(&prep_stmt, &mut closure_env, &local_env);
                update_fn_alias_env_for_stmt(&prep_stmt, &mut fn_alias_env, known_functions);
                update_local_env_for_stmt(&prep_stmt, &mut local_env);
                update_local_types_for_stmt(
                    &prep_stmt,
                    &mut local_type_env,
                    templates,
                    known_functions,
                );
            }
        }
        let keep_stmt = update_closure_env_for_stmt(&stmt, &mut closure_env, &local_env);
        update_fn_alias_env_for_stmt(&stmt, &mut fn_alias_env, known_functions);
        update_local_env_for_stmt(&stmt, &mut local_env);
        update_local_types_for_stmt(&stmt, &mut local_type_env, templates, known_functions);
        if keep_stmt {
            rewritten_stmts.push(stmt);
        }
    }
    block.stmts = rewritten_stmts;
    if let Some(tail) = &mut block.tail {
        rewrite_expr_high_level(
            tail,
            templates,
            trait_methods,
            instances,
            queue,
            known_functions,
            &closure_env,
            &fn_alias_env,
            &local_env,
            &local_type_env,
            ret_expected,
            ret_expected,
        )?;
    }
    if block_contains_closure_expr(block) {
        sanitize_remaining_closure_literals_in_block(
            block,
            templates,
            known_functions,
            &local_type_env,
            instances,
            queue,
        )?;
    }
    Ok(())
}

fn sanitize_remaining_closure_literals_in_stmt(
    stmt: &mut Stmt,
    templates: &HashMap<String, Function>,
    known_functions: &HashMap<String, Function>,
    local_types_in_scope: &HashMap<String, TypeAst>,
    instances: &mut HashMap<String, String>,
    queue: &mut VecDeque<Function>,
) -> Result<(), String> {
    match stmt {
        Stmt::Let { init, .. } | Stmt::Const { init, .. } => {
            sanitize_remaining_closure_literals_in_expr(
                init,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
        }
        Stmt::Assign { target, value, .. } => {
            sanitize_remaining_closure_literals_in_expr(
                target,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
            sanitize_remaining_closure_literals_in_expr(
                value,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
        }
        Stmt::Expr { expr, .. } | Stmt::Go { expr, .. } | Stmt::Defer { expr, .. } => {
            sanitize_remaining_closure_literals_in_expr(
                expr,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
        }
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                sanitize_remaining_closure_literals_in_expr(
                    expr,
                    templates,
                    known_functions,
                    local_types_in_scope,
                    instances,
                    queue,
                )?;
            }
        }
        Stmt::While { cond, body, .. } => {
            sanitize_remaining_closure_literals_in_expr(
                cond,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
            sanitize_remaining_closure_literals_in_block(
                body,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
        }
        Stmt::Loop { body, .. } => {
            sanitize_remaining_closure_literals_in_block(
                body,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
        }
        Stmt::ForIn { iter, body, .. } => {
            sanitize_remaining_closure_literals_in_expr(
                iter,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
            sanitize_remaining_closure_literals_in_block(
                body,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
        }
        Stmt::ForRange {
            start, end, body, ..
        } => {
            sanitize_remaining_closure_literals_in_expr(
                start,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
            sanitize_remaining_closure_literals_in_expr(
                end,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
            sanitize_remaining_closure_literals_in_block(
                body,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
        }
        Stmt::Select { arms, .. } => {
            for arm in arms {
                match &mut arm.kind {
                    crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                        sanitize_remaining_closure_literals_in_expr(
                            chan,
                            templates,
                            known_functions,
                            local_types_in_scope,
                            instances,
                            queue,
                        )?;
                        sanitize_remaining_closure_literals_in_expr(
                            value,
                            templates,
                            known_functions,
                            local_types_in_scope,
                            instances,
                            queue,
                        )?;
                    }
                    crate::frontend::ast::SelectArmKind::Recv { chan, .. } => {
                        sanitize_remaining_closure_literals_in_expr(
                            chan,
                            templates,
                            known_functions,
                            local_types_in_scope,
                            instances,
                            queue,
                        )?;
                    }
                    crate::frontend::ast::SelectArmKind::After { ms } => {
                        sanitize_remaining_closure_literals_in_expr(
                            ms,
                            templates,
                            known_functions,
                            local_types_in_scope,
                            instances,
                            queue,
                        )?;
                    }
                    crate::frontend::ast::SelectArmKind::Default => {}
                }
                match &mut arm.body {
                    BlockOrExpr::Block(block) => sanitize_remaining_closure_literals_in_block(
                        block,
                        templates,
                        known_functions,
                        local_types_in_scope,
                        instances,
                        queue,
                    )?,
                    BlockOrExpr::Expr(expr) => sanitize_remaining_closure_literals_in_expr(
                        expr,
                        templates,
                        known_functions,
                        local_types_in_scope,
                        instances,
                        queue,
                    )?,
                }
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
    Ok(())
}

fn sanitize_remaining_closure_literals_in_block(
    block: &mut Block,
    templates: &HashMap<String, Function>,
    known_functions: &HashMap<String, Function>,
    local_types_in_scope: &HashMap<String, TypeAst>,
    instances: &mut HashMap<String, String>,
    queue: &mut VecDeque<Function>,
) -> Result<(), String> {
    for stmt in &mut block.stmts {
        if stmt_contains_closure_expr(stmt) {
            sanitize_remaining_closure_literals_in_stmt(
                stmt,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
        }
    }
    if let Some(tail) = &mut block.tail
        && expr_contains_closure_expr(tail)
    {
        sanitize_remaining_closure_literals_in_expr(
            tail,
            templates,
            known_functions,
            local_types_in_scope,
            instances,
            queue,
        )?;
    }
    Ok(())
}

fn sanitize_remaining_closure_literals_in_expr(
    expr: &mut Expr,
    templates: &HashMap<String, Function>,
    known_functions: &HashMap<String, Function>,
    local_types_in_scope: &HashMap<String, TypeAst>,
    instances: &mut HashMap<String, String>,
    queue: &mut VecDeque<Function>,
) -> Result<(), String> {
    match &mut expr.kind {
        ExprKind::StructLit { fields, .. } => {
            for (_, field_expr) in fields {
                sanitize_remaining_closure_literals_in_expr(
                    field_expr,
                    templates,
                    known_functions,
                    local_types_in_scope,
                    instances,
                    queue,
                )?;
            }
        }
        ExprKind::ArrayLit(items) | ExprKind::Tuple(items) => {
            for item in items {
                sanitize_remaining_closure_literals_in_expr(
                    item,
                    templates,
                    known_functions,
                    local_types_in_scope,
                    instances,
                    queue,
                )?;
            }
        }
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
            sanitize_remaining_closure_literals_in_block(
                block,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
        }
        ExprKind::If {
            cond,
            then_block,
            else_block,
        } => {
            sanitize_remaining_closure_literals_in_expr(
                cond,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
            sanitize_remaining_closure_literals_in_block(
                then_block,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
            if let Some(else_block) = else_block {
                sanitize_remaining_closure_literals_in_block(
                    else_block,
                    templates,
                    known_functions,
                    local_types_in_scope,
                    instances,
                    queue,
                )?;
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            sanitize_remaining_closure_literals_in_expr(
                scrutinee,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    sanitize_remaining_closure_literals_in_expr(
                        guard,
                        templates,
                        known_functions,
                        local_types_in_scope,
                        instances,
                        queue,
                    )?;
                }
                match &mut arm.body {
                    BlockOrExpr::Block(block) => sanitize_remaining_closure_literals_in_block(
                        block,
                        templates,
                        known_functions,
                        local_types_in_scope,
                        instances,
                        queue,
                    )?,
                    BlockOrExpr::Expr(expr) => sanitize_remaining_closure_literals_in_expr(
                        expr,
                        templates,
                        known_functions,
                        local_types_in_scope,
                        instances,
                        queue,
                    )?,
                }
            }
        }
        ExprKind::Call { callee, args, .. } => {
            sanitize_remaining_closure_literals_in_expr(
                callee,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
            for arg in args {
                sanitize_remaining_closure_literals_in_expr(
                    arg,
                    templates,
                    known_functions,
                    local_types_in_scope,
                    instances,
                    queue,
                )?;
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
            sanitize_remaining_closure_literals_in_expr(
                base,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
        }
        ExprKind::Index { base, index } => {
            sanitize_remaining_closure_literals_in_expr(
                base,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
            sanitize_remaining_closure_literals_in_expr(
                index,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
        }
        ExprKind::Binary { left, right, .. } => {
            sanitize_remaining_closure_literals_in_expr(
                left,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
            sanitize_remaining_closure_literals_in_expr(
                right,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
        }
        ExprKind::Send { chan, value } => {
            sanitize_remaining_closure_literals_in_expr(
                chan,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
            sanitize_remaining_closure_literals_in_expr(
                value,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            )?;
        }
        ExprKind::Closure { .. } => {
            if let Some(runtime_expr) = ensure_runtime_closure_object_expr(
                expr,
                templates,
                known_functions,
                local_types_in_scope,
                instances,
                queue,
            ) {
                *expr = runtime_expr;
            } else {
                return Err(
                    "internal: failed to lower closure literal to runtime object".to_string(),
                );
            }
        }
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil
        | ExprKind::Ident(_) => {}
    }
    Ok(())
}

fn closure_capture_projection_expr(
    idx: usize,
    span: &crate::frontend::ast::Span,
    id_seed: usize,
) -> Expr {
    let base_id = id_seed.wrapping_add(idx.saturating_mul(8).saturating_add(1));
    let idx_id = id_seed.wrapping_add(idx.saturating_mul(8).saturating_add(2));
    Expr {
        id: id_seed.wrapping_add(idx.saturating_mul(8).saturating_add(3)),
        kind: ExprKind::Index {
            base: Box::new(Expr {
                id: base_id,
                kind: ExprKind::Ident("self".to_string()),
                span: span.clone(),
            }),
            index: Box::new(Expr {
                id: idx_id,
                kind: ExprKind::Int(idx.to_string()),
                span: span.clone(),
            }),
        },
        span: span.clone(),
    }
}

fn ensure_runtime_closure_object_expr(
    closure_expr: &Expr,
    templates: &HashMap<String, Function>,
    known_functions: &HashMap<String, Function>,
    local_types_in_scope: &HashMap<String, TypeAst>,
    instances: &mut HashMap<String, String>,
    queue: &mut VecDeque<Function>,
) -> Option<Expr> {
    let ExprKind::Closure { params, body } = &closure_expr.kind else {
        return None;
    };
    let inferred = infer_expr_type_ast(
        closure_expr,
        local_types_in_scope,
        templates,
        known_functions,
    )?;
    let TypeAstKind::Closure {
        params: inferred_params,
        ret: inferred_ret,
        is_variadic,
    } = &inferred.kind
    else {
        return None;
    };
    if inferred_params.len() != params.len() {
        return None;
    }
    let method_name =
        closure_runtime_method_name(inferred_params, inferred_ret.as_ref(), *is_variadic);
    let binding = ClosureBinding {
        params: params.clone(),
        body: body.as_ref().clone(),
        span: closure_expr.span.clone(),
        expr_id: closure_expr.id,
        enclosing_locals: local_types_in_scope.keys().cloned().collect(),
    };
    let captures = closure_captured_outer_locals(&binding);
    let mut capture_types = Vec::with_capacity(captures.len());
    for cap in &captures {
        let ty = local_types_in_scope.get(cap).cloned()?;
        capture_types.push(ty);
    }

    let env_type = TypeAst {
        kind: TypeAstKind::Tuple(capture_types),
        span: closure_expr.span.clone(),
    };
    let env_type_key = type_ast_key(&env_type);
    let cache_key = format!(
        "__closure_runtime_method__{}__{}__{}",
        closure_expr.id, method_name, env_type_key
    );
    let method_symbol = if let Some(existing) = instances.get(&cache_key) {
        existing.clone()
    } else {
        let mut fn_params = Vec::with_capacity(params.len() + 1);
        fn_params.push(Param {
            name: "self".to_string(),
            ty: env_type.clone(),
            span: closure_expr.span.clone(),
        });
        for (idx, param) in params.iter().enumerate() {
            let ty = param
                .ty
                .clone()
                .unwrap_or_else(|| inferred_params[idx].clone());
            fn_params.push(Param {
                name: param.name.clone(),
                ty,
                span: param.span.clone(),
            });
        }

        let mut method_body = match body.as_ref() {
            BlockOrExpr::Block(block) => block.as_ref().clone(),
            BlockOrExpr::Expr(expr) => Block {
                stmts: Vec::new(),
                tail: Some(expr.clone()),
                span: expr.span.clone(),
            },
        };
        let mut subst = HashMap::new();
        let projection_seed = closure_expr.id.wrapping_mul(131_071).wrapping_add(97_531);
        for (idx, cap) in captures.iter().enumerate() {
            subst.insert(
                cap.clone(),
                closure_capture_projection_expr(idx, &closure_expr.span, projection_seed),
            );
        }
        substitute_free_idents_with_exprs_in_block(&mut method_body, &subst);

        let recv_name = format!("closure_env_{}_{}", closure_expr.id, captures.len());
        let method_symbol = mangle_impl_method("__gost_closure_runtime", &recv_name, &method_name);
        queue.push_back(Function {
            vis: Visibility::Private,
            name: method_symbol.clone(),
            type_params: Vec::new(),
            params: fn_params,
            is_variadic: *is_variadic,
            ret_type: Some(inferred_ret.as_ref().clone()),
            is_extern: false,
            is_unsafe: false,
            extern_abi: None,
            body: method_body,
            span: closure_expr.span.clone(),
        });
        instances.insert(cache_key, method_symbol.clone());
        method_symbol
    };

    let mut env_items = Vec::with_capacity(captures.len());
    let expr_seed = closure_expr.id.wrapping_mul(65_537).wrapping_add(19_997);
    for (idx, cap) in captures.iter().enumerate() {
        env_items.push(Expr {
            id: expr_seed.wrapping_add(idx.saturating_mul(5)),
            kind: ExprKind::Ident(cap.clone()),
            span: closure_expr.span.clone(),
        });
    }
    let env_expr = Expr {
        id: expr_seed.wrapping_add(4096),
        kind: ExprKind::Tuple(env_items),
        span: closure_expr.span.clone(),
    };
    let runtime_obj_expr = Expr {
        id: expr_seed.wrapping_add(8192),
        kind: ExprKind::Cast {
            expr: Box::new(env_expr),
            ty: TypeAst {
                kind: TypeAstKind::Closure {
                    params: inferred_params.clone(),
                    ret: Box::new(inferred_ret.as_ref().clone()),
                    is_variadic: *is_variadic,
                },
                span: closure_expr.span.clone(),
            },
        },
        span: closure_expr.span.clone(),
    };

    let _ = method_symbol;
    Some(runtime_obj_expr)
}

fn extract_closure_init_prelude(stmt: &mut Stmt) -> Option<Vec<Stmt>> {
    let init = match stmt {
        Stmt::Let { init, .. } | Stmt::Const { init, .. } => init,
        _ => return None,
    };
    let block = match &mut init.kind {
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => block,
        _ => return None,
    };
    let tail = block.tail.as_ref()?;
    if !matches!(tail.kind, ExprKind::Closure { .. }) {
        return None;
    }
    let prelude = std::mem::take(&mut block.stmts);
    if prelude.is_empty() {
        return None;
    }
    let tail_expr = block.tail.take()?;
    *init = tail_expr;
    Some(prelude)
}

fn update_fn_alias_env_for_stmt(
    stmt: &Stmt,
    fn_alias_env: &mut HashMap<String, String>,
    known_functions: &HashMap<String, Function>,
) {
    match stmt {
        Stmt::Let { name, init, .. } | Stmt::Const { name, init, .. } => {
            if let ExprKind::Ident(src_name) = &init.kind {
                if known_functions.contains_key(src_name) {
                    fn_alias_env.insert(name.clone(), src_name.clone());
                } else if let Some(target) = fn_alias_env.get(src_name).cloned() {
                    fn_alias_env.insert(name.clone(), target);
                } else {
                    fn_alias_env.remove(name);
                }
            } else {
                fn_alias_env.remove(name);
            }
        }
        Stmt::Assign { target, .. } => {
            if let ExprKind::Ident(name) = &target.kind {
                fn_alias_env.remove(name);
            }
        }
        _ => {}
    }
}

fn update_closure_env_for_stmt(
    stmt: &Stmt,
    closure_env: &mut HashMap<String, ClosureBinding>,
    locals_in_scope: &HashSet<String>,
) -> bool {
    fn remove_bindings_for_base(closure_env: &mut HashMap<String, ClosureBinding>, base: &str) {
        closure_env.remove(base);
        let prefix = format!("{}.", base);
        closure_env.retain(|name, _| !name.starts_with(&prefix));
    }

    match stmt {
        Stmt::Let { name, ty, init, .. } | Stmt::Const { name, ty, init, .. } => {
            let requires_fnptr_rewrite = ty
                .as_ref()
                .is_some_and(|annot| matches!(annot.kind, TypeAstKind::FnPtr { .. }));
            remove_bindings_for_base(closure_env, name);
            if let ExprKind::Closure { params, body } = &init.kind {
                closure_env.insert(
                    name.clone(),
                    ClosureBinding {
                        params: params.clone(),
                        body: body.as_ref().clone(),
                        span: init.span.clone(),
                        expr_id: init.id,
                        enclosing_locals: locals_in_scope.clone(),
                    },
                );
                !requires_fnptr_rewrite
            } else if let ExprKind::Ident(src_name) = &init.kind {
                if let Some(binding) = closure_env.get(src_name).cloned() {
                    closure_env.insert(name.clone(), binding);
                    return !requires_fnptr_rewrite;
                }
                true
            } else if let ExprKind::StructLit { fields, .. } = &init.kind {
                if fields.is_empty() {
                    return true;
                }
                for (field_name, field_expr) in fields {
                    let key = format!("{}.{}", name, field_name);
                    match &field_expr.kind {
                        ExprKind::Closure { params, body } => {
                            closure_env.insert(
                                key,
                                ClosureBinding {
                                    params: params.clone(),
                                    body: body.as_ref().clone(),
                                    span: field_expr.span.clone(),
                                    expr_id: field_expr.id,
                                    enclosing_locals: locals_in_scope.clone(),
                                },
                            );
                        }
                        ExprKind::Ident(src_name) => {
                            if let Some(binding) = closure_env.get(src_name).cloned() {
                                closure_env.insert(key, binding);
                            } else {
                                closure_env.remove(&key);
                            }
                        }
                        _ => {
                            closure_env.remove(&key);
                        }
                    }
                }
                true
            } else {
                true
            }
        }
        Stmt::Assign { target, .. } => {
            if let ExprKind::Ident(name) = &target.kind {
                remove_bindings_for_base(closure_env, name);
            }
            true
        }
        _ => true,
    }
}

fn update_local_env_for_stmt(stmt: &Stmt, local_env: &mut HashSet<String>) {
    match stmt {
        Stmt::Let { name, .. } | Stmt::Const { name, .. } => {
            local_env.insert(name.clone());
        }
        _ => {}
    }
}

fn update_local_types_for_stmt(
    stmt: &Stmt,
    local_types: &mut HashMap<String, TypeAst>,
    templates: &HashMap<String, Function>,
    known_functions: &HashMap<String, Function>,
) {
    match stmt {
        Stmt::Let { name, ty, init, .. } | Stmt::Const { name, ty, init, .. } => {
            if let Some(ty) = ty {
                local_types.insert(name.clone(), ty.clone());
            } else if let ExprKind::Cast { ty: cast_ty, .. } = &init.kind {
                local_types.insert(name.clone(), cast_ty.clone());
            } else if let Some(inferred_ty) =
                infer_expr_type_ast(init, local_types, templates, known_functions)
            {
                local_types.insert(name.clone(), inferred_ty);
            } else {
                local_types.remove(name);
            }
        }
        _ => {}
    }
}

fn rewrite_block_or_expr_high_level(
    body: &mut BlockOrExpr,
    templates: &HashMap<String, Function>,
    trait_methods: &HashMap<String, Vec<TraitMethod>>,
    instances: &mut HashMap<String, String>,
    queue: &mut VecDeque<Function>,
    known_functions: &HashMap<String, Function>,
    closure_env: &HashMap<String, ClosureBinding>,
    fn_alias_env: &HashMap<String, String>,
    locals_in_scope: &HashSet<String>,
    local_types_in_scope: &HashMap<String, TypeAst>,
    ret_expected: Option<&TypeAst>,
    expected: Option<&TypeAst>,
) -> Result<(), String> {
    match body {
        BlockOrExpr::Block(block) => rewrite_block_high_level_in_env(
            block,
            templates,
            trait_methods,
            instances,
            queue,
            known_functions,
            closure_env,
            fn_alias_env,
            locals_in_scope,
            local_types_in_scope,
            ret_expected,
        ),
        BlockOrExpr::Expr(expr) => rewrite_expr_high_level(
            expr,
            templates,
            trait_methods,
            instances,
            queue,
            known_functions,
            closure_env,
            fn_alias_env,
            locals_in_scope,
            local_types_in_scope,
            ret_expected,
            expected,
        ),
    }
}

fn rewrite_stmt_high_level(
    stmt: &mut Stmt,
    templates: &HashMap<String, Function>,
    trait_methods: &HashMap<String, Vec<TraitMethod>>,
    instances: &mut HashMap<String, String>,
    queue: &mut VecDeque<Function>,
    known_functions: &HashMap<String, Function>,
    closure_env: &HashMap<String, ClosureBinding>,
    fn_alias_env: &HashMap<String, String>,
    locals_in_scope: &HashSet<String>,
    local_types_in_scope: &HashMap<String, TypeAst>,
    ret_expected: Option<&TypeAst>,
) -> Result<(), String> {
    match stmt {
        Stmt::Let { ty, init, .. } | Stmt::Const { ty, init, .. } => {
            rewrite_expr_high_level(
                init,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                ty.as_ref(),
            )?;
        }
        Stmt::Assign { target, value, .. } => {
            rewrite_expr_high_level(
                target,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
            let value_expected = match &target.kind {
                ExprKind::Ident(name) => local_types_in_scope.get(name),
                _ => None,
            };
            rewrite_expr_high_level(
                value,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                value_expected,
            )?;
        }
        Stmt::Expr { expr, .. } | Stmt::Defer { expr, .. } => {
            rewrite_expr_high_level(
                expr,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
        }
        Stmt::Go { expr, .. } => {
            if let ExprKind::Call { type_args, .. } = &expr.kind
                && !type_args.is_empty()
            {
                return Err("go does not accept type arguments".to_string());
            }
            rewrite_expr_high_level(
                expr,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
        }
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                rewrite_expr_high_level(
                    expr,
                    templates,
                    trait_methods,
                    instances,
                    queue,
                    known_functions,
                    closure_env,
                    fn_alias_env,
                    locals_in_scope,
                    local_types_in_scope,
                    ret_expected,
                    ret_expected,
                )?;
            }
        }
        Stmt::While { cond, body, .. } => {
            rewrite_expr_high_level(
                cond,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
            rewrite_block_high_level_in_env(
                body,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
            )?;
        }
        Stmt::Loop { body, .. } => {
            rewrite_block_high_level_in_env(
                body,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
            )?;
        }
        Stmt::ForIn {
            name,
            index,
            iter,
            body,
            ..
        } => {
            rewrite_expr_high_level(
                iter,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
            let mut body_locals = locals_in_scope.clone();
            body_locals.insert(name.clone());
            if let Some(idx) = index {
                body_locals.insert(idx.clone());
            }
            rewrite_block_high_level_in_env(
                body,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                &body_locals,
                local_types_in_scope,
                ret_expected,
            )?;
        }
        Stmt::ForRange {
            index,
            name,
            start,
            end,
            body,
            ..
        } => {
            rewrite_expr_high_level(
                start,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
            rewrite_expr_high_level(
                end,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
            let mut body_locals = locals_in_scope.clone();
            body_locals.insert(name.clone());
            if let Some(index_name) = index {
                body_locals.insert(index_name.clone());
            }
            rewrite_block_high_level_in_env(
                body,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                &body_locals,
                local_types_in_scope,
                ret_expected,
            )?;
        }
        Stmt::Select { arms, .. } => {
            for arm in arms {
                match &mut arm.kind {
                    crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                        rewrite_expr_high_level(
                            chan,
                            templates,
                            trait_methods,
                            instances,
                            queue,
                            known_functions,
                            closure_env,
                            fn_alias_env,
                            locals_in_scope,
                            local_types_in_scope,
                            ret_expected,
                            None,
                        )?;
                        rewrite_expr_high_level(
                            value,
                            templates,
                            trait_methods,
                            instances,
                            queue,
                            known_functions,
                            closure_env,
                            fn_alias_env,
                            locals_in_scope,
                            local_types_in_scope,
                            ret_expected,
                            None,
                        )?;
                    }
                    crate::frontend::ast::SelectArmKind::Recv { chan, .. } => {
                        rewrite_expr_high_level(
                            chan,
                            templates,
                            trait_methods,
                            instances,
                            queue,
                            known_functions,
                            closure_env,
                            fn_alias_env,
                            locals_in_scope,
                            local_types_in_scope,
                            ret_expected,
                            None,
                        )?;
                    }
                    crate::frontend::ast::SelectArmKind::After { ms } => {
                        rewrite_expr_high_level(
                            ms,
                            templates,
                            trait_methods,
                            instances,
                            queue,
                            known_functions,
                            closure_env,
                            fn_alias_env,
                            locals_in_scope,
                            local_types_in_scope,
                            ret_expected,
                            None,
                        )?;
                    }
                    crate::frontend::ast::SelectArmKind::Default => {}
                }
                let mut arm_locals = locals_in_scope.clone();
                if let crate::frontend::ast::SelectArmKind::Recv {
                    bind: Some((value_name, ok_name)),
                    ..
                } = &arm.kind
                {
                    arm_locals.insert(value_name.clone());
                    arm_locals.insert(ok_name.clone());
                }
                rewrite_block_or_expr_high_level(
                    &mut arm.body,
                    templates,
                    trait_methods,
                    instances,
                    queue,
                    known_functions,
                    closure_env,
                    fn_alias_env,
                    &arm_locals,
                    local_types_in_scope,
                    ret_expected,
                    None,
                )?;
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
    Ok(())
}

fn rewrite_expr_high_level(
    expr: &mut Expr,
    templates: &HashMap<String, Function>,
    trait_methods: &HashMap<String, Vec<TraitMethod>>,
    instances: &mut HashMap<String, String>,
    queue: &mut VecDeque<Function>,
    known_functions: &HashMap<String, Function>,
    closure_env: &HashMap<String, ClosureBinding>,
    fn_alias_env: &HashMap<String, String>,
    locals_in_scope: &HashSet<String>,
    local_types_in_scope: &HashMap<String, TypeAst>,
    ret_expected: Option<&TypeAst>,
    expected: Option<&TypeAst>,
) -> Result<(), String> {
    match &mut expr.kind {
        ExprKind::StructLit { fields, .. } => {
            for (_, field_expr) in fields {
                rewrite_expr_high_level(
                    field_expr,
                    templates,
                    trait_methods,
                    instances,
                    queue,
                    known_functions,
                    closure_env,
                    fn_alias_env,
                    locals_in_scope,
                    local_types_in_scope,
                    ret_expected,
                    None,
                )?;
            }
        }
        ExprKind::ArrayLit(items) => {
            for item in items {
                rewrite_expr_high_level(
                    item,
                    templates,
                    trait_methods,
                    instances,
                    queue,
                    known_functions,
                    closure_env,
                    fn_alias_env,
                    locals_in_scope,
                    local_types_in_scope,
                    ret_expected,
                    None,
                )?;
            }
        }
        ExprKind::Tuple(items) => {
            for item in items {
                rewrite_expr_high_level(
                    item,
                    templates,
                    trait_methods,
                    instances,
                    queue,
                    known_functions,
                    closure_env,
                    fn_alias_env,
                    locals_in_scope,
                    local_types_in_scope,
                    ret_expected,
                    None,
                )?;
            }
        }
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
            rewrite_block_high_level_in_env(
                block,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
            )?;
        }
        ExprKind::If {
            cond,
            then_block,
            else_block,
        } => {
            rewrite_expr_high_level(
                cond,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
            rewrite_block_high_level_in_env(
                then_block,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
            )?;
            if let Some(block) = else_block {
                rewrite_block_high_level_in_env(
                    block,
                    templates,
                    trait_methods,
                    instances,
                    queue,
                    known_functions,
                    closure_env,
                    fn_alias_env,
                    locals_in_scope,
                    local_types_in_scope,
                    ret_expected,
                )?;
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            rewrite_expr_high_level(
                scrutinee,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
            for arm in arms {
                let mut arm_locals = locals_in_scope.clone();
                collect_pattern_bindings(&arm.pattern, &mut arm_locals);
                if let Some(guard) = &mut arm.guard {
                    rewrite_expr_high_level(
                        guard,
                        templates,
                        trait_methods,
                        instances,
                        queue,
                        known_functions,
                        closure_env,
                        fn_alias_env,
                        &arm_locals,
                        local_types_in_scope,
                        ret_expected,
                        None,
                    )?;
                }
                rewrite_block_or_expr_high_level(
                    &mut arm.body,
                    templates,
                    trait_methods,
                    instances,
                    queue,
                    known_functions,
                    closure_env,
                    fn_alias_env,
                    &arm_locals,
                    local_types_in_scope,
                    ret_expected,
                    expected,
                )?;
            }
        }
        ExprKind::Closure { params, body } => {
            let mut nested_env = closure_env.clone();
            let mut nested_locals = locals_in_scope.clone();
            let mut nested_local_types = local_types_in_scope.clone();
            for param in params.iter() {
                nested_env.remove(&param.name);
                nested_locals.insert(param.name.clone());
                if let Some(ty) = &param.ty {
                    nested_local_types.insert(param.name.clone(), ty.clone());
                }
            }
            rewrite_block_or_expr_high_level(
                body.as_mut(),
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                &nested_env,
                fn_alias_env,
                &nested_locals,
                &nested_local_types,
                ret_expected,
                expected,
            )?;
            if let Some(expected_ty) = expected
                && is_fnptr_type_ast(expected_ty)
            {
                let binding = ClosureBinding {
                    params: params.clone(),
                    body: body.as_ref().clone(),
                    span: expr.span.clone(),
                    expr_id: expr.id,
                    enclosing_locals: locals_in_scope.clone(),
                };
                let lifted = lift_closure_binding_to_fnptr(
                    "__anon_closure",
                    &binding,
                    expected_ty,
                    instances,
                    queue,
                );
                match lifted {
                    Ok(Some(name)) => expr.kind = ExprKind::Ident(name),
                    Ok(None) => {}
                    Err(err) => return Err(err),
                }
            }
        }
        ExprKind::Call {
            callee,
            type_args,
            args,
        } => {
            rewrite_expr_high_level(
                callee,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
            if let ExprKind::Field { base, name } = &callee.kind
                && let ExprKind::Ident(pkg_name) = &base.kind
                && !locals_in_scope.contains(pkg_name)
            {
                let namespaced = format!("{}.{}", pkg_name, name);
                if known_functions.contains_key(&namespaced) {
                    callee.kind = ExprKind::Ident(namespaced);
                }
            }
            let mut arg_expected = vec![None; args.len()];
            let mut has_known_callee_sig = false;
            let resolved_known_callee = match &callee.kind {
                ExprKind::Ident(callee_name) => {
                    if known_functions.contains_key(callee_name)
                        || queue.iter().any(|f| f.name == *callee_name)
                    {
                        Some(callee_name.clone())
                    } else {
                        fn_alias_env.get(callee_name).cloned()
                    }
                }
                _ => None,
            };
            if let Some(resolved_callee_name) = resolved_known_callee.as_ref()
                && let Some(func_sig) = known_functions
                    .get(resolved_callee_name)
                    .or_else(|| queue.iter().find(|f| f.name == *resolved_callee_name))
            {
                has_known_callee_sig = true;
                for (idx, param) in func_sig.params.iter().enumerate().take(args.len()) {
                    arg_expected[idx] = Some(param.ty.clone());
                }
                if let Some((fn_param_idx, _, _)) =
                    find_captured_closure_fnptr_arg(func_sig, args, closure_env, locals_in_scope)
                {
                    // Preserve capturing closure form for call-site specialization.
                    if let Some(expected_slot) = arg_expected.get_mut(fn_param_idx) {
                        *expected_slot = None;
                    }
                }
            }
            if !has_known_callee_sig {
                if let ExprKind::Ident(callee_name) = &callee.kind
                    && let Some(callee_ty) = local_types_in_scope.get(callee_name)
                {
                    set_call_arg_expected_from_fnptr(callee_ty, &mut arg_expected);
                }
                if arg_expected.iter().all(Option::is_none)
                    && let Some(callee_ty) = infer_expr_type_ast(
                        callee,
                        local_types_in_scope,
                        templates,
                        known_functions,
                    )
                {
                    set_call_arg_expected_from_fnptr(&callee_ty, &mut arg_expected);
                }
            }
            for (idx, arg) in args.iter_mut().enumerate() {
                rewrite_expr_high_level(
                    arg,
                    templates,
                    trait_methods,
                    instances,
                    queue,
                    known_functions,
                    closure_env,
                    fn_alias_env,
                    locals_in_scope,
                    local_types_in_scope,
                    ret_expected,
                    arg_expected[idx].as_ref(),
                )?;
            }
            if let ExprKind::Closure { params, body } = &callee.kind {
                let desugared =
                    desugar_immediate_closure_call(params, body.as_ref(), args, expr.span.clone())?;
                expr.kind = ExprKind::Block(Box::new(desugared));
                if let ExprKind::Block(block) = &mut expr.kind {
                    rewrite_block_high_level_in_env(
                        block,
                        templates,
                        trait_methods,
                        instances,
                        queue,
                        known_functions,
                        closure_env,
                        fn_alias_env,
                        locals_in_scope,
                        local_types_in_scope,
                        ret_expected,
                    )?;
                }
                return Ok(());
            }

            if let ExprKind::Field { base, name: field } = &callee.kind
                && let ExprKind::Ident(base_name) = &base.kind
            {
                let key = format!("{}.{}", base_name, field);
                if let Some(binding) = closure_env.get(&key) {
                    let desugared = desugar_immediate_closure_call(
                        &binding.params,
                        &binding.body,
                        args,
                        expr.span.clone(),
                    )?;
                    expr.kind = ExprKind::Block(Box::new(desugared));
                    if let ExprKind::Block(block) = &mut expr.kind {
                        rewrite_block_high_level_in_env(
                            block,
                            templates,
                            trait_methods,
                            instances,
                            queue,
                            known_functions,
                            closure_env,
                            fn_alias_env,
                            locals_in_scope,
                            local_types_in_scope,
                            ret_expected,
                        )?;
                    }
                    return Ok(());
                }
            }

            if let ExprKind::Ident(name) = &callee.kind
                && let Some(binding) = closure_env.get(name)
            {
                let desugared = desugar_immediate_closure_call(
                    &binding.params,
                    &binding.body,
                    args,
                    expr.span.clone(),
                )?;
                expr.kind = ExprKind::Block(Box::new(desugared));
                if let ExprKind::Block(block) = &mut expr.kind {
                    rewrite_block_high_level_in_env(
                        block,
                        templates,
                        trait_methods,
                        instances,
                        queue,
                        known_functions,
                        closure_env,
                        fn_alias_env,
                        locals_in_scope,
                        local_types_in_scope,
                        ret_expected,
                    )?;
                }
                return Ok(());
            }

            let already_runtime_closure_call = matches!(
                &callee.kind,
                ExprKind::Field { name, .. } if name.starts_with("__gost_closure_call_")
            );
            if !already_runtime_closure_call
                && let Some((params, ret, is_variadic, method_name)) =
                    infer_closure_runtime_signature(
                        callee,
                        local_types_in_scope,
                        templates,
                        known_functions,
                    )
            {
                let _ = (params, ret, is_variadic);
                let old_callee = std::mem::replace(
                    callee,
                    Box::new(Expr {
                        id: expr.id,
                        kind: ExprKind::Nil,
                        span: expr.span.clone(),
                    }),
                );
                let method_callee = Expr {
                    id: old_callee.id,
                    kind: ExprKind::Field {
                        base: old_callee,
                        name: method_name,
                    },
                    span: expr.span.clone(),
                };
                **callee = method_callee;
                type_args.clear();
            }

            if let ExprKind::Field { base, name: method } = &callee.kind {
                let is_value_receiver = match &base.kind {
                    ExprKind::Ident(name) => locals_in_scope.contains(name),
                    _ => true,
                };
                if is_value_receiver {
                    instantiate_generic_method_call(
                        base,
                        method,
                        type_args,
                        args,
                        expected,
                        local_types_in_scope,
                        templates,
                        trait_methods,
                        known_functions,
                        instances,
                        queue,
                    )?;
                }
            }

            if let ExprKind::Ident(name) = &mut callee.kind
                && templates.contains_key(name)
            {
                if type_args.is_empty() {
                    let template = templates
                        .get(name)
                        .ok_or_else(|| format!("generic template `{}` not found", name))?;
                    *type_args = infer_generic_type_args_or_fallback(
                        template,
                        args,
                        expected,
                        local_types_in_scope,
                        templates,
                        known_functions,
                    )?;
                }
                let specialized = instantiate_generic_function(
                    name,
                    type_args,
                    templates,
                    trait_methods,
                    known_functions,
                    instances,
                    queue,
                )?;
                *name = specialized;
                type_args.clear();
            }
            let callee_name = match &callee.kind {
                ExprKind::Ident(name) => Some(name.clone()),
                _ => None,
            };
            if let Some(callee_name) = callee_name {
                let resolved_callee_name = if known_functions.contains_key(&callee_name)
                    || queue.iter().any(|f| f.name == callee_name)
                {
                    callee_name.clone()
                } else if let Some(alias) = fn_alias_env.get(&callee_name) {
                    alias.clone()
                } else {
                    callee_name.clone()
                };
                let func_sig = known_functions
                    .get(&resolved_callee_name)
                    .cloned()
                    .or_else(|| {
                        queue
                            .iter()
                            .find(|f| f.name == resolved_callee_name)
                            .cloned()
                    });
                if let Some(func_sig) = func_sig {
                    let args_snapshot = args.clone();
                    if let Some(replacement) =
                        callable_return_replacement_expr(expr, &func_sig, &args_snapshot)
                    {
                        *expr = replacement;
                        rewrite_expr_high_level(
                            expr,
                            templates,
                            trait_methods,
                            instances,
                            queue,
                            known_functions,
                            closure_env,
                            fn_alias_env,
                            locals_in_scope,
                            local_types_in_scope,
                            ret_expected,
                            expected,
                        )?;
                        return Ok(());
                    }
                    if let Some((fn_param_idx, binding, captures)) = find_captured_closure_fnptr_arg(
                        &func_sig,
                        &args_snapshot,
                        closure_env,
                        locals_in_scope,
                    ) {
                        specialize_call_with_captured_closure(
                            expr,
                            &resolved_callee_name,
                            &func_sig,
                            fn_param_idx,
                            &binding,
                            &captures,
                            local_types_in_scope,
                            instances,
                            queue,
                        )?;
                        return Ok(());
                    }
                }
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
            rewrite_expr_high_level(
                base,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
        }
        ExprKind::Index { base, index } => {
            rewrite_expr_high_level(
                base,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
            rewrite_expr_high_level(
                index,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
        }
        ExprKind::Binary { left, right, .. } => {
            rewrite_expr_high_level(
                left,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
            rewrite_expr_high_level(
                right,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
        }
        ExprKind::Send { chan, value } => {
            rewrite_expr_high_level(
                chan,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
            rewrite_expr_high_level(
                value,
                templates,
                trait_methods,
                instances,
                queue,
                known_functions,
                closure_env,
                fn_alias_env,
                locals_in_scope,
                local_types_in_scope,
                ret_expected,
                None,
            )?;
        }
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil => {}
        ExprKind::Ident(name) => {
            if let Some(expected_ty) = expected
                && is_fnptr_type_ast(expected_ty)
                && let Some(binding) = closure_env.get(name)
            {
                let lifted =
                    lift_closure_binding_to_fnptr(name, binding, expected_ty, instances, queue);
                match lifted {
                    Ok(Some(lifted_name)) => *name = lifted_name,
                    Ok(None) => {}
                    Err(err) => return Err(err),
                }
            }
        }
    }
    Ok(())
}

fn type_ast_callable_parts(kind: &TypeAstKind) -> Option<(&Vec<TypeAst>, &TypeAst, bool)> {
    match kind {
        TypeAstKind::FnPtr {
            params,
            ret,
            is_variadic,
        }
        | TypeAstKind::Closure {
            params,
            ret,
            is_variadic,
        } => Some((params, ret.as_ref(), *is_variadic)),
        _ => None,
    }
}

fn is_callable_type_ast(ty: &TypeAst) -> bool {
    type_ast_callable_parts(&ty.kind).is_some()
}

fn is_fnptr_type_ast(ty: &TypeAst) -> bool {
    matches!(ty.kind, TypeAstKind::FnPtr { .. })
}

fn closure_runtime_signature_hash(params: &[TypeAst], ret: &TypeAst, is_variadic: bool) -> u64 {
    let sig_ty = TypeAst {
        kind: TypeAstKind::Closure {
            params: params.to_vec(),
            ret: Box::new(ret.clone()),
            is_variadic,
        },
        span: ret.span.clone(),
    };
    let mut hasher = DefaultHasher::new();
    type_ast_key(&sig_ty).hash(&mut hasher);
    hasher.finish()
}

fn closure_runtime_method_name(params: &[TypeAst], ret: &TypeAst, is_variadic: bool) -> String {
    let sig_hash = closure_runtime_signature_hash(params, ret, is_variadic);
    format!("__gost_closure_call_{:016x}", sig_hash)
}

fn infer_closure_runtime_signature(
    callee: &Expr,
    local_types_in_scope: &HashMap<String, TypeAst>,
    templates: &HashMap<String, Function>,
    known_functions: &HashMap<String, Function>,
) -> Option<(Vec<TypeAst>, TypeAst, bool, String)> {
    let callee_ty = infer_expr_type_ast(callee, local_types_in_scope, templates, known_functions)?;
    match callee_ty.kind {
        TypeAstKind::Closure {
            params,
            ret,
            is_variadic,
        } => {
            let method = closure_runtime_method_name(&params, ret.as_ref(), is_variadic);
            Some((params, (*ret).clone(), is_variadic, method))
        }
        _ => None,
    }
}

#[derive(Clone)]
enum CallableReturnSource {
    Param(usize),
    Closure {
        params: Vec<ClosureParam>,
        body: BlockOrExpr,
        span: crate::frontend::ast::Span,
        expr_id: crate::frontend::ast::ExprId,
    },
}

fn callable_return_source(func: &Function) -> Option<CallableReturnSource> {
    if !func.ret_type.as_ref().is_some_and(is_callable_type_ast) {
        return None;
    }

    let mut return_expr: Option<&Expr> = None;
    let mut return_stmt_idx = 0usize;
    for (idx, stmt) in func.body.stmts.iter().enumerate() {
        if let Stmt::Return {
            expr: Some(expr), ..
        } = stmt
        {
            return_expr = Some(expr);
            return_stmt_idx = idx;
        }
    }
    if return_expr.is_none()
        && let Some(tail) = &func.body.tail
    {
        return_expr = Some(tail);
        return_stmt_idx = func.body.stmts.len();
    }
    let returned = return_expr?;

    let callable_param_idx = |name: &str| -> Option<usize> {
        func.params.iter().enumerate().find_map(|(idx, param)| {
            if param.name == name && is_callable_type_ast(&param.ty) {
                Some(idx)
            } else {
                None
            }
        })
    };

    match &returned.kind {
        ExprKind::Ident(name) => {
            if let Some(idx) = callable_param_idx(name) {
                return Some(CallableReturnSource::Param(idx));
            }
            for stmt in func.body.stmts.iter().take(return_stmt_idx) {
                let (bound_name, init) = match stmt {
                    Stmt::Let { name, init, .. } | Stmt::Const { name, init, .. } => (name, init),
                    _ => continue,
                };
                if bound_name != name {
                    continue;
                }
                match &init.kind {
                    ExprKind::Closure { params, body } => {
                        return Some(CallableReturnSource::Closure {
                            params: params.clone(),
                            body: body.as_ref().clone(),
                            span: init.span.clone(),
                            expr_id: init.id,
                        });
                    }
                    ExprKind::Ident(src) => {
                        if let Some(idx) = callable_param_idx(src) {
                            return Some(CallableReturnSource::Param(idx));
                        }
                    }
                    _ => {}
                }
            }
            None
        }
        ExprKind::Closure { params, body } => Some(CallableReturnSource::Closure {
            params: params.clone(),
            body: body.as_ref().clone(),
            span: returned.span.clone(),
            expr_id: returned.id,
        }),
        _ => None,
    }
}

fn can_inline_captured_arg_expr(expr: &Expr) -> bool {
    matches!(
        expr.kind,
        ExprKind::Ident(_)
            | ExprKind::Bool(_)
            | ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Char(_)
            | ExprKind::String(_)
            | ExprKind::Nil
    )
}

fn substitute_free_idents_with_exprs_in_block_or_expr(
    body: &mut BlockOrExpr,
    subst: &HashMap<String, Expr>,
) {
    match body {
        BlockOrExpr::Block(block) => substitute_free_idents_with_exprs_in_block(block, subst),
        BlockOrExpr::Expr(expr) => substitute_free_idents_with_exprs_in_expr(expr, subst),
    }
}

fn substitute_free_idents_with_exprs_in_block(block: &mut Block, subst: &HashMap<String, Expr>) {
    for stmt in &mut block.stmts {
        substitute_free_idents_with_exprs_in_stmt(stmt, subst);
    }
    if let Some(tail) = &mut block.tail {
        substitute_free_idents_with_exprs_in_expr(tail, subst);
    }
}

fn substitute_free_idents_with_exprs_in_stmt(stmt: &mut Stmt, subst: &HashMap<String, Expr>) {
    match stmt {
        Stmt::Let { init, .. } | Stmt::Const { init, .. } => {
            substitute_free_idents_with_exprs_in_expr(init, subst);
        }
        Stmt::Assign { target, value, .. } => {
            substitute_free_idents_with_exprs_in_expr(target, subst);
            substitute_free_idents_with_exprs_in_expr(value, subst);
        }
        Stmt::Expr { expr, .. } | Stmt::Go { expr, .. } | Stmt::Defer { expr, .. } => {
            substitute_free_idents_with_exprs_in_expr(expr, subst);
        }
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                substitute_free_idents_with_exprs_in_expr(expr, subst);
            }
        }
        Stmt::While { cond, body, .. } => {
            substitute_free_idents_with_exprs_in_expr(cond, subst);
            substitute_free_idents_with_exprs_in_block(body, subst);
        }
        Stmt::Loop { body, .. } => substitute_free_idents_with_exprs_in_block(body, subst),
        Stmt::ForIn { iter, body, .. } => {
            substitute_free_idents_with_exprs_in_expr(iter, subst);
            substitute_free_idents_with_exprs_in_block(body, subst);
        }
        Stmt::ForRange {
            start, end, body, ..
        } => {
            substitute_free_idents_with_exprs_in_expr(start, subst);
            substitute_free_idents_with_exprs_in_expr(end, subst);
            substitute_free_idents_with_exprs_in_block(body, subst);
        }
        Stmt::Select { arms, .. } => {
            for arm in arms {
                match &mut arm.kind {
                    crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                        substitute_free_idents_with_exprs_in_expr(chan, subst);
                        substitute_free_idents_with_exprs_in_expr(value, subst);
                    }
                    crate::frontend::ast::SelectArmKind::Recv { chan, .. } => {
                        substitute_free_idents_with_exprs_in_expr(chan, subst);
                    }
                    crate::frontend::ast::SelectArmKind::After { ms } => {
                        substitute_free_idents_with_exprs_in_expr(ms, subst);
                    }
                    crate::frontend::ast::SelectArmKind::Default => {}
                }
                match &mut arm.body {
                    BlockOrExpr::Block(block) => {
                        substitute_free_idents_with_exprs_in_block(block, subst)
                    }
                    BlockOrExpr::Expr(expr) => {
                        substitute_free_idents_with_exprs_in_expr(expr, subst)
                    }
                }
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
}

fn substitute_free_idents_with_exprs_in_expr(expr: &mut Expr, subst: &HashMap<String, Expr>) {
    match &mut expr.kind {
        ExprKind::Ident(name) => {
            if let Some(replacement) = subst.get(name) {
                *expr = replacement.clone();
            }
        }
        ExprKind::StructLit { fields, .. } => {
            for (_, field_expr) in fields {
                substitute_free_idents_with_exprs_in_expr(field_expr, subst);
            }
        }
        ExprKind::ArrayLit(items) | ExprKind::Tuple(items) => {
            for item in items {
                substitute_free_idents_with_exprs_in_expr(item, subst);
            }
        }
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
            substitute_free_idents_with_exprs_in_block(block, subst);
        }
        ExprKind::If {
            cond,
            then_block,
            else_block,
        } => {
            substitute_free_idents_with_exprs_in_expr(cond, subst);
            substitute_free_idents_with_exprs_in_block(then_block, subst);
            if let Some(block) = else_block {
                substitute_free_idents_with_exprs_in_block(block, subst);
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            substitute_free_idents_with_exprs_in_expr(scrutinee, subst);
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    substitute_free_idents_with_exprs_in_expr(guard, subst);
                }
                match &mut arm.body {
                    BlockOrExpr::Block(block) => {
                        substitute_free_idents_with_exprs_in_block(block, subst)
                    }
                    BlockOrExpr::Expr(expr) => {
                        substitute_free_idents_with_exprs_in_expr(expr, subst)
                    }
                }
            }
        }
        ExprKind::Closure { body, .. } => {
            substitute_free_idents_with_exprs_in_block_or_expr(body.as_mut(), subst);
        }
        ExprKind::Call { callee, args, .. } => {
            substitute_free_idents_with_exprs_in_expr(callee, subst);
            for arg in args {
                substitute_free_idents_with_exprs_in_expr(arg, subst);
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
            substitute_free_idents_with_exprs_in_expr(base, subst);
        }
        ExprKind::Index { base, index } => {
            substitute_free_idents_with_exprs_in_expr(base, subst);
            substitute_free_idents_with_exprs_in_expr(index, subst);
        }
        ExprKind::Binary { left, right, .. } => {
            substitute_free_idents_with_exprs_in_expr(left, subst);
            substitute_free_idents_with_exprs_in_expr(right, subst);
        }
        ExprKind::Send { chan, value } => {
            substitute_free_idents_with_exprs_in_expr(chan, subst);
            substitute_free_idents_with_exprs_in_expr(value, subst);
        }
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil => {}
    }
}

fn callable_return_replacement_expr(
    call_expr: &Expr,
    callee_func: &Function,
    args: &[Expr],
) -> Option<Expr> {
    let source = callable_return_source(callee_func)?;
    match source {
        CallableReturnSource::Param(idx) => {
            if let Some(arg) = args.get(idx) {
                return Some(arg.clone());
            }
            None
        }
        CallableReturnSource::Closure {
            params,
            mut body,
            span: _span,
            expr_id,
        } => {
            let mut param_name_to_index = HashMap::new();
            for (idx, param) in callee_func.params.iter().enumerate() {
                param_name_to_index.insert(param.name.clone(), idx);
            }
            let enclosing_locals: HashSet<String> = callee_func
                .params
                .iter()
                .map(|param| param.name.clone())
                .collect();
            let binding = ClosureBinding {
                params: params.clone(),
                body: body.clone(),
                span: call_expr.span.clone(),
                expr_id,
                enclosing_locals,
            };
            let captures = closure_captured_outer_locals(&binding);
            let mut subst = HashMap::new();
            let mut prelude = Vec::new();
            for captured in captures {
                let arg_idx = param_name_to_index.get(&captured).copied()?;
                let arg = args.get(arg_idx)?;
                if can_inline_captured_arg_expr(arg) {
                    subst.insert(captured, arg.clone());
                    continue;
                }
                let cap_name = format!("__gost_closure_cap_{}_{}", call_expr.id, arg_idx);
                prelude.push(Stmt::Let {
                    name: cap_name.clone(),
                    ty: None,
                    init: arg.clone(),
                    span: call_expr.span.clone(),
                });
                subst.insert(
                    captured,
                    Expr {
                        id: call_expr.id,
                        kind: ExprKind::Ident(cap_name),
                        span: call_expr.span.clone(),
                    },
                );
            }
            substitute_free_idents_with_exprs_in_block_or_expr(&mut body, &subst);
            let closure_expr = Expr {
                id: call_expr.id,
                kind: ExprKind::Closure {
                    params,
                    body: Box::new(body),
                },
                span: call_expr.span.clone(),
            };
            if prelude.is_empty() {
                return Some(closure_expr);
            }
            Some(Expr {
                id: call_expr.id,
                kind: ExprKind::Block(Box::new(Block {
                    stmts: prelude,
                    tail: Some(closure_expr),
                    span: call_expr.span.clone(),
                })),
                span: call_expr.span.clone(),
            })
        }
    }
}

fn set_call_arg_expected_from_fnptr(callee_ty: &TypeAst, arg_expected: &mut [Option<TypeAst>]) {
    let Some((params, _, _)) = type_ast_callable_parts(&callee_ty.kind) else {
        return;
    };
    for (idx, param) in params.iter().enumerate().take(arg_expected.len()) {
        if arg_expected[idx].is_none() {
            arg_expected[idx] = Some(param.clone());
        }
    }
}

fn closure_fnptr_arity_mismatch(
    closure_param_count: usize,
    expected_fixed_count: usize,
    is_variadic: bool,
) -> Option<String> {
    if closure_param_count == expected_fixed_count {
        return None;
    }
    if is_variadic {
        return Some(format!(
            "closure for variadic fn pointer must declare exactly {} fixed argument(s), got {}",
            expected_fixed_count, closure_param_count
        ));
    }
    Some(format!(
        "closure expects {} arguments, but fn pointer type expects {}",
        closure_param_count, expected_fixed_count
    ))
}

fn lift_closure_binding_to_fnptr(
    binding_name: &str,
    binding: &ClosureBinding,
    expected: &TypeAst,
    instances: &mut HashMap<String, String>,
    queue: &mut VecDeque<Function>,
) -> Result<Option<String>, String> {
    let Some((expected_params, expected_ret, is_variadic)) =
        type_ast_callable_parts(&expected.kind)
    else {
        return Err("closure can only be converted to function pointer type".to_string());
    };
    if let Some(msg) =
        closure_fnptr_arity_mismatch(binding.params.len(), expected_params.len(), is_variadic)
    {
        return Err(msg);
    }
    let captures = closure_captured_outer_locals(binding);
    if !captures.is_empty() {
        return Ok(None);
    }

    let sig_key = type_ast_key(expected);
    let cache_key = format!(
        "__closure_lift__{}__{}__{}",
        binding_name, binding.expr_id, sig_key
    );
    if let Some(existing) = instances.get(&cache_key) {
        return Ok(Some(existing.clone()));
    }

    let mut hasher = DefaultHasher::new();
    cache_key.hash(&mut hasher);
    let sym_hash = hasher.finish();
    let lifted_name = format!("__closure_fn_{}_{}", binding.expr_id, sym_hash);

    let mut params = Vec::with_capacity(binding.params.len());
    for (idx, p) in binding.params.iter().enumerate() {
        let ty = p.ty.clone().unwrap_or_else(|| expected_params[idx].clone());
        params.push(Param {
            name: p.name.clone(),
            ty,
            span: p.span.clone(),
        });
    }

    let body = match &binding.body {
        BlockOrExpr::Block(block) => block.as_ref().clone(),
        BlockOrExpr::Expr(expr) => Block {
            stmts: Vec::new(),
            tail: Some(expr.clone()),
            span: expr.span.clone(),
        },
    };

    queue.push_back(Function {
        vis: Visibility::Private,
        name: lifted_name.clone(),
        type_params: Vec::new(),
        params,
        is_variadic,
        ret_type: Some(expected_ret.clone()),
        is_extern: false,
        is_unsafe: false,
        extern_abi: None,
        body,
        span: binding.span.clone(),
    });
    instances.insert(cache_key, lifted_name.clone());
    Ok(Some(lifted_name))
}

fn find_captured_closure_fnptr_arg(
    callee_func: &Function,
    args: &[Expr],
    closure_env: &HashMap<String, ClosureBinding>,
    locals_in_scope: &HashSet<String>,
) -> Option<(usize, ClosureBinding, Vec<String>)> {
    for (idx, (param, arg)) in callee_func.params.iter().zip(args.iter()).enumerate() {
        if !is_callable_type_ast(&param.ty) {
            continue;
        }
        match &arg.kind {
            ExprKind::Ident(name) => {
                if let Some(binding) = closure_env.get(name) {
                    let captures = closure_captured_outer_locals(binding);
                    if !captures.is_empty() {
                        return Some((idx, binding.clone(), captures));
                    }
                }
            }
            ExprKind::Field { base, name } => {
                if let ExprKind::Ident(base_name) = &base.kind {
                    let key = format!("{}.{}", base_name, name);
                    if let Some(binding) = closure_env.get(&key) {
                        let captures = closure_captured_outer_locals(binding);
                        if !captures.is_empty() {
                            return Some((idx, binding.clone(), captures));
                        }
                    }
                }
            }
            ExprKind::Closure { params, body } => {
                let binding = ClosureBinding {
                    params: params.clone(),
                    body: body.as_ref().clone(),
                    span: arg.span.clone(),
                    expr_id: arg.id,
                    enclosing_locals: locals_in_scope.clone(),
                };
                let captures = closure_captured_outer_locals(&binding);
                if !captures.is_empty() {
                    return Some((idx, binding, captures));
                }
            }
            _ => {}
        }
    }
    None
}

fn specialize_call_with_captured_closure(
    expr: &mut Expr,
    callee_name: &str,
    callee_func: &Function,
    fn_param_idx: usize,
    binding: &ClosureBinding,
    captures: &[String],
    local_types_in_scope: &HashMap<String, TypeAst>,
    instances: &mut HashMap<String, String>,
    queue: &mut VecDeque<Function>,
) -> Result<(), String> {
    let specialized_name = specialize_function_with_captured_closure(
        callee_name,
        callee_func,
        fn_param_idx,
        binding,
        captures,
        local_types_in_scope,
        instances,
        queue,
    )?;
    let ExprKind::Call {
        callee,
        type_args,
        args,
    } = &mut expr.kind
    else {
        return Ok(());
    };
    let mut new_args = Vec::with_capacity(args.len().saturating_sub(1) + captures.len());
    for (idx, arg) in std::mem::take(args).into_iter().enumerate() {
        if idx != fn_param_idx {
            new_args.push(arg);
        }
    }
    for cap_name in captures {
        new_args.push(Expr {
            id: expr.id,
            kind: ExprKind::Ident(cap_name.clone()),
            span: expr.span.clone(),
        });
    }
    callee.kind = ExprKind::Ident(specialized_name);
    type_args.clear();
    *args = new_args;
    Ok(())
}

fn specialize_function_with_captured_closure(
    callee_name: &str,
    callee_func: &Function,
    fn_param_idx: usize,
    binding: &ClosureBinding,
    captures: &[String],
    local_types_in_scope: &HashMap<String, TypeAst>,
    instances: &mut HashMap<String, String>,
    queue: &mut VecDeque<Function>,
) -> Result<String, String> {
    if !callee_func.type_params.is_empty() {
        return Err(format!(
            "internal: capturing closure specialization expects monomorphic callee `{}`",
            callee_name
        ));
    }
    let fn_param = callee_func
        .params
        .get(fn_param_idx)
        .ok_or_else(|| "closure argument index out of bounds".to_string())?;
    let Some((expected_params, _, is_variadic)) = type_ast_callable_parts(&fn_param.ty.kind) else {
        return Err("closure specialization requires fn pointer parameter".to_string());
    };
    if let Some(msg) =
        closure_fnptr_arity_mismatch(binding.params.len(), expected_params.len(), is_variadic)
    {
        return Err(msg);
    }

    let mut key_hasher = DefaultHasher::new();
    callee_name.hash(&mut key_hasher);
    fn_param_idx.hash(&mut key_hasher);
    binding.expr_id.hash(&mut key_hasher);
    for cap in captures {
        cap.hash(&mut key_hasher);
        if let Some(ty) = local_types_in_scope.get(cap) {
            type_ast_key(ty).hash(&mut key_hasher);
        }
    }
    let cache_key = format!("__closure_call_spec__{}", key_hasher.finish());
    if let Some(existing) = instances.get(&cache_key) {
        return Ok(existing.clone());
    }

    let mut cap_rename = HashMap::new();
    let mut cap_params = Vec::with_capacity(captures.len());
    for (idx, cap_name) in captures.iter().enumerate() {
        let cap_ty = local_types_in_scope.get(cap_name).cloned().ok_or_else(|| {
            format!(
                "internal: missing inferred type for captured variable `{}` during closure specialization",
                cap_name
            )
        })?;
        let cap_param_name = format!("__cap_{}_{}", idx, cap_name);
        cap_rename.insert(cap_name.clone(), cap_param_name.clone());
        cap_params.push(Param {
            name: cap_param_name,
            ty: cap_ty,
            span: binding.span.clone(),
        });
    }

    let mut closure_params = binding.params.clone();
    for (idx, p) in closure_params.iter_mut().enumerate() {
        if p.ty.is_none() {
            p.ty = Some(expected_params[idx].clone());
        }
    }
    let mut closure_body = binding.body.clone();
    rename_free_idents_in_block_or_expr(&mut closure_body, &cap_rename);

    let mut new_body = callee_func.body.clone();
    inline_fn_param_calls_in_block(
        &mut new_body,
        &fn_param.name,
        &closure_params,
        &closure_body,
    )?;

    let mut new_params = Vec::with_capacity(callee_func.params.len() - 1 + cap_params.len());
    for (idx, p) in callee_func.params.iter().enumerate() {
        if idx != fn_param_idx {
            new_params.push(p.clone());
        }
    }
    new_params.extend(cap_params);

    let specialized_name = format!("__closure_spec_{}_{}", callee_name, binding.expr_id);
    queue.push_back(Function {
        vis: Visibility::Private,
        name: specialized_name.clone(),
        type_params: Vec::new(),
        params: new_params,
        is_variadic: callee_func.is_variadic,
        ret_type: callee_func.ret_type.clone(),
        is_extern: false,
        is_unsafe: callee_func.is_unsafe,
        extern_abi: None,
        body: new_body,
        span: callee_func.span.clone(),
    });
    instances.insert(cache_key, specialized_name.clone());
    Ok(specialized_name)
}

fn rename_free_idents_in_block_or_expr(body: &mut BlockOrExpr, renames: &HashMap<String, String>) {
    match body {
        BlockOrExpr::Block(block) => rename_free_idents_in_block(block, renames),
        BlockOrExpr::Expr(expr) => rename_free_idents_in_expr(expr, renames),
    }
}

fn rename_free_idents_in_block(block: &mut Block, renames: &HashMap<String, String>) {
    for stmt in &mut block.stmts {
        rename_free_idents_in_stmt(stmt, renames);
    }
    if let Some(tail) = &mut block.tail {
        rename_free_idents_in_expr(tail, renames);
    }
}

fn rename_free_idents_in_stmt(stmt: &mut Stmt, renames: &HashMap<String, String>) {
    match stmt {
        Stmt::Let { init, .. } | Stmt::Const { init, .. } => {
            rename_free_idents_in_expr(init, renames);
        }
        Stmt::Assign { target, value, .. } => {
            rename_free_idents_in_expr(target, renames);
            rename_free_idents_in_expr(value, renames);
        }
        Stmt::Expr { expr, .. } | Stmt::Go { expr, .. } | Stmt::Defer { expr, .. } => {
            rename_free_idents_in_expr(expr, renames);
        }
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                rename_free_idents_in_expr(expr, renames);
            }
        }
        Stmt::While { cond, body, .. } => {
            rename_free_idents_in_expr(cond, renames);
            rename_free_idents_in_block(body, renames);
        }
        Stmt::Loop { body, .. } => rename_free_idents_in_block(body, renames),
        Stmt::ForIn { iter, body, .. } => {
            rename_free_idents_in_expr(iter, renames);
            rename_free_idents_in_block(body, renames);
        }
        Stmt::ForRange {
            start, end, body, ..
        } => {
            rename_free_idents_in_expr(start, renames);
            rename_free_idents_in_expr(end, renames);
            rename_free_idents_in_block(body, renames);
        }
        Stmt::Select { arms, .. } => {
            for arm in arms {
                match &mut arm.kind {
                    crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                        rename_free_idents_in_expr(chan, renames);
                        rename_free_idents_in_expr(value, renames);
                    }
                    crate::frontend::ast::SelectArmKind::Recv { chan, .. } => {
                        rename_free_idents_in_expr(chan, renames);
                    }
                    crate::frontend::ast::SelectArmKind::After { ms } => {
                        rename_free_idents_in_expr(ms, renames);
                    }
                    crate::frontend::ast::SelectArmKind::Default => {}
                }
                match &mut arm.body {
                    BlockOrExpr::Block(block) => rename_free_idents_in_block(block, renames),
                    BlockOrExpr::Expr(expr) => rename_free_idents_in_expr(expr, renames),
                }
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
}

fn rename_free_idents_in_expr(expr: &mut Expr, renames: &HashMap<String, String>) {
    match &mut expr.kind {
        ExprKind::Ident(name) => {
            if let Some(new_name) = renames.get(name) {
                *name = new_name.clone();
            }
        }
        ExprKind::StructLit { fields, .. } => {
            for (_, field_expr) in fields {
                rename_free_idents_in_expr(field_expr, renames);
            }
        }
        ExprKind::ArrayLit(items) => {
            for item in items {
                rename_free_idents_in_expr(item, renames);
            }
        }
        ExprKind::Tuple(items) => {
            for item in items {
                rename_free_idents_in_expr(item, renames);
            }
        }
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
            rename_free_idents_in_block(block, renames);
        }
        ExprKind::If {
            cond,
            then_block,
            else_block,
        } => {
            rename_free_idents_in_expr(cond, renames);
            rename_free_idents_in_block(then_block, renames);
            if let Some(block) = else_block {
                rename_free_idents_in_block(block, renames);
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            rename_free_idents_in_expr(scrutinee, renames);
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    rename_free_idents_in_expr(guard, renames);
                }
                match &mut arm.body {
                    BlockOrExpr::Block(block) => rename_free_idents_in_block(block, renames),
                    BlockOrExpr::Expr(expr) => rename_free_idents_in_expr(expr, renames),
                }
            }
        }
        ExprKind::Closure { body, .. } => {
            rename_free_idents_in_block_or_expr(body, renames);
        }
        ExprKind::Call { callee, args, .. } => {
            rename_free_idents_in_expr(callee, renames);
            for arg in args {
                rename_free_idents_in_expr(arg, renames);
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
            rename_free_idents_in_expr(base, renames);
        }
        ExprKind::Index { base, index } => {
            rename_free_idents_in_expr(base, renames);
            rename_free_idents_in_expr(index, renames);
        }
        ExprKind::Binary { left, right, .. } => {
            rename_free_idents_in_expr(left, renames);
            rename_free_idents_in_expr(right, renames);
        }
        ExprKind::Send { chan, value } => {
            rename_free_idents_in_expr(chan, renames);
            rename_free_idents_in_expr(value, renames);
        }
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil => {}
    }
}

fn inline_fn_param_calls_in_block(
    block: &mut Block,
    fn_param_name: &str,
    closure_params: &[ClosureParam],
    closure_body: &BlockOrExpr,
) -> Result<(), String> {
    for stmt in &mut block.stmts {
        inline_fn_param_calls_in_stmt(stmt, fn_param_name, closure_params, closure_body)?;
    }
    if let Some(tail) = &mut block.tail {
        inline_fn_param_calls_in_expr(tail, fn_param_name, closure_params, closure_body)?;
    }
    Ok(())
}

fn inline_fn_param_calls_in_stmt(
    stmt: &mut Stmt,
    fn_param_name: &str,
    closure_params: &[ClosureParam],
    closure_body: &BlockOrExpr,
) -> Result<(), String> {
    match stmt {
        Stmt::Let { init, .. } | Stmt::Const { init, .. } => {
            inline_fn_param_calls_in_expr(init, fn_param_name, closure_params, closure_body)?;
        }
        Stmt::Assign { target, value, .. } => {
            inline_fn_param_calls_in_expr(target, fn_param_name, closure_params, closure_body)?;
            inline_fn_param_calls_in_expr(value, fn_param_name, closure_params, closure_body)?;
        }
        Stmt::Expr { expr, .. } | Stmt::Go { expr, .. } | Stmt::Defer { expr, .. } => {
            inline_fn_param_calls_in_expr(expr, fn_param_name, closure_params, closure_body)?;
        }
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                inline_fn_param_calls_in_expr(expr, fn_param_name, closure_params, closure_body)?;
            }
        }
        Stmt::While { cond, body, .. } => {
            inline_fn_param_calls_in_expr(cond, fn_param_name, closure_params, closure_body)?;
            inline_fn_param_calls_in_block(body, fn_param_name, closure_params, closure_body)?;
        }
        Stmt::Loop { body, .. } => {
            inline_fn_param_calls_in_block(body, fn_param_name, closure_params, closure_body)?;
        }
        Stmt::ForIn { iter, body, .. } => {
            inline_fn_param_calls_in_expr(iter, fn_param_name, closure_params, closure_body)?;
            inline_fn_param_calls_in_block(body, fn_param_name, closure_params, closure_body)?;
        }
        Stmt::ForRange {
            start, end, body, ..
        } => {
            inline_fn_param_calls_in_expr(start, fn_param_name, closure_params, closure_body)?;
            inline_fn_param_calls_in_expr(end, fn_param_name, closure_params, closure_body)?;
            inline_fn_param_calls_in_block(body, fn_param_name, closure_params, closure_body)?;
        }
        Stmt::Select { arms, .. } => {
            for arm in arms {
                match &mut arm.kind {
                    crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                        inline_fn_param_calls_in_expr(
                            chan,
                            fn_param_name,
                            closure_params,
                            closure_body,
                        )?;
                        inline_fn_param_calls_in_expr(
                            value,
                            fn_param_name,
                            closure_params,
                            closure_body,
                        )?;
                    }
                    crate::frontend::ast::SelectArmKind::Recv { chan, .. } => {
                        inline_fn_param_calls_in_expr(
                            chan,
                            fn_param_name,
                            closure_params,
                            closure_body,
                        )?;
                    }
                    crate::frontend::ast::SelectArmKind::After { ms } => {
                        inline_fn_param_calls_in_expr(
                            ms,
                            fn_param_name,
                            closure_params,
                            closure_body,
                        )?;
                    }
                    crate::frontend::ast::SelectArmKind::Default => {}
                }
                match &mut arm.body {
                    BlockOrExpr::Block(block) => {
                        inline_fn_param_calls_in_block(
                            block,
                            fn_param_name,
                            closure_params,
                            closure_body,
                        )?;
                    }
                    BlockOrExpr::Expr(expr) => {
                        inline_fn_param_calls_in_expr(
                            expr,
                            fn_param_name,
                            closure_params,
                            closure_body,
                        )?;
                    }
                }
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
    Ok(())
}

fn inline_fn_param_calls_in_expr(
    expr: &mut Expr,
    fn_param_name: &str,
    closure_params: &[ClosureParam],
    closure_body: &BlockOrExpr,
) -> Result<(), String> {
    match &mut expr.kind {
        ExprKind::Call { callee, args, .. } => {
            for arg in args.iter_mut() {
                inline_fn_param_calls_in_expr(arg, fn_param_name, closure_params, closure_body)?;
            }
            if let ExprKind::Field { base, name } = &callee.kind
                && let ExprKind::Ident(recv_name) = &base.kind
                && recv_name == fn_param_name
                && name.starts_with("__gost_closure_call_")
            {
                let desugared = desugar_immediate_closure_call(
                    closure_params,
                    closure_body,
                    args,
                    expr.span.clone(),
                )?;
                expr.kind = ExprKind::Block(Box::new(desugared));
                if let ExprKind::Block(block) = &mut expr.kind {
                    inline_fn_param_calls_in_block(
                        block,
                        fn_param_name,
                        closure_params,
                        closure_body,
                    )?;
                }
                return Ok(());
            }
            if let ExprKind::Ident(name) = &callee.kind
                && name == fn_param_name
            {
                let desugared = desugar_immediate_closure_call(
                    closure_params,
                    closure_body,
                    args,
                    expr.span.clone(),
                )?;
                expr.kind = ExprKind::Block(Box::new(desugared));
                if let ExprKind::Block(block) = &mut expr.kind {
                    inline_fn_param_calls_in_block(
                        block,
                        fn_param_name,
                        closure_params,
                        closure_body,
                    )?;
                }
                return Ok(());
            }
            inline_fn_param_calls_in_expr(callee, fn_param_name, closure_params, closure_body)?;
        }
        ExprKind::Ident(name) => {
            if name == fn_param_name {
                expr.kind = ExprKind::Closure {
                    params: closure_params.to_vec(),
                    body: Box::new(closure_body.clone()),
                };
                return Ok(());
            }
        }
        ExprKind::StructLit { fields, .. } => {
            for (_, field_expr) in fields {
                inline_fn_param_calls_in_expr(
                    field_expr,
                    fn_param_name,
                    closure_params,
                    closure_body,
                )?;
            }
        }
        ExprKind::ArrayLit(items) => {
            for item in items {
                inline_fn_param_calls_in_expr(item, fn_param_name, closure_params, closure_body)?;
            }
        }
        ExprKind::Tuple(items) => {
            for item in items {
                inline_fn_param_calls_in_expr(item, fn_param_name, closure_params, closure_body)?;
            }
        }
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
            inline_fn_param_calls_in_block(block, fn_param_name, closure_params, closure_body)?;
        }
        ExprKind::If {
            cond,
            then_block,
            else_block,
        } => {
            inline_fn_param_calls_in_expr(cond, fn_param_name, closure_params, closure_body)?;
            inline_fn_param_calls_in_block(
                then_block,
                fn_param_name,
                closure_params,
                closure_body,
            )?;
            if let Some(block) = else_block {
                inline_fn_param_calls_in_block(block, fn_param_name, closure_params, closure_body)?;
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            inline_fn_param_calls_in_expr(scrutinee, fn_param_name, closure_params, closure_body)?;
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    inline_fn_param_calls_in_expr(
                        guard,
                        fn_param_name,
                        closure_params,
                        closure_body,
                    )?;
                }
                match &mut arm.body {
                    BlockOrExpr::Block(block) => {
                        inline_fn_param_calls_in_block(
                            block,
                            fn_param_name,
                            closure_params,
                            closure_body,
                        )?;
                    }
                    BlockOrExpr::Expr(expr) => {
                        inline_fn_param_calls_in_expr(
                            expr,
                            fn_param_name,
                            closure_params,
                            closure_body,
                        )?;
                    }
                }
            }
        }
        ExprKind::Closure { body, .. } => match body.as_mut() {
            BlockOrExpr::Block(block) => {
                inline_fn_param_calls_in_block(block, fn_param_name, closure_params, closure_body)?
            }
            BlockOrExpr::Expr(expr) => {
                inline_fn_param_calls_in_expr(expr, fn_param_name, closure_params, closure_body)?
            }
        },
        ExprKind::Field { base, .. }
        | ExprKind::Unary { expr: base, .. }
        | ExprKind::Cast { expr: base, .. }
        | ExprKind::Borrow { expr: base, .. }
        | ExprKind::Deref { expr: base }
        | ExprKind::Try { expr: base }
        | ExprKind::Recv { chan: base }
        | ExprKind::Close { chan: base }
        | ExprKind::After { ms: base } => {
            inline_fn_param_calls_in_expr(base, fn_param_name, closure_params, closure_body)?;
        }
        ExprKind::Index { base, index } => {
            inline_fn_param_calls_in_expr(base, fn_param_name, closure_params, closure_body)?;
            inline_fn_param_calls_in_expr(index, fn_param_name, closure_params, closure_body)?;
        }
        ExprKind::Binary { left, right, .. } => {
            inline_fn_param_calls_in_expr(left, fn_param_name, closure_params, closure_body)?;
            inline_fn_param_calls_in_expr(right, fn_param_name, closure_params, closure_body)?;
        }
        ExprKind::Send { chan, value } => {
            inline_fn_param_calls_in_expr(chan, fn_param_name, closure_params, closure_body)?;
            inline_fn_param_calls_in_expr(value, fn_param_name, closure_params, closure_body)?;
        }
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil => {}
    }
    Ok(())
}

fn collect_pattern_bindings(pattern: &Pattern, out: &mut HashSet<String>) {
    match pattern {
        Pattern::Ident(name) => {
            out.insert(name.clone());
        }
        Pattern::Or(patterns) => {
            for pattern in patterns {
                collect_pattern_bindings(pattern, out);
            }
        }
        Pattern::Variant { binds, .. } => {
            for name in binds {
                out.insert(name.clone());
            }
        }
        Pattern::Wildcard | Pattern::Bool(_) | Pattern::Int(_) => {}
    }
}

fn closure_captured_outer_locals(binding: &ClosureBinding) -> Vec<String> {
    let mut captures = HashSet::new();
    let mut initial_bound = HashSet::new();
    for param in &binding.params {
        initial_bound.insert(param.name.clone());
    }
    collect_captured_in_block_or_expr(
        &binding.body,
        &initial_bound,
        &binding.enclosing_locals,
        &mut captures,
    );
    let mut out: Vec<String> = captures.into_iter().collect();
    out.sort();
    out
}

fn collect_captured_in_block_or_expr(
    body: &BlockOrExpr,
    bound: &HashSet<String>,
    enclosing_locals: &HashSet<String>,
    captures: &mut HashSet<String>,
) {
    match body {
        BlockOrExpr::Block(block) => {
            collect_captured_in_block(block, bound, enclosing_locals, captures);
        }
        BlockOrExpr::Expr(expr) => {
            collect_captured_in_expr(expr, bound, enclosing_locals, captures);
        }
    }
}

fn collect_captured_in_block(
    block: &Block,
    bound: &HashSet<String>,
    enclosing_locals: &HashSet<String>,
    captures: &mut HashSet<String>,
) {
    let mut scope = bound.clone();
    for stmt in &block.stmts {
        collect_captured_in_stmt(stmt, &scope, enclosing_locals, captures);
        if let Stmt::Let { name, .. } | Stmt::Const { name, .. } = stmt {
            scope.insert(name.clone());
        }
    }
    if let Some(tail) = &block.tail {
        collect_captured_in_expr(tail, &scope, enclosing_locals, captures);
    }
}

fn collect_captured_in_stmt(
    stmt: &Stmt,
    bound: &HashSet<String>,
    enclosing_locals: &HashSet<String>,
    captures: &mut HashSet<String>,
) {
    match stmt {
        Stmt::Let { init, .. } | Stmt::Const { init, .. } => {
            collect_captured_in_expr(init, bound, enclosing_locals, captures);
        }
        Stmt::Assign { target, value, .. } => {
            collect_captured_in_expr(target, bound, enclosing_locals, captures);
            collect_captured_in_expr(value, bound, enclosing_locals, captures);
        }
        Stmt::Expr { expr, .. } | Stmt::Go { expr, .. } | Stmt::Defer { expr, .. } => {
            collect_captured_in_expr(expr, bound, enclosing_locals, captures);
        }
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                collect_captured_in_expr(expr, bound, enclosing_locals, captures);
            }
        }
        Stmt::While { cond, body, .. } => {
            collect_captured_in_expr(cond, bound, enclosing_locals, captures);
            collect_captured_in_block(body, bound, enclosing_locals, captures);
        }
        Stmt::Loop { body, .. } => {
            collect_captured_in_block(body, bound, enclosing_locals, captures);
        }
        Stmt::ForIn {
            name,
            index,
            iter,
            body,
            ..
        } => {
            collect_captured_in_expr(iter, bound, enclosing_locals, captures);
            let mut body_scope = bound.clone();
            body_scope.insert(name.clone());
            if let Some(idx) = index {
                body_scope.insert(idx.clone());
            }
            collect_captured_in_block(body, &body_scope, enclosing_locals, captures);
        }
        Stmt::ForRange {
            index,
            name,
            start,
            end,
            body,
            ..
        } => {
            collect_captured_in_expr(start, bound, enclosing_locals, captures);
            collect_captured_in_expr(end, bound, enclosing_locals, captures);
            let mut body_scope = bound.clone();
            body_scope.insert(name.clone());
            if let Some(index_name) = index {
                body_scope.insert(index_name.clone());
            }
            collect_captured_in_block(body, &body_scope, enclosing_locals, captures);
        }
        Stmt::Select { arms, .. } => {
            for arm in arms {
                match &arm.kind {
                    crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                        collect_captured_in_expr(chan, bound, enclosing_locals, captures);
                        collect_captured_in_expr(value, bound, enclosing_locals, captures);
                    }
                    crate::frontend::ast::SelectArmKind::Recv { chan, .. } => {
                        collect_captured_in_expr(chan, bound, enclosing_locals, captures);
                    }
                    crate::frontend::ast::SelectArmKind::After { ms } => {
                        collect_captured_in_expr(ms, bound, enclosing_locals, captures);
                    }
                    crate::frontend::ast::SelectArmKind::Default => {}
                }
                let mut arm_scope = bound.clone();
                if let crate::frontend::ast::SelectArmKind::Recv {
                    bind: Some((value_name, ok_name)),
                    ..
                } = &arm.kind
                {
                    arm_scope.insert(value_name.clone());
                    arm_scope.insert(ok_name.clone());
                }
                collect_captured_in_block_or_expr(
                    &arm.body,
                    &arm_scope,
                    enclosing_locals,
                    captures,
                );
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
}

fn collect_captured_in_expr(
    expr: &Expr,
    bound: &HashSet<String>,
    enclosing_locals: &HashSet<String>,
    captures: &mut HashSet<String>,
) {
    match &expr.kind {
        ExprKind::StructLit { fields, .. } => {
            for (_, field_expr) in fields {
                collect_captured_in_expr(field_expr, bound, enclosing_locals, captures);
            }
        }
        ExprKind::ArrayLit(items) => {
            for item in items {
                collect_captured_in_expr(item, bound, enclosing_locals, captures);
            }
        }
        ExprKind::Tuple(items) => {
            for item in items {
                collect_captured_in_expr(item, bound, enclosing_locals, captures);
            }
        }
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
            collect_captured_in_block(block, bound, enclosing_locals, captures);
        }
        ExprKind::If {
            cond,
            then_block,
            else_block,
        } => {
            collect_captured_in_expr(cond, bound, enclosing_locals, captures);
            collect_captured_in_block(then_block, bound, enclosing_locals, captures);
            if let Some(block) = else_block {
                collect_captured_in_block(block, bound, enclosing_locals, captures);
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            collect_captured_in_expr(scrutinee, bound, enclosing_locals, captures);
            for arm in arms {
                let mut arm_scope = bound.clone();
                collect_pattern_bindings(&arm.pattern, &mut arm_scope);
                if let Some(guard) = &arm.guard {
                    collect_captured_in_expr(guard, &arm_scope, enclosing_locals, captures);
                }
                collect_captured_in_block_or_expr(
                    &arm.body,
                    &arm_scope,
                    enclosing_locals,
                    captures,
                );
            }
        }
        ExprKind::Closure { params, body } => {
            let mut nested_scope = bound.clone();
            for param in params {
                nested_scope.insert(param.name.clone());
            }
            collect_captured_in_block_or_expr(body, &nested_scope, enclosing_locals, captures);
        }
        ExprKind::Call { callee, args, .. } => {
            collect_captured_in_expr(callee, bound, enclosing_locals, captures);
            for arg in args {
                collect_captured_in_expr(arg, bound, enclosing_locals, captures);
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
            collect_captured_in_expr(base, bound, enclosing_locals, captures);
        }
        ExprKind::Index { base, index } => {
            collect_captured_in_expr(base, bound, enclosing_locals, captures);
            collect_captured_in_expr(index, bound, enclosing_locals, captures);
        }
        ExprKind::Binary { left, right, .. } => {
            collect_captured_in_expr(left, bound, enclosing_locals, captures);
            collect_captured_in_expr(right, bound, enclosing_locals, captures);
        }
        ExprKind::Send { chan, value } => {
            collect_captured_in_expr(chan, bound, enclosing_locals, captures);
            collect_captured_in_expr(value, bound, enclosing_locals, captures);
        }
        ExprKind::Ident(name) => {
            if !bound.contains(name) && enclosing_locals.contains(name) {
                captures.insert(name.clone());
            }
        }
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil => {}
    }
}

fn instantiate_generic_method_call(
    base: &Expr,
    method: &str,
    type_args: &mut Vec<TypeAst>,
    args: &[Expr],
    expected: Option<&TypeAst>,
    local_types_in_scope: &HashMap<String, TypeAst>,
    templates: &HashMap<String, Function>,
    trait_methods: &HashMap<String, Vec<TraitMethod>>,
    known_functions: &HashMap<String, Function>,
    instances: &mut HashMap<String, String>,
    queue: &mut VecDeque<Function>,
) -> Result<(), String> {
    let recv_ty = infer_expr_type_ast(base, local_types_in_scope, templates, known_functions);
    let mut candidates: Vec<(&str, &Function)> = templates
        .iter()
        .filter_map(|(name, func)| {
            if is_impl_mangled(name) && logical_method_name(name) == method {
                Some((name.as_str(), func))
            } else {
                None
            }
        })
        .filter(|(_, func)| {
            let Some(recv_ty) = &recv_ty else {
                return true;
            };
            let Some(param0) = func.params.first() else {
                return false;
            };
            recv_param_compatible_ast(recv_ty, &param0.ty)
        })
        .collect();
    let recv_is_interface_like = matches!(
        recv_ty.as_ref().map(|ty| &ty.kind),
        Some(TypeAstKind::Interface)
    ) || matches!(
        recv_ty.as_ref().map(|ty| &ty.kind),
        Some(TypeAstKind::Named(name)) if trait_methods.contains_key(name)
    );
    if candidates.is_empty() {
        if !type_args.is_empty() {
            if recv_is_interface_like {
                let explicit_type_args = type_args.clone();
                let method_templates: Vec<String> = templates
                    .iter()
                    .filter_map(|(name, template)| {
                        if !is_impl_mangled(name) || logical_method_name(name) != method {
                            return None;
                        }
                        if template.type_params.len() != explicit_type_args.len() {
                            return None;
                        }
                        Some(name.clone())
                    })
                    .collect();
                for name in method_templates {
                    instantiate_generic_function(
                        &name,
                        &explicit_type_args,
                        templates,
                        trait_methods,
                        known_functions,
                        instances,
                        queue,
                    )?;
                }
                return Ok(());
            }
            return Err(format!(
                "method `{}` is not generic; use `[]` only for type arguments and `()` for value arguments",
                method
            ));
        }
        return Ok(());
    }
    candidates.sort_by(|a, b| a.0.cmp(b.0));

    if type_args.is_empty() {
        let mut infer_args = Vec::with_capacity(args.len() + 1);
        infer_args.push(base.clone());
        infer_args.extend(args.iter().cloned());
        for (name, template) in candidates {
            let inferred = infer_generic_type_args_or_fallback(
                template,
                &infer_args,
                expected,
                local_types_in_scope,
                templates,
                known_functions,
            )?;
            instantiate_generic_function(
                name,
                &inferred,
                templates,
                trait_methods,
                known_functions,
                instances,
                queue,
            )?;
        }
        return Ok(());
    }

    let explicit_type_args = type_args.clone();
    let mut matched_arity = false;
    let mut instantiated_any = false;
    let mut first_err: Option<String> = None;
    for (name, template) in candidates {
        if template.type_params.len() != explicit_type_args.len() {
            continue;
        }
        matched_arity = true;
        match instantiate_generic_function(
            name,
            &explicit_type_args,
            templates,
            trait_methods,
            known_functions,
            instances,
            queue,
        ) {
            Ok(_) => instantiated_any = true,
            Err(err) => {
                if first_err.is_none() {
                    first_err = Some(err);
                }
            }
        }
    }
    if !matched_arity {
        return Err(format!(
            "generic method `{}` does not accept {} type arguments",
            method,
            explicit_type_args.len()
        ));
    }
    if !instantiated_any {
        return Err(first_err
            .unwrap_or_else(|| format!("failed to instantiate generic method `{}`", method)));
    }
    if !recv_is_interface_like {
        type_args.clear();
    }
    Ok(())
}

fn infer_generic_type_args(
    template: &Function,
    args: &[Expr],
    expected: Option<&TypeAst>,
    local_types_in_scope: &HashMap<String, TypeAst>,
    templates: &HashMap<String, Function>,
    known_functions: &HashMap<String, Function>,
) -> Option<Vec<TypeAst>> {
    let mut inferred: HashMap<String, TypeAst> = HashMap::new();
    replay_generic_inference(
        template,
        args,
        expected,
        local_types_in_scope,
        templates,
        known_functions,
        true,
        &mut inferred,
    );
    let mut missing = template
        .type_params
        .iter()
        .filter(|tp| !inferred.contains_key(&tp.name))
        .map(|tp| tp.name.clone())
        .collect::<Vec<_>>();

    if !missing.is_empty() {
        // If expected-type first pass could not fully resolve, allow literal/default
        // argument inference as a second pass.
        replay_generic_inference(
            template,
            args,
            expected,
            local_types_in_scope,
            templates,
            known_functions,
            false,
            &mut inferred,
        );
        missing = template
            .type_params
            .iter()
            .filter(|tp| !inferred.contains_key(&tp.name))
            .map(|tp| tp.name.clone())
            .collect::<Vec<_>>();
    }

    if !missing.is_empty() {
        let candidates = collect_generic_fallback_candidates(
            args,
            expected,
            local_types_in_scope,
            templates,
            known_functions,
        );
        for param_name in missing {
            let mut selected: Option<HashMap<String, TypeAst>> = None;
            for cand in &candidates {
                let mut trial = inferred.clone();
                if !bind_inferred_type_param(&param_name, cand, &mut trial) {
                    continue;
                }
                replay_generic_inference(
                    template,
                    args,
                    expected,
                    local_types_in_scope,
                    templates,
                    known_functions,
                    false,
                    &mut trial,
                );
                if trial.contains_key(&param_name) {
                    selected = Some(trial);
                    break;
                }
            }
            if let Some(next) = selected {
                inferred = next;
            } else if let Some(default_ty) = candidates.first() {
                let _ = bind_inferred_type_param(&param_name, default_ty, &mut inferred);
            }
        }
    }
    if template
        .type_params
        .iter()
        .any(|param| !inferred.contains_key(&param.name))
    {
        return None;
    }

    let mut out = Vec::with_capacity(template.type_params.len());
    for param in &template.type_params {
        let ty = inferred.get(&param.name)?.clone();
        out.push(ty);
    }
    Some(out)
}

fn infer_generic_type_args_or_fallback(
    template: &Function,
    args: &[Expr],
    expected: Option<&TypeAst>,
    local_types_in_scope: &HashMap<String, TypeAst>,
    templates: &HashMap<String, Function>,
    known_functions: &HashMap<String, Function>,
) -> Result<Vec<TypeAst>, String> {
    if let Some(inferred) = infer_generic_type_args(
        template,
        args,
        expected,
        local_types_in_scope,
        templates,
        known_functions,
    ) {
        return Ok(inferred);
    }
    let mut candidates = collect_generic_fallback_candidates(
        args,
        expected,
        local_types_in_scope,
        templates,
        known_functions,
    );
    candidates.dedup_by(|a, b| type_ast_eq(a, b));
    let Some(fallback_ty) = candidates.into_iter().next() else {
        return Err(format!(
            "cannot infer generic type arguments for `{}`; provide explicit type arguments",
            template.name
        ));
    };
    Ok(template
        .type_params
        .iter()
        .map(|_| fallback_ty.clone())
        .collect())
}

fn replay_generic_inference(
    template: &Function,
    args: &[Expr],
    expected: Option<&TypeAst>,
    local_types_in_scope: &HashMap<String, TypeAst>,
    templates: &HashMap<String, Function>,
    known_functions: &HashMap<String, Function>,
    prefer_expected: bool,
    inferred: &mut HashMap<String, TypeAst>,
) {
    for (param, arg) in template.params.iter().zip(args.iter()) {
        infer_generic_type_args_from_param(
            &param.ty,
            arg,
            &template.type_params,
            local_types_in_scope,
            templates,
            known_functions,
            prefer_expected,
            inferred,
        );
    }
    if let (Some(ret_pat), Some(expected_ret)) = (&template.ret_type, expected) {
        let _ = unify_generic_pattern(ret_pat, expected_ret, &template.type_params, inferred);
    }
}

fn collect_generic_fallback_candidates(
    args: &[Expr],
    expected: Option<&TypeAst>,
    local_types_in_scope: &HashMap<String, TypeAst>,
    templates: &HashMap<String, Function>,
    known_functions: &HashMap<String, Function>,
) -> Vec<TypeAst> {
    fn push_unique(out: &mut Vec<TypeAst>, ty: TypeAst) {
        if !out.iter().any(|item| type_ast_eq(item, &ty)) {
            out.push(ty);
        }
    }

    let mut out = Vec::new();
    if let Some(expected) = expected {
        push_unique(&mut out, expected.clone());
        match &expected.kind {
            TypeAstKind::Ref(inner)
            | TypeAstKind::MutRef(inner)
            | TypeAstKind::Own(inner)
            | TypeAstKind::Alias(inner)
            | TypeAstKind::Slice(inner)
            | TypeAstKind::Chan(inner)
            | TypeAstKind::Shared(inner) => push_unique(&mut out, (**inner).clone()),
            _ => {}
        }
    }
    for arg in args {
        if let Some(ty) = infer_expr_type_ast(arg, local_types_in_scope, templates, known_functions)
        {
            push_unique(&mut out, ty);
        }
    }
    out
}

fn infer_generic_type_args_from_param(
    param_ty: &TypeAst,
    arg: &Expr,
    type_params: &[TypeParam],
    local_types_in_scope: &HashMap<String, TypeAst>,
    templates: &HashMap<String, Function>,
    known_functions: &HashMap<String, Function>,
    prefer_expected: bool,
    inferred: &mut HashMap<String, TypeAst>,
) {
    if let ExprKind::Closure { params, body } = &arg.kind {
        infer_generic_type_args_from_closure(
            param_ty,
            params,
            body,
            type_params,
            local_types_in_scope,
            templates,
            known_functions,
            inferred,
        );
    }
    if prefer_expected
        && matches!(arg.kind, ExprKind::Int(_) | ExprKind::Float(_))
        && pattern_mentions_type_param(param_ty, type_params)
    {
        return;
    }
    let Some(arg_ty) = infer_expr_type_ast(arg, local_types_in_scope, templates, known_functions)
    else {
        return;
    };
    let _ = unify_generic_pattern(param_ty, &arg_ty, type_params, inferred);
}

fn pattern_mentions_type_param(pattern: &TypeAst, type_params: &[TypeParam]) -> bool {
    match &pattern.kind {
        TypeAstKind::Named(name) => type_params.iter().any(|tp| tp.name == *name),
        TypeAstKind::Ref(inner)
        | TypeAstKind::MutRef(inner)
        | TypeAstKind::Own(inner)
        | TypeAstKind::Alias(inner)
        | TypeAstKind::Slice(inner)
        | TypeAstKind::Chan(inner)
        | TypeAstKind::Shared(inner) => pattern_mentions_type_param(inner, type_params),
        TypeAstKind::Array(inner, _) => pattern_mentions_type_param(inner, type_params),
        TypeAstKind::Map(k, v) | TypeAstKind::Result(k, v) => {
            pattern_mentions_type_param(k, type_params)
                || pattern_mentions_type_param(v, type_params)
        }
        TypeAstKind::Tuple(items) => items
            .iter()
            .any(|item| pattern_mentions_type_param(item, type_params)),
        TypeAstKind::FnPtr { params, ret, .. } | TypeAstKind::Closure { params, ret, .. } => {
            params
                .iter()
                .any(|p| pattern_mentions_type_param(p, type_params))
                || pattern_mentions_type_param(ret, type_params)
        }
        TypeAstKind::Interface => false,
    }
}

fn infer_generic_type_args_from_closure(
    param_ty: &TypeAst,
    closure_params: &[ClosureParam],
    body: &BlockOrExpr,
    type_params: &[TypeParam],
    local_types_in_scope: &HashMap<String, TypeAst>,
    templates: &HashMap<String, Function>,
    known_functions: &HashMap<String, Function>,
    inferred: &mut HashMap<String, TypeAst>,
) {
    let Some((expected_params, expected_ret, _)) = type_ast_callable_parts(&param_ty.kind) else {
        return;
    };
    if closure_params.len() != expected_params.len() {
        return;
    }
    for (closure_param, expected_param) in closure_params.iter().zip(expected_params.iter()) {
        if let Some(actual_param_ty) = &closure_param.ty {
            let _ = unify_generic_pattern(expected_param, actual_param_ty, type_params, inferred);
        }
    }
    let mut closure_locals = local_types_in_scope.clone();
    for closure_param in closure_params {
        if let Some(param_ty) = &closure_param.ty {
            closure_locals.insert(closure_param.name.clone(), param_ty.clone());
        }
    }
    if let Some(actual_ret_ty) =
        infer_closure_body_type_ast(body, &closure_locals, templates, known_functions)
    {
        let _ = unify_generic_pattern(expected_ret, &actual_ret_ty, type_params, inferred);
    }
}

fn infer_closure_body_type_ast(
    body: &BlockOrExpr,
    local_types_in_scope: &HashMap<String, TypeAst>,
    templates: &HashMap<String, Function>,
    known_functions: &HashMap<String, Function>,
) -> Option<TypeAst> {
    match body {
        BlockOrExpr::Expr(expr) => {
            infer_expr_type_ast(expr, local_types_in_scope, templates, known_functions)
        }
        BlockOrExpr::Block(block) => {
            let mut inferred_locals = local_types_in_scope.clone();
            for stmt in &block.stmts {
                update_local_types_for_stmt(stmt, &mut inferred_locals, templates, known_functions);
            }
            if let Some(tail) = &block.tail {
                infer_expr_type_ast(tail, &inferred_locals, templates, known_functions)
            } else {
                Some(type_ast_named("unit", &block.span))
            }
        }
    }
}

fn unify_generic_pattern(
    pattern: &TypeAst,
    actual: &TypeAst,
    type_params: &[TypeParam],
    inferred: &mut HashMap<String, TypeAst>,
) -> bool {
    match &pattern.kind {
        TypeAstKind::Named(name) => {
            if type_params.iter().any(|tp| tp.name == *name) {
                return bind_inferred_type_param(name, actual, inferred);
            }
            matches!(&actual.kind, TypeAstKind::Named(other) if other == name)
        }
        TypeAstKind::Ref(p) => match &actual.kind {
            TypeAstKind::Ref(a) => unify_generic_pattern(p, a, type_params, inferred),
            _ => false,
        },
        TypeAstKind::MutRef(p) => match &actual.kind {
            TypeAstKind::MutRef(a) => unify_generic_pattern(p, a, type_params, inferred),
            _ => false,
        },
        TypeAstKind::Own(p) => match &actual.kind {
            TypeAstKind::Own(a) => unify_generic_pattern(p, a, type_params, inferred),
            _ => false,
        },
        TypeAstKind::Alias(p) => match &actual.kind {
            TypeAstKind::Alias(a) => unify_generic_pattern(p, a, type_params, inferred),
            _ => false,
        },
        TypeAstKind::Slice(p) => match &actual.kind {
            TypeAstKind::Slice(a) => unify_generic_pattern(p, a, type_params, inferred),
            _ => false,
        },
        TypeAstKind::Array(p_ty, p_len) => match &actual.kind {
            TypeAstKind::Array(a_ty, a_len) => {
                p_len == a_len && unify_generic_pattern(p_ty, a_ty, type_params, inferred)
            }
            _ => false,
        },
        TypeAstKind::Map(pk, pv) => match &actual.kind {
            TypeAstKind::Map(ak, av) => {
                unify_generic_pattern(pk, ak, type_params, inferred)
                    && unify_generic_pattern(pv, av, type_params, inferred)
            }
            _ => false,
        },
        TypeAstKind::Result(po, pe) => match &actual.kind {
            TypeAstKind::Result(ao, ae) => {
                unify_generic_pattern(po, ao, type_params, inferred)
                    && unify_generic_pattern(pe, ae, type_params, inferred)
            }
            _ => false,
        },
        TypeAstKind::Chan(p) => match &actual.kind {
            TypeAstKind::Chan(a) => unify_generic_pattern(p, a, type_params, inferred),
            _ => false,
        },
        TypeAstKind::Shared(p) => match &actual.kind {
            TypeAstKind::Shared(a) => unify_generic_pattern(p, a, type_params, inferred),
            _ => false,
        },
        TypeAstKind::Interface => matches!(actual.kind, TypeAstKind::Interface),
        TypeAstKind::Tuple(ps) => match &actual.kind {
            TypeAstKind::Tuple(as_) if ps.len() == as_.len() => {
                for (p, a) in ps.iter().zip(as_.iter()) {
                    if !unify_generic_pattern(p, a, type_params, inferred) {
                        return false;
                    }
                }
                true
            }
            _ => false,
        },
        TypeAstKind::FnPtr {
            params: pp,
            ret: pr,
            is_variadic: pv,
        }
        | TypeAstKind::Closure {
            params: pp,
            ret: pr,
            is_variadic: pv,
        } => match &actual.kind {
            TypeAstKind::FnPtr {
                params: ap,
                ret: ar,
                is_variadic: av,
            }
            | TypeAstKind::Closure {
                params: ap,
                ret: ar,
                is_variadic: av,
            } if pp.len() == ap.len() && pv == av => {
                for (p, a) in pp.iter().zip(ap.iter()) {
                    if !unify_generic_pattern(p, a, type_params, inferred) {
                        return false;
                    }
                }
                unify_generic_pattern(pr, ar, type_params, inferred)
            }
            _ => false,
        },
    }
}

fn bind_inferred_type_param(
    name: &str,
    actual: &TypeAst,
    inferred: &mut HashMap<String, TypeAst>,
) -> bool {
    match inferred.get(name) {
        Some(prev) => type_ast_eq(prev, actual),
        None => {
            inferred.insert(name.to_string(), actual.clone());
            true
        }
    }
}

fn infer_expr_type_ast(
    expr: &Expr,
    local_types_in_scope: &HashMap<String, TypeAst>,
    templates: &HashMap<String, Function>,
    known_functions: &HashMap<String, Function>,
) -> Option<TypeAst> {
    match &expr.kind {
        ExprKind::Bool(_) => Some(type_ast_named("bool", &expr.span)),
        ExprKind::Int(_) => Some(type_ast_named("i32", &expr.span)),
        ExprKind::Float(_) => Some(type_ast_named("f64", &expr.span)),
        ExprKind::Char(_) => Some(type_ast_named("char", &expr.span)),
        ExprKind::String(_) => Some(type_ast_named("string", &expr.span)),
        ExprKind::Nil => Some(type_ast_named("error", &expr.span)),
        ExprKind::Ident(name) => local_types_in_scope.get(name).cloned(),
        ExprKind::StructLit { name, .. } => Some(type_ast_named(name, &expr.span)),
        ExprKind::Cast { ty, .. } => Some(ty.clone()),
        ExprKind::Borrow {
            is_mut,
            expr: inner,
        } => infer_expr_type_ast(inner, local_types_in_scope, templates, known_functions).map(
            |inner_ty| TypeAst {
                kind: if *is_mut {
                    TypeAstKind::MutRef(Box::new(inner_ty))
                } else {
                    TypeAstKind::Ref(Box::new(inner_ty))
                },
                span: expr.span.clone(),
            },
        ),
        ExprKind::Unary { op, expr: inner } => {
            let inner_ty =
                infer_expr_type_ast(inner, local_types_in_scope, templates, known_functions)?;
            match op {
                UnaryOp::Not => Some(type_ast_named("bool", &expr.span)),
                UnaryOp::Neg | UnaryOp::BitNot => Some(inner_ty),
            }
        }
        ExprKind::Deref { expr: inner } => {
            let inner_ty =
                infer_expr_type_ast(inner, local_types_in_scope, templates, known_functions)?;
            match inner_ty.kind {
                TypeAstKind::Ref(pointee) | TypeAstKind::MutRef(pointee) => {
                    Some((*pointee).clone())
                }
                _ => None,
            }
        }
        ExprKind::Index { base, index } => {
            let base_ty =
                infer_expr_type_ast(base, local_types_in_scope, templates, known_functions)?;
            let mut container = base_ty.clone();
            while let TypeAstKind::Ref(inner) | TypeAstKind::MutRef(inner) = &container.kind {
                container = (**inner).clone();
            }
            match container.kind {
                TypeAstKind::Tuple(items) => {
                    let idx = match &index.kind {
                        ExprKind::Int(v) => v.parse::<usize>().ok(),
                        _ => None,
                    }?;
                    items.get(idx).cloned()
                }
                TypeAstKind::Array(elem, _) | TypeAstKind::Slice(elem) => Some((*elem).clone()),
                _ => None,
            }
        }
        ExprKind::Binary { op, left, right } => {
            let left_ty =
                infer_expr_type_ast(left, local_types_in_scope, templates, known_functions)?;
            let right_ty =
                infer_expr_type_ast(right, local_types_in_scope, templates, known_functions)?;
            match op {
                BinaryOp::And | BinaryOp::Or => Some(type_ast_named("bool", &expr.span)),
                BinaryOp::Eq
                | BinaryOp::NotEq
                | BinaryOp::Lt
                | BinaryOp::Lte
                | BinaryOp::Gt
                | BinaryOp::Gte => Some(type_ast_named("bool", &expr.span)),
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Rem
                | BinaryOp::BitAnd
                | BinaryOp::BitOr
                | BinaryOp::BitXor
                | BinaryOp::Shl
                | BinaryOp::Shr => {
                    if type_ast_eq(&left_ty, &right_ty) {
                        Some(left_ty)
                    } else {
                        None
                    }
                }
            }
        }
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
            let mut block_locals = local_types_in_scope.clone();
            for stmt in &block.stmts {
                update_local_types_for_stmt(stmt, &mut block_locals, templates, known_functions);
            }
            if let Some(tail) = &block.tail {
                infer_expr_type_ast(tail, &block_locals, templates, known_functions)
            } else {
                Some(type_ast_named("unit", &expr.span))
            }
        }
        ExprKind::Tuple(items) => {
            let mut tys = Vec::with_capacity(items.len());
            for item in items {
                tys.push(infer_expr_type_ast(
                    item,
                    local_types_in_scope,
                    templates,
                    known_functions,
                )?);
            }
            Some(TypeAst {
                kind: TypeAstKind::Tuple(tys),
                span: expr.span.clone(),
            })
        }
        ExprKind::ArrayLit(items) => {
            let first = items.first()?;
            let first_ty =
                infer_expr_type_ast(first, local_types_in_scope, templates, known_functions)?;
            for item in items.iter().skip(1) {
                let item_ty =
                    infer_expr_type_ast(item, local_types_in_scope, templates, known_functions)?;
                if !type_ast_eq(&first_ty, &item_ty) {
                    return None;
                }
            }
            Some(TypeAst {
                kind: TypeAstKind::Array(Box::new(first_ty), items.len()),
                span: expr.span.clone(),
            })
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
                && let ExprKind::Ident(enum_name) = &base.kind
                && (enum_name == "Result" || enum_name == "result")
                && (variant == "Ok" || variant == "ok" || variant == "Err" || variant == "err")
                && type_args.len() == 2
            {
                return Some(TypeAst {
                    kind: TypeAstKind::Result(
                        Box::new(type_args[0].clone()),
                        Box::new(type_args[1].clone()),
                    ),
                    span: expr.span.clone(),
                });
            }
            let callee_name = match &callee.kind {
                ExprKind::Ident(name) => Some(name.clone()),
                ExprKind::Field { base, name } => match &base.kind {
                    ExprKind::Ident(pkg) => Some(format!("{}.{}", pkg, name)),
                    _ => None,
                },
                _ => None,
            };
            if let Some(name) = callee_name
                && let Some(func) = known_functions.get(&name)
            {
                let mut ret_ty = func
                    .ret_type
                    .clone()
                    .unwrap_or_else(|| type_ast_named("unit", &expr.span));
                if func.type_params.is_empty() {
                    return Some(ret_ty);
                }
                if !type_args.is_empty() {
                    if type_args.len() != func.type_params.len() {
                        return None;
                    }
                    let mut map = HashMap::new();
                    for (param, arg) in func.type_params.iter().zip(type_args.iter()) {
                        map.insert(param.name.clone(), arg.clone());
                    }
                    substitute_type_ast(&mut ret_ty, &map);
                    return Some(ret_ty);
                }
                if let Some(inferred) = infer_generic_type_args(
                    func,
                    args,
                    None,
                    local_types_in_scope,
                    templates,
                    known_functions,
                ) {
                    if inferred.len() != func.type_params.len() {
                        return None;
                    }
                    let mut map = HashMap::new();
                    for (param, arg) in func.type_params.iter().zip(inferred.iter()) {
                        map.insert(param.name.clone(), arg.clone());
                    }
                    substitute_type_ast(&mut ret_ty, &map);
                    return Some(ret_ty);
                }
            }
            None
        }
        ExprKind::Closure { params, body } => {
            let mut closure_locals = local_types_in_scope.clone();
            let mut param_tys = Vec::with_capacity(params.len());
            for param in params {
                // Why: Lowering requires a concrete callable shape; fall back to i32 when no
                // annotation is available so residual untyped closures do not crash lowering.
                let ty = param
                    .ty
                    .clone()
                    .unwrap_or_else(|| type_ast_named("i32", &param.span));
                closure_locals.insert(param.name.clone(), ty.clone());
                param_tys.push(ty);
            }
            let ret_ty =
                infer_closure_body_type_ast(body, &closure_locals, templates, known_functions)
                    .unwrap_or_else(|| type_ast_named("unit", &expr.span));
            Some(TypeAst {
                kind: TypeAstKind::Closure {
                    params: param_tys,
                    ret: Box::new(ret_ty),
                    is_variadic: false,
                },
                span: expr.span.clone(),
            })
        }
        _ => None,
    }
}

fn type_ast_named(name: &str, span: &crate::frontend::ast::Span) -> TypeAst {
    TypeAst {
        kind: TypeAstKind::Named(name.to_string()),
        span: span.clone(),
    }
}

fn type_ast_eq(a: &TypeAst, b: &TypeAst) -> bool {
    match (&a.kind, &b.kind) {
        (TypeAstKind::Named(x), TypeAstKind::Named(y)) => x == y,
        (TypeAstKind::Ref(x), TypeAstKind::Ref(y))
        | (TypeAstKind::MutRef(x), TypeAstKind::MutRef(y))
        | (TypeAstKind::Own(x), TypeAstKind::Own(y))
        | (TypeAstKind::Alias(x), TypeAstKind::Alias(y))
        | (TypeAstKind::Slice(x), TypeAstKind::Slice(y))
        | (TypeAstKind::Chan(x), TypeAstKind::Chan(y))
        | (TypeAstKind::Shared(x), TypeAstKind::Shared(y)) => type_ast_eq(x, y),
        (TypeAstKind::Array(x_ty, x_len), TypeAstKind::Array(y_ty, y_len)) => {
            x_len == y_len && type_ast_eq(x_ty, y_ty)
        }
        (TypeAstKind::Map(xk, xv), TypeAstKind::Map(yk, yv))
        | (TypeAstKind::Result(xk, xv), TypeAstKind::Result(yk, yv)) => {
            type_ast_eq(xk, yk) && type_ast_eq(xv, yv)
        }
        (TypeAstKind::Tuple(xs), TypeAstKind::Tuple(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| type_ast_eq(x, y))
        }
        (
            TypeAstKind::FnPtr {
                params: xp,
                ret: xr,
                is_variadic: xv,
            },
            TypeAstKind::FnPtr {
                params: yp,
                ret: yr,
                is_variadic: yv,
            },
        )
        | (
            TypeAstKind::Closure {
                params: xp,
                ret: xr,
                is_variadic: xv,
            },
            TypeAstKind::Closure {
                params: yp,
                ret: yr,
                is_variadic: yv,
            },
        )
        | (
            TypeAstKind::FnPtr {
                params: xp,
                ret: xr,
                is_variadic: xv,
            },
            TypeAstKind::Closure {
                params: yp,
                ret: yr,
                is_variadic: yv,
            },
        )
        | (
            TypeAstKind::Closure {
                params: xp,
                ret: xr,
                is_variadic: xv,
            },
            TypeAstKind::FnPtr {
                params: yp,
                ret: yr,
                is_variadic: yv,
            },
        ) => {
            xv == yv
                && xp.len() == yp.len()
                && xp.iter().zip(yp.iter()).all(|(x, y)| type_ast_eq(x, y))
                && type_ast_eq(xr, yr)
        }
        (TypeAstKind::Interface, TypeAstKind::Interface) => true,
        _ => false,
    }
}

fn collect_trait_method_requirements(items: &[Item]) -> HashMap<String, Vec<TraitMethod>> {
    let mut out = HashMap::new();
    for item in items {
        if let Item::TypeAlias(alias) = item
            && alias.is_trait
        {
            out.insert(alias.name.clone(), alias.trait_methods.clone());
        }
    }
    out
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AstRefKind {
    None,
    Shared,
    Mut,
}

fn peel_type_ast_refs(ty: &TypeAst) -> (&TypeAst, AstRefKind) {
    match &ty.kind {
        TypeAstKind::Ref(inner) => match &inner.kind {
            TypeAstKind::MutRef(nested) => (nested.as_ref(), AstRefKind::Mut),
            _ => (inner.as_ref(), AstRefKind::Shared),
        },
        TypeAstKind::MutRef(inner) => (inner.as_ref(), AstRefKind::Mut),
        _ => (ty, AstRefKind::None),
    }
}

fn recv_param_compatible_ast(recv_ty: &TypeAst, param0_ty: &TypeAst) -> bool {
    let (recv_base, recv_kind) = peel_type_ast_refs(recv_ty);
    let (param_base, param_kind) = peel_type_ast_refs(param0_ty);
    if !type_ast_eq(recv_base, param_base) {
        return false;
    }
    match (recv_kind, param_kind) {
        (AstRefKind::None, AstRefKind::None)
        | (AstRefKind::Shared, AstRefKind::Shared)
        | (AstRefKind::Mut, AstRefKind::Mut)
        | (AstRefKind::None, AstRefKind::Shared)
        | (AstRefKind::None, AstRefKind::Mut)
        | (AstRefKind::Shared, AstRefKind::None)
        | (AstRefKind::Mut, AstRefKind::None)
        | (AstRefKind::Mut, AstRefKind::Shared) => true,
        (AstRefKind::Shared, AstRefKind::Mut) => false,
    }
}

fn function_ret_or_unit(func: &Function) -> TypeAst {
    func.ret_type.clone().unwrap_or(TypeAst {
        kind: TypeAstKind::Named("unit".to_string()),
        span: func.span.clone(),
    })
}

fn function_satisfies_trait_method(
    recv_ty: &TypeAst,
    required: &TraitMethod,
    candidate: &Function,
) -> bool {
    if !candidate.type_params.is_empty() {
        return false;
    }
    if candidate.is_variadic != required.is_variadic {
        return false;
    }
    let Some(recv_param) = candidate.params.first() else {
        return false;
    };
    if !recv_param_compatible_ast(recv_ty, &recv_param.ty) {
        return false;
    }
    if candidate.params.len().saturating_sub(1) != required.params.len() {
        return false;
    }
    for (cand, req) in candidate.params.iter().skip(1).zip(required.params.iter()) {
        if !type_ast_eq(&cand.ty, &req.ty) {
            return false;
        }
    }
    type_ast_eq(&function_ret_or_unit(candidate), &required.ret_type)
}

fn type_satisfies_trait_bound(
    recv_ty: &TypeAst,
    trait_name: &str,
    required_methods: &[TraitMethod],
    known_functions: &HashMap<String, Function>,
) -> Result<(), String> {
    for required in required_methods {
        let mut matched = false;
        for candidate in known_functions.values() {
            if logical_method_name(&candidate.name) != required.name {
                continue;
            }
            if function_satisfies_trait_method(recv_ty, required, candidate) {
                matched = true;
                break;
            }
        }
        if !matched {
            return Err(format!(
                "type argument `{}` does not satisfy bound `{}`: missing method `{}`",
                type_ast_key(recv_ty),
                trait_name,
                required.name
            ));
        }
    }
    Ok(())
}

fn validate_generic_type_bounds(
    template: &Function,
    type_args: &[TypeAst],
    trait_methods: &HashMap<String, Vec<TraitMethod>>,
    known_functions: &HashMap<String, Function>,
) -> Result<(), String> {
    for (param, arg) in template.type_params.iter().zip(type_args.iter()) {
        for bound in &param.bounds {
            match &bound.kind {
                TypeAstKind::Named(name) if name == "interface" => {}
                TypeAstKind::Named(name) => {
                    let required = trait_methods.get(name).ok_or_else(|| {
                        format!(
                            "unknown generic bound `{}` on type parameter `{}`",
                            name, param.name
                        )
                    })?;
                    type_satisfies_trait_bound(arg, name, required, known_functions).map_err(
                        |detail| {
                            format!(
                                "{} (while instantiating `{}` for `{}`)",
                                detail, template.name, param.name
                            )
                        },
                    )?;
                }
                _ => {
                    return Err(format!(
                        "generic bound on `{}` must be a trait name",
                        param.name
                    ));
                }
            }
        }
    }
    Ok(())
}

fn instantiate_generic_function(
    name: &str,
    type_args: &[TypeAst],
    templates: &HashMap<String, Function>,
    trait_methods: &HashMap<String, Vec<TraitMethod>>,
    known_functions: &HashMap<String, Function>,
    instances: &mut HashMap<String, String>,
    queue: &mut VecDeque<Function>,
) -> Result<String, String> {
    let max_depth = max_type_arg_depth(type_args);
    if max_depth > MONOMORPHIZATION_TYPE_DEPTH_LIMIT {
        return Err(format!(
            "generic instantiation depth exceeded for `{}` (max {}, got {}; possible recursive monomorphization)",
            name, MONOMORPHIZATION_TYPE_DEPTH_LIMIT, max_depth
        ));
    }
    let key = generic_instance_key(name, type_args);
    if let Some(existing) = instances.get(&key) {
        return Ok(existing.clone());
    }
    let template = templates
        .get(name)
        .ok_or_else(|| format!("generic template `{}` not found", name))?
        .clone();
    if template.type_params.len() != type_args.len() {
        return Err(format!(
            "generic function `{}` expects {} type arguments, got {}",
            name,
            template.type_params.len(),
            type_args.len()
        ));
    }
    validate_generic_type_bounds(&template, type_args, trait_methods, known_functions)?;

    let mut map: HashMap<String, TypeAst> = HashMap::new();
    for (param, arg) in template.type_params.iter().zip(type_args.iter()) {
        map.insert(param.name.clone(), arg.clone());
    }

    let mut specialized = template.clone();
    specialized.name = specialized_name(name, &key);
    specialized.type_params.clear();
    for param in &mut specialized.params {
        substitute_type_ast(&mut param.ty, &map);
    }
    if let Some(ret) = &mut specialized.ret_type {
        substitute_type_ast(ret, &map);
    }
    substitute_block_types(&mut specialized.body, &map);

    instances.insert(key, specialized.name.clone());
    queue.push_back(specialized.clone());
    Ok(specialized.name)
}

fn desugar_immediate_closure_call(
    params: &[ClosureParam],
    body: &BlockOrExpr,
    args: &[Expr],
    span: crate::frontend::ast::Span,
) -> Result<Block, String> {
    if params.len() != args.len() {
        return Err(format!(
            "closure expects {} arguments, got {}",
            params.len(),
            args.len()
        ));
    }
    let mut prep = Vec::new();
    for (param, arg) in params.iter().zip(args.iter()) {
        prep.push(Stmt::Let {
            name: param.name.clone(),
            ty: param.ty.clone(),
            init: arg.clone(),
            span: param.span.clone(),
        });
    }
    match body {
        BlockOrExpr::Block(block) => {
            let mut stmts = prep;
            stmts.extend(block.stmts.clone());
            Ok(Block {
                stmts,
                tail: block.tail.clone(),
                span,
            })
        }
        BlockOrExpr::Expr(expr) => Ok(Block {
            stmts: prep,
            tail: Some(expr.clone()),
            span,
        }),
    }
}

fn substitute_block_types(block: &mut Block, map: &HashMap<String, TypeAst>) {
    for stmt in &mut block.stmts {
        substitute_stmt_types(stmt, map);
    }
    if let Some(tail) = &mut block.tail {
        substitute_expr_types(tail, map);
    }
}

fn substitute_stmt_types(stmt: &mut Stmt, map: &HashMap<String, TypeAst>) {
    match stmt {
        Stmt::Let { ty, init, .. } | Stmt::Const { ty, init, .. } => {
            if let Some(ty) = ty {
                substitute_type_ast(ty, map);
            }
            substitute_expr_types(init, map);
        }
        Stmt::Assign { target, value, .. } => {
            substitute_expr_types(target, map);
            substitute_expr_types(value, map);
        }
        Stmt::Expr { expr, .. } | Stmt::Go { expr, .. } | Stmt::Defer { expr, .. } => {
            substitute_expr_types(expr, map);
        }
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                substitute_expr_types(expr, map);
            }
        }
        Stmt::While { cond, body, .. } => {
            substitute_expr_types(cond, map);
            substitute_block_types(body, map);
        }
        Stmt::Loop { body, .. } => substitute_block_types(body, map),
        Stmt::ForIn { iter, body, .. } => {
            substitute_expr_types(iter, map);
            substitute_block_types(body, map);
        }
        Stmt::ForRange {
            start, end, body, ..
        } => {
            substitute_expr_types(start, map);
            substitute_expr_types(end, map);
            substitute_block_types(body, map);
        }
        Stmt::Select { arms, .. } => {
            for arm in arms {
                match &mut arm.kind {
                    crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                        substitute_expr_types(chan, map);
                        substitute_expr_types(value, map);
                    }
                    crate::frontend::ast::SelectArmKind::Recv { chan, .. } => {
                        substitute_expr_types(chan, map);
                    }
                    crate::frontend::ast::SelectArmKind::After { ms } => {
                        substitute_expr_types(ms, map);
                    }
                    crate::frontend::ast::SelectArmKind::Default => {}
                }
                match &mut arm.body {
                    BlockOrExpr::Block(block) => substitute_block_types(block, map),
                    BlockOrExpr::Expr(expr) => substitute_expr_types(expr, map),
                }
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
}

fn substitute_expr_types(expr: &mut Expr, map: &HashMap<String, TypeAst>) {
    match &mut expr.kind {
        ExprKind::StructLit { fields, .. } => {
            for (_, expr) in fields {
                substitute_expr_types(expr, map);
            }
        }
        ExprKind::ArrayLit(items) => {
            for item in items {
                substitute_expr_types(item, map);
            }
        }
        ExprKind::Tuple(items) => {
            for item in items {
                substitute_expr_types(item, map);
            }
        }
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
            substitute_block_types(block, map);
        }
        ExprKind::If {
            cond,
            then_block,
            else_block,
        } => {
            substitute_expr_types(cond, map);
            substitute_block_types(then_block, map);
            if let Some(block) = else_block {
                substitute_block_types(block, map);
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            substitute_expr_types(scrutinee, map);
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    substitute_expr_types(guard, map);
                }
                match &mut arm.body {
                    BlockOrExpr::Block(block) => substitute_block_types(block, map),
                    BlockOrExpr::Expr(expr) => substitute_expr_types(expr, map),
                }
            }
        }
        ExprKind::Closure { params, body } => {
            for p in params {
                if let Some(ty) = &mut p.ty {
                    substitute_type_ast(ty, map);
                }
            }
            match body.as_mut() {
                BlockOrExpr::Block(block) => substitute_block_types(block, map),
                BlockOrExpr::Expr(expr) => substitute_expr_types(expr, map),
            }
        }
        ExprKind::Call {
            callee,
            type_args,
            args,
        } => {
            substitute_expr_types(callee, map);
            for ty in type_args {
                substitute_type_ast(ty, map);
            }
            for arg in args {
                substitute_expr_types(arg, map);
            }
        }
        ExprKind::Field { base, .. }
        | ExprKind::Unary { expr: base, .. }
        | ExprKind::Borrow { expr: base, .. }
        | ExprKind::Deref { expr: base }
        | ExprKind::Try { expr: base }
        | ExprKind::Recv { chan: base }
        | ExprKind::Close { chan: base }
        | ExprKind::After { ms: base } => substitute_expr_types(base, map),
        ExprKind::Cast { expr: inner, ty } => {
            substitute_expr_types(inner, map);
            substitute_type_ast(ty, map);
        }
        ExprKind::Index { base, index } => {
            substitute_expr_types(base, map);
            substitute_expr_types(index, map);
        }
        ExprKind::Binary { left, right, .. } => {
            substitute_expr_types(left, map);
            substitute_expr_types(right, map);
        }
        ExprKind::Send { chan, value } => {
            substitute_expr_types(chan, map);
            substitute_expr_types(value, map);
        }
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil
        | ExprKind::Ident(_) => {}
    }
}

fn substitute_type_ast(ty: &mut TypeAst, map: &HashMap<String, TypeAst>) {
    match &mut ty.kind {
        TypeAstKind::Named(name) => {
            if let Some(repl) = map.get(name) {
                *ty = repl.clone();
            }
        }
        TypeAstKind::Ref(inner)
        | TypeAstKind::MutRef(inner)
        | TypeAstKind::Own(inner)
        | TypeAstKind::Alias(inner)
        | TypeAstKind::Slice(inner)
        | TypeAstKind::Array(inner, _)
        | TypeAstKind::Chan(inner)
        | TypeAstKind::Shared(inner) => substitute_type_ast(inner, map),
        TypeAstKind::Map(key, value) | TypeAstKind::Result(key, value) => {
            substitute_type_ast(key, map);
            substitute_type_ast(value, map);
        }
        TypeAstKind::Tuple(items) => {
            for item in items {
                substitute_type_ast(item, map);
            }
        }
        TypeAstKind::FnPtr { params, ret, .. } | TypeAstKind::Closure { params, ret, .. } => {
            for param in params {
                substitute_type_ast(param, map);
            }
            substitute_type_ast(ret, map);
        }
        TypeAstKind::Interface => {}
    }
}

fn generic_instance_key(name: &str, type_args: &[TypeAst]) -> String {
    let mut key = name.to_string();
    for arg in type_args {
        key.push('#');
        key.push_str(&type_ast_key(arg));
    }
    key
}

fn max_type_arg_depth(type_args: &[TypeAst]) -> usize {
    type_args
        .iter()
        .map(type_ast_depth)
        .max()
        .unwrap_or_default()
}

fn type_ast_depth(root: &TypeAst) -> usize {
    let mut max_depth = 0usize;
    let mut stack: Vec<(&TypeAst, usize)> = vec![(root, 1)];
    while let Some((ty, depth)) = stack.pop() {
        if depth > max_depth {
            max_depth = depth;
        }
        match &ty.kind {
            TypeAstKind::Named(_) | TypeAstKind::Interface => {}
            TypeAstKind::Ref(inner)
            | TypeAstKind::MutRef(inner)
            | TypeAstKind::Own(inner)
            | TypeAstKind::Alias(inner)
            | TypeAstKind::Slice(inner)
            | TypeAstKind::Array(inner, _)
            | TypeAstKind::Chan(inner)
            | TypeAstKind::Shared(inner) => stack.push((inner, depth + 1)),
            TypeAstKind::Map(key, value) | TypeAstKind::Result(key, value) => {
                stack.push((key, depth + 1));
                stack.push((value, depth + 1));
            }
            TypeAstKind::Tuple(items) => {
                for item in items {
                    stack.push((item, depth + 1));
                }
            }
            TypeAstKind::FnPtr { params, ret, .. } | TypeAstKind::Closure { params, ret, .. } => {
                stack.push((ret, depth + 1));
                for param in params {
                    stack.push((param, depth + 1));
                }
            }
        }
    }
    max_depth
}

fn specialized_name(base: &str, key: &str) -> String {
    let mut hasher = DefaultHasher::new();
    key.hash(&mut hasher);
    format!("{}$g{:016x}", base, hasher.finish())
}

fn type_ast_key(ty: &TypeAst) -> String {
    match &ty.kind {
        TypeAstKind::Named(name) => name.clone(),
        TypeAstKind::Ref(inner) => format!("ref<{}>", type_ast_key(inner)),
        TypeAstKind::MutRef(inner) => format!("mutref<{}>", type_ast_key(inner)),
        TypeAstKind::Own(inner) => format!("own<{}>", type_ast_key(inner)),
        TypeAstKind::Alias(inner) => format!("alias<{}>", type_ast_key(inner)),
        TypeAstKind::Slice(inner) => format!("slice<{}>", type_ast_key(inner)),
        TypeAstKind::Array(inner, len) => format!("array<{},{}>", len, type_ast_key(inner)),
        TypeAstKind::Map(k, v) => format!("map<{},{}>", type_ast_key(k), type_ast_key(v)),
        TypeAstKind::Result(ok, err) => {
            format!("result<{},{}>", type_ast_key(ok), type_ast_key(err))
        }
        TypeAstKind::Chan(inner) => format!("chan<{}>", type_ast_key(inner)),
        TypeAstKind::Shared(inner) => format!("shared<{}>", type_ast_key(inner)),
        TypeAstKind::Interface => "interface".to_string(),
        TypeAstKind::Tuple(items) => {
            let mut s = "tuple<".to_string();
            for (idx, item) in items.iter().enumerate() {
                if idx > 0 {
                    s.push(',');
                }
                s.push_str(&type_ast_key(item));
            }
            s.push('>');
            s
        }
        TypeAstKind::FnPtr {
            params,
            ret,
            is_variadic,
        } => {
            let mut s = "fn<".to_string();
            for (idx, param) in params.iter().enumerate() {
                if idx > 0 {
                    s.push(',');
                }
                s.push_str(&type_ast_key(param));
            }
            s.push_str("->");
            s.push_str(&type_ast_key(ret));
            if *is_variadic {
                s.push_str(",...");
            }
            s.push('>');
            s
        }
        TypeAstKind::Closure {
            params,
            ret,
            is_variadic,
        } => {
            let mut s = "closure<".to_string();
            for (idx, param) in params.iter().enumerate() {
                if idx > 0 {
                    s.push(',');
                }
                s.push_str(&type_ast_key(param));
            }
            s.push_str("->");
            s.push_str(&type_ast_key(ret));
            if *is_variadic {
                s.push_str(",...");
            }
            s.push('>');
            s
        }
    }
}

#[cfg(test)]
mod tests {
    use super::compile_to_llvm;
    use crate::pkg::resolve::ModMode;
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    struct TempSource {
        path: PathBuf,
    }

    impl TempSource {
        fn new(source: &str) -> Self {
            let nonce = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("clock drift in test environment")
                .as_nanos();
            let path = std::env::temp_dir().join(format!(
                "gost-compile-test-{}-{}.gs",
                std::process::id(),
                nonce
            ));
            fs::write(&path, source).expect("failed to write temp source");
            Self { path }
        }

        fn path(&self) -> &Path {
            &self.path
        }
    }

    impl Drop for TempSource {
        fn drop(&mut self) {
            let _ = fs::remove_file(&self.path);
        }
    }

    #[test]
    fn sema_rejects_invalid_program_before_codegen() {
        let src = r#"
module main

fn main() -> i32 {
    string_len(1)
    return 0
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("invalid program should fail in sema");
        assert!(
            err.contains("string_len expects string"),
            "expected sema diagnostic, got:\n{err}"
        );
        assert!(
            !err.contains("internal compiler error (codegen)"),
            "invalid source should not surface codegen internal errors:\n{err}"
        );
    }

    #[test]
    fn valid_program_compiles_through_mir_pipeline() {
        let src = r#"
module main

fn main() -> i32 {
    return 0
}
"#;
        let file = TempSource::new(src);
        let llvm = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("valid source should compile");
        assert!(
            llvm.contains("define i32 @__gost_user_main()"),
            "expected lowered user main in llvm output"
        );
        assert!(
            llvm.contains("define i32 @main()"),
            "expected runtime entry wrapper in llvm output"
        );
    }

    #[test]
    fn package_header_is_rejected() {
        let src = r#"
package main

fn main() -> i32 {
    return 0
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("`package` header should be rejected");
        assert!(
            err.contains("expected `module`, found identifier `package`"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn invalid_prefixed_integer_literal_is_rejected() {
        let src = r#"
module main

fn main() -> i32 {
    let x = 0x
    return x
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("0x without digits must fail");
        assert!(
            err.contains("invalid integer literal: expected at least one digit after `0x`"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn overflowing_prefixed_integer_literal_is_rejected() {
        let src = r#"
module main

fn main() -> i32 {
    let x = 0xfffffffffffffffffffffffffffffffffffff
    return x
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("too-large prefixed integer literal must fail");
        assert!(
            err.contains("invalid integer literal: value after `0x` is too large"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn invalid_prefixed_integer_suffix_is_rejected() {
        let src = r#"
module main

fn main() -> i32 {
    let x = 0b102
    return x
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("invalid binary digit should fail at lexing boundary");
        assert!(
            err.contains("invalid integer literal: invalid digit or suffix `2` in `0b` literal"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn invalid_float_exponent_literal_is_rejected() {
        let src = r#"
module main

fn main() -> i32 {
    let x = 1e+
    return x
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("invalid exponent literal should fail at lexing boundary");
        assert!(
            err.contains(
                "invalid float literal: expected at least one digit after exponent marker"
            ),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn unterminated_string_literal_is_reported_at_lexing_boundary() {
        let src = r#"
module main

fn main() -> i32 {
    let s = "abc
    return 0
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("unterminated string should fail in lexer");
        assert!(
            err.contains("unterminated string literal"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn oversized_char_literal_is_reported_at_lexing_boundary() {
        let src = r#"
module main

fn main() -> i32 {
    let c = 'ab'
    return 0
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("multi-char literal should fail in lexer");
        assert!(
            err.contains("character literal must contain exactly one character"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn unicode_identifier_is_accepted() {
        let src = r#"
module main

fn main() -> i32 {
    let 한 = 1
    return 한
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("unicode identifier should parse and type-check");
    }

    #[test]
    fn parser_reports_expected_and_found_token_names() {
        let src = r#"
module main

fn main() -> i32 {
    let x = (1 + 2]
    return x
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("mismatched bracket should produce expected/found diagnostic");
        assert!(
            err.contains("expected `)`, found `]`"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn repr_integer_fieldless_enum_uses_tag_layout() {
        let src = r#"
module main

repr(u8) enum Color {
    Red
    Green
    Blue
}

fn main() -> i32 {
    let c = Color.Green
    let out = match c {
        Color.Green => 0,
        _ => 1,
    }
    if out != 0 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        let llvm = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("repr(integer) fieldless enum should compile");
        assert!(
            llvm.contains("%Color = type { i8 }"),
            "expected repr(u8) enum tag layout in llvm output"
        );
    }

    #[test]
    fn repr_integer_enum_with_payload_uses_tag_layout() {
        let src = r#"
module main

repr(u8) enum E {
    A(i32)
    B
}

fn main() -> i32 {
    let e = E.A(7)
    let out = match e {
        E.A(v) => v,
        E.B => 0,
    }
    if out != 7 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        let llvm = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("repr(integer) enum with payload should compile");
        assert!(
            llvm.contains("%E = type { i8, i8* }"),
            "expected repr(u8) payload enum tag layout in llvm output"
        );
    }

    #[test]
    fn repr_transparent_struct_requires_single_field() {
        let src = r#"
module main

repr(transparent) struct Wrap {
    a: i32
    b: i32
}

fn main() -> i32 {
    return 0
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("invalid repr(transparent) struct should fail");
        assert!(
            err.contains("repr(transparent) struct must have exactly one field"),
            "unexpected diagnostic:\n{err}"
        );
    }

    #[test]
    fn generic_callee_supports_capturing_closure_specialization() {
        let src = r#"
module main

fn apply[T](f: fn(T)->T, x: T) -> T {
    return f(x)
}

fn main() -> i32 {
    let base: i32 = 1
    let add = |x: i32| x + base
    let out = apply(add, 2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("generic call with capturing closure should compile");
    }

    #[test]
    fn capturing_closure_specialization_supports_variadic_fnptr() {
        let src = r#"
module main

fn apply(f: fn(i32, ...)->i32, x: i32) -> i32 {
    return f(x)
}

fn main() -> i32 {
    let base: i32 = 1
    let add = |x: i32| x + base
    let out = apply(add, 2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("capturing closure specialization with variadic fnptr should compile");
    }

    #[test]
    fn closure_typed_param_accepts_capturing_closure_argument() {
        let src = r#"
module main

fn apply(f: closure(i32)->i32, x: i32) -> i32 {
    return f(x)
}

fn main() -> i32 {
    let base: i32 = 1
    let add = |x: i32| x + base
    let out = apply(add, 2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("capturing closure should compile for closure-typed parameter");
    }

    #[test]
    fn closure_typed_local_binding_accepts_capturing_closure() {
        let src = r#"
module main

fn main() -> i32 {
    let base: i32 = 1
    let add: closure(i32)->i32 = |x: i32| x + base
    let out = add(2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("capturing closure should compile for closure-typed local binding");
    }

    #[test]
    fn closure_typed_alias_call_specializes_capturing_closure() {
        let src = r#"
module main

fn apply(f: closure(i32)->i32, x: i32) -> i32 {
    return f(x)
}

fn main() -> i32 {
    let run: fn(closure(i32)->i32, i32)->i32 = apply
    let base: i32 = 1
    let add: closure(i32)->i32 = |x: i32| x + base
    let out = run(add, 2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("capturing closure should specialize through closure-typed alias call");
    }

    #[test]
    fn capturing_closure_accepts_fnptr_typed_local_binding() {
        let src = r#"
module main

fn main() -> i32 {
    let base: i32 = 1
    let add: fn(i32)->i32 = |x: i32| x + base
    let out = add(2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("capturing closure should work for fnptr-typed local binding");
    }

    #[test]
    fn capturing_closure_specializes_through_fnptr_alias_call() {
        let src = r#"
module main

fn apply(f: fn(i32)->i32, x: i32) -> i32 {
    return f(x)
}

fn main() -> i32 {
    let run: fn(fn(i32)->i32, i32)->i32 = apply
    let base: i32 = 1
    let add: fn(i32)->i32 = |x: i32| x + base
    let out = run(add, 2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("capturing closure should specialize through fnptr alias call");
    }

    #[test]
    fn capturing_closure_specializes_through_variadic_fnptr_alias_call() {
        let src = r#"
module main

fn apply(f: fn(i32, ...)->i32, x: i32) -> i32 {
    return f(x)
}

fn main() -> i32 {
    let run: fn(fn(i32, ...)->i32, i32)->i32 = apply
    let base: i32 = 1
    let add: fn(i32, ...)->i32 = |x: i32| x + base
    let out = run(add, 2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("capturing closure should specialize through variadic fnptr alias call");
    }

    #[test]
    fn variadic_fnptr_call_accepts_non_capturing_closure_binding() {
        let src = r#"
module main

fn apply(f: fn(i32, ...)->i32, x: i32) -> i32 {
    return f(x)
}

fn main() -> i32 {
    let run: fn(fn(i32, ...)->i32, i32)->i32 = apply
    let add = |x: i32| x + 1
    let out = run(add, 2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("non-capturing closure binding should convert for variadic fnptr call");
    }

    #[test]
    fn variadic_fnptr_call_accepts_non_capturing_inline_closure() {
        let src = r#"
module main

fn apply(f: fn(i32, ...)->i32, x: i32) -> i32 {
    return f(x)
}

fn main() -> i32 {
    let run: fn(fn(i32, ...)->i32, i32)->i32 = apply
    let out = run(|x: i32| x + 1, 2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("inline closure should convert for variadic fnptr call");
    }

    #[test]
    fn closure_return_value_preserves_capture() {
        let src = r#"
module main

fn make_adder(base: i32) -> closure(i32)->i32 {
    let add: closure(i32)->i32 = |x: i32| x + base
    return add
}

fn main() -> i32 {
    let add = make_adder(1)
    let out = add(2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("capturing closure should survive return and call");
    }

    #[test]
    fn closure_identity_round_trip_preserves_capture() {
        let src = r#"
module main

fn id(f: closure(i32)->i32) -> closure(i32)->i32 {
    return f
}

fn main() -> i32 {
    let base: i32 = 1
    let add: closure(i32)->i32 = |x: i32| x + base
    let out = id(add)(2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("capturing closure should survive identity round-trip");
    }

    #[test]
    fn closure_factory_capture_expression_is_frozen_at_creation() {
        let src = r#"
module main

fn make_adder(base: i32) -> closure(i32)->i32 {
    let add: closure(i32)->i32 = |x: i32| x + base
    return add
}

fn main() -> i32 {
    let i: i32 = 1
    let add = make_adder(i + 1)
    i = 10
    let out = add(1)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("closure factory capture expression should be evaluated at creation");
    }

    #[test]
    fn capturing_closure_specialization_handles_fn_param_value_alias() {
        let src = r#"
module main

fn apply_alias(f: closure(i32)->i32, x: i32) -> i32 {
    let g: closure(i32)->i32 = f
    return g(x)
}

fn main() -> i32 {
    let base: i32 = 1
    let add: closure(i32)->i32 = |x: i32| x + base
    let out = apply_alias(add, 2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("capturing closure should specialize through value alias of fn param");
    }

    #[test]
    fn capturing_closure_fnptr_return_round_trip() {
        let src = r#"
module main

fn make_adder(base: i32) -> fn(i32)->i32 {
    let add: fn(i32)->i32 = |x: i32| x + base
    return add
}

fn main() -> i32 {
    let add = make_adder(1)
    let out = add(2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("capturing closure should survive fnptr-typed return and call");
    }

    #[test]
    fn capturing_closure_can_flow_through_struct_field() {
        let src = r#"
module main

struct Holder {
    f: closure(i32)->i32
}

fn main() -> i32 {
    let base: i32 = 1
    let add: closure(i32)->i32 = |x: i32| x + base
    let h = Holder { f = add }
    let out = h.f(2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("capturing closure should flow through struct field and call");
    }

    #[test]
    fn capturing_closure_struct_field_with_non_closure_fields_compiles() {
        let src = r#"
module main

struct Holder {
    f: closure(i32)->i32
    n: i32
}

fn main() -> i32 {
    let base: i32 = 1
    let h = Holder { f = |x: i32| x + base, n = 2 }
    let out = h.f(2)
    if out != 3 {
        return 1
    }
    if h.n != 2 {
        return 2
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("capturing closure should compile in mixed struct literal fields");
    }

    #[test]
    fn closure_array_index_call_compiles() {
        let src = r#"
module main

fn main() -> i32 {
    let base: i32 = 1
    let fs: [2]closure(i32)->i32 = [|x: i32| x + base, |x: i32| x + 2]
    let out = fs[0](2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("closure stored in array should be callable via index");
    }

    #[test]
    fn closure_param_round_trip_and_call_compiles() {
        let src = r#"
module main

fn forward(f: closure(i32)->i32) -> closure(i32)->i32 {
    return f
}

fn invoke(f: closure(i32)->i32, x: i32) -> i32 {
    return f(x)
}

fn main() -> i32 {
    let base: i32 = 1
    let add: closure(i32)->i32 = |x: i32| x + base
    let f = forward(add)
    let out = invoke(f, 2)
    if out != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("closure should round-trip through closure-typed params and return");
    }

    #[test]
    fn generic_inference_uses_assignment_expected_type() {
        let src = r#"
module main

fn id[T](x: T) -> T {
    return x
}

fn main() -> i32 {
    let x: i64 = 0 as i64
    x = id(1)
    if x != (1 as i64) {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("generic inference should use assignment expected type");
    }

    #[test]
    fn generic_inference_uses_literal_args_even_with_expected_context() {
        let src = r#"
module main

fn sink[T](x: T) -> i32 {
    return 0
}

fn main() -> i32 {
    let y: i32 = sink(1)
    return y
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("generic inference should still use literal argument types");
    }

    #[test]
    fn generic_inference_uses_monomorphic_call_argument_type() {
        let src = r#"
module main

fn get() -> i64 {
    return 7 as i64
}

fn sink[T](x: T) -> i32 {
    return 0
}

fn main() -> i32 {
    return sink(get())
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("generic inference should use argument call return type");
    }

    #[test]
    fn recursive_generic_monomorphization_depth_is_rejected() {
        let src = r#"
module main

fn recurse[T](x: T) -> i32 {
    let y = &x
    return recurse[ref[T]](y)
}

fn main() -> i32 {
    return recurse[i32](0)
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false).expect_err(
            "recursive generic monomorphization should be rejected before stack overflow",
        );
        assert!(
            err.contains("E1201"),
            "expected diagnostic code E1201, got: {err}"
        );
        assert!(
            err.contains("generic instantiation depth exceeded for `recurse`"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn recursive_generic_with_same_type_args_still_compiles() {
        let src = r#"
module main

fn recurse_same[T](x: T) -> T {
    return recurse_same[T](x)
}

fn main() -> i32 {
    let _ = recurse_same[i32](0)
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("generic recursion with stable type args should compile");
    }

    #[test]
    fn generic_inference_rejects_conflicting_argument_types() {
        let src = r#"
module main

fn same[T](a: T, b: T) -> T {
    return a
}

fn main() -> i32 {
    let _ = same(1, "x")
    return 0
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("conflicting generic argument types must be rejected");
        assert!(
            err.contains("argument type mismatch"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn generic_inference_rejects_incompatible_expected_return_type() {
        let src = r#"
module main

fn make[T](x: T) -> T {
    return x
}

fn main() -> i32 {
    let v: string = make(1)
    if v == "" {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("incompatible expected return type should be rejected");
        assert!(
            err.contains("let initializer type mismatch") || err.contains("argument type mismatch"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn generic_inference_rejects_unconstrained_type_params_without_defaulting() {
        let src = r#"
module main

fn tag[T]() -> i32 {
    return 0
}

fn main() -> i32 {
    tag()
    return 0
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("unconstrained generic call should require explicit type args");
        assert!(
            err.contains("cannot infer generic type arguments"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn range_for_supports_index_and_value_binders() {
        let src = r#"
module main

fn main() -> i32 {
    let sum = 0
    for i, v in 1..=3 {
        sum += i + v
    }
    if sum != 9 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("range for with two binders should compile");
    }

    #[test]
    fn match_expr_allows_omitting_last_trailing_comma() {
        let src = r#"
module main

fn main() -> i32 {
    let x: i32 = 1
    let y: i32 = match x { 1 => 2, _ => 3 }
    return y
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("match expression should allow omitting trailing comma on last arm");
    }

    #[test]
    fn field_access_works_through_ref_and_mutref_base() {
        let src = r#"
module main

struct S {
    x: i32
}

fn setx(p: mutref[S]) {
    p.x = 1
}

fn getx(p: ref[S]) -> i32 {
    return p.x
}

fn main() -> i32 {
    let s = S { x = 0 }
    setx(&mut s)
    return getx(&s)
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("field access through ref/mutref should compile");
    }

    #[test]
    fn match_with_try_supports_variant_patterns_without_bindings() {
        let src = r#"
module main

fn one() -> result[i32, error] {
    return Result.Ok[i32, error](1)
}

fn calc() -> result[i32, error] {
    let r = Result.Ok[i32, error](0)
    let v = match r {
        Result.Ok(_) => one()?,
        Result.Err(_) => 0,
    }
    return Result.Ok[i32, error](v)
}

fn main() -> i32 {
    let out = match calc() {
        Result.Ok(v) => v,
        Result.Err(_) => -1,
    }
    if out != 1 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("match with try should support non-literal variant patterns");
    }

    #[test]
    fn match_with_try_supports_guard_and_bindings() {
        let src = r#"
module main

fn bump(x: i32) -> result[i32, error] {
    return Result.Ok[i32, error](x + 1)
}

fn calc() -> result[i32, error] {
    let r = Result.Ok[i32, error](1)
    let v = match r {
        Result.Ok(x) if x == 1 => bump(x)?,
        Result.Ok(x) => x,
        Result.Err(_) => 0,
    }
    return Result.Ok[i32, error](v)
}

fn main() -> i32 {
    let out = match calc() {
        Result.Ok(v) => v,
        Result.Err(_) => -1,
    }
    if out != 2 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("match with try should support guards and pattern bindings");
    }

    #[test]
    fn parser_accepts_non_c_repr_forms() {
        let src = r#"
module main

repr(u8)
enum Flag {
    Off
    On
}

fn main() -> i32 {
    let f = Flag.On
    let out = match f {
        Flag.On => 1,
        _ => 0,
    }
    if out != 1 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("repr(u8) enum should parse and compile");
    }

    #[test]
    fn fieldless_enum_without_repr_uses_tag_layout() {
        let src = r#"
module main

enum State {
    Idle
    Busy
}

fn main() -> i32 {
    let s = State.Busy
    let out = match s {
        State.Busy => 1,
        _ => 0,
    }
    return out
}
"#;
        let file = TempSource::new(src);
        let llvm = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("fieldless enum should use tag-only layout");
        assert!(
            llvm.contains("%State = type { i32 }"),
            "expected fieldless enum tag-only layout in llvm output"
        );
    }

    #[test]
    fn global_let_allows_inferred_type_and_runtime_initializer() {
        let src = r#"
module main

let g = add(1, 2)

fn add(a: i32, b: i32) -> i32 {
    return a + b
}

fn main() -> i32 {
    if g != 3 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        let llvm = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("global let with inferred type and non-const init should compile");
        assert!(
            llvm.contains("call void @__gost_fn___gost_global_init_user_"),
            "expected global runtime init call in main path"
        );
    }

    #[test]
    fn user_variadic_function_compiles() {
        let src = r#"
module main

fn pick(head: i32, ...) -> i32 {
    return head
}

fn main() -> i32 {
    return pick(7, 8, 9)
}
"#;
        let file = TempSource::new(src);
        let llvm = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("non-extern variadic function should compile");
        assert!(
            llvm.contains("define i32 @__gost_fn_pick_") && llvm.contains("(i32 %arg0, ...)"),
            "expected variadic user function definition"
        );
    }

    #[test]
    fn dyn_trait_object_dispatch_compiles() {
        let src = r#"
module main

trait Greeter {
    fn greet() -> i32
}

copy struct User {
    id: i32
}

impl Greeter for User {
    fn greet() -> i32 {
        return self.id + 1
    }
}

fn call(v: dyn Greeter) -> i32 {
    return v.greet()
}

fn main() -> i32 {
    let u = User { id = 1 }
    let any = u as dyn Greeter
    if call(any) != 2 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("dyn trait object dispatch should compile");
    }

    #[test]
    fn trait_object_cast_rejects_missing_impl() {
        let src = r#"
module main

trait Greeter {
    fn greet() -> i32
}

copy struct User {
    id: i32
}

fn main() -> i32 {
    let u = User { id = 1 }
    let _ = u as Greeter
    return 0
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("casting to trait object without impl should fail");
        assert!(
            err.contains("does not implement trait `Greeter`"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn trait_object_rejects_methods_outside_trait() {
        let src = r#"
module main

trait Greeter {
    fn greet() -> i32
}

copy struct User {
    id: i32
}

impl Greeter for User {
    fn greet() -> i32 {
        return self.id + 1
    }
}

impl User {
    fn only_user() -> i32 {
        return 7
    }
}

fn main() -> i32 {
    let u = User { id = 1 }
    let any = u as Greeter
    return any.only_user()
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("trait object should reject methods outside declared trait");
        assert!(
            err.contains("unknown method `only_user` on trait object `Greeter`"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn recursive_struct_with_own_indirection_compiles() {
        let src = r#"
module main

struct Node {
    next: own[Node]
}

fn main() -> i32 {
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("recursive struct through own indirection should compile");
    }

    #[test]
    fn direct_recursive_struct_cycle_is_rejected() {
        let src = r#"
module main

struct Bad {
    next: Bad
}

fn main() -> i32 {
    return 0
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("direct recursive value cycle should be rejected");
        assert!(
            err.contains("recursive value cycle is not allowed"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn mutual_recursive_structs_with_indirection_compile() {
        let src = r#"
module main

struct A {
    b: own[B]
}

struct B {
    a: own[A]
}

fn main() -> i32 {
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("mutual recursion via own indirection should compile");
    }

    #[test]
    fn import_wrapper_preserves_variadic_alias() {
        use crate::frontend::ast::{
            Block, Expr, ExprKind, FileAst, Function, ImportSpec, Item, Param, Span, TypeAst,
            TypeAstKind, Visibility,
        };

        let span = Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        };
        let i32_ty = TypeAst {
            kind: TypeAstKind::Named("i32".to_string()),
            span: span.clone(),
        };
        let func = Function {
            vis: Visibility::Public,
            name: "pick".to_string(),
            type_params: Vec::new(),
            params: vec![Param {
                name: "head".to_string(),
                ty: i32_ty.clone(),
                span: span.clone(),
            }],
            is_variadic: true,
            ret_type: Some(i32_ty),
            is_extern: false,
            is_unsafe: false,
            extern_abi: None,
            body: Block {
                stmts: Vec::new(),
                tail: Some(Expr {
                    id: 0,
                    kind: ExprKind::Ident("head".to_string()),
                    span: span.clone(),
                }),
                span: span.clone(),
            },
            span: span.clone(),
        };
        let file = FileAst {
            package: "pkg".to_string(),
            imports: Vec::new(),
            items: vec![Item::Function(func)],
        };
        let spec = ImportSpec {
            path: "pkg".to_string(),
            alias: Some("pkg".to_string()),
            only: None,
        };
        let mut next_expr_id = 100;
        let wrappers = super::build_import_wrappers(&file, &spec, &mut next_expr_id);
        assert_eq!(wrappers.len(), 1, "expected one aliased wrapper");
        let Item::Function(alias_fn) = &wrappers[0] else {
            panic!("expected function wrapper");
        };
        assert_eq!(alias_fn.name, "pkg.pick");
        assert!(alias_fn.is_variadic, "variadic marker must be preserved");
        let tail_id = alias_fn
            .body
            .tail
            .as_ref()
            .map(|expr| expr.id)
            .expect("aliased function should keep body tail");
        assert_ne!(tail_id, 0, "aliased variadic body ids must be refreshed");
    }

    #[test]
    fn map_supports_non_64_integer_key_types() {
        let src = r#"
module main

fn main() -> i32 {
    let mp = make_map[i32, i32](4 as i64)
    map_set[i32, i32](&mut mp, 1, 10)
    let _ = map_get[i32, i32](&mp, 1)
    return 0
}
"#;
        let file = TempSource::new(src);
        let llvm = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("map with i32 key should compile");
        assert!(
            llvm.contains("@__gost_map_new(i32 1"),
            "expected signed integer map key kind in runtime call"
        );
    }

    #[test]
    fn map_supports_bool_and_char_key_types() {
        let src = r#"
module main

fn main() -> i32 {
    let mb = make_map[bool, i32](4 as i64)
    map_set[bool, i32](&mut mb, true, 1)
    let _ = map_get[bool, i32](&mb, true)

    let mc = make_map[char, i32](4 as i64)
    map_set[char, i32](&mut mc, 'x', 2)
    let _ = map_get[char, i32](&mc, 'x')
    return 0
}
"#;
        let file = TempSource::new(src);
        let llvm = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("map with bool/char keys should compile");
        assert!(
            llvm.contains("@__gost_map_new(i32 2"),
            "expected unsigned integer map key kind in runtime call"
        );
    }

    #[test]
    fn map_supports_repr_integer_enum_keys() {
        let src = r#"
module main

repr(u8)
enum Key {
    A
    B
}

fn main() -> i32 {
    let m = make_map[Key, i32](4 as i64)
    map_set[Key, i32](&mut m, Key.B, 7)
    let _ = map_get[Key, i32](&m, Key.B)
    return 0
}
"#;
        let file = TempSource::new(src);
        let llvm = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("map with repr(integer) enum key should compile");
        assert!(
            llvm.contains("@__gost_map_new(i32 2"),
            "expected unsigned integer map key kind for repr(u8) enum"
        );
    }

    #[test]
    fn map_supports_fieldless_enum_keys_without_repr() {
        let src = r#"
module main

enum Key {
    A
    B
}

fn main() -> i32 {
    let m = make_map[Key, i32](4 as i64)
    map_set[Key, i32](&mut m, Key.B, 7)
    let _ = map_get[Key, i32](&m, Key.B)
    return 0
}
"#;
        let file = TempSource::new(src);
        let llvm = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("map with fieldless enum key should compile");
        assert!(
            llvm.contains("@__gost_map_new(i32 2"),
            "expected unsigned integer map key kind for fieldless enum"
        );
    }

    #[test]
    fn map_supports_bytewise_struct_keys() {
        let src = r#"
module main

struct Key {
    a: i32
    b: u8
}

fn main() -> i32 {
    let m = make_map[Key, i32](4 as i64)
    map_set[Key, i32](&mut m, Key { a = 1, b = 2 }, 7)
    let _ = map_get[Key, i32](&m, Key { a = 1, b = 2 })
    return 0
}
"#;
        let file = TempSource::new(src);
        let llvm = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("map with bytewise struct key should compile");
        assert!(
            llvm.contains("@__gost_map_new(i32 4"),
            "expected bytewise map key kind in runtime call"
        );
        assert!(
            llvm.contains("i32 (i8*, i8*)* @__gost_map_keyop_")
                && llvm.contains("i64 (i8*)* @__gost_map_keyop_")
                && llvm.contains("void (i8*, i8*)* @__gost_map_keyop_")
                && llvm.contains("void (i8*)* @__gost_map_keyop_"),
            "expected map key callback vtable thunks in runtime call"
        );
    }

    #[test]
    fn global_let_allows_inferred_collection_type() {
        let src = r#"
module main

fn make() -> []i32 {
    return make_slice[i32](2 as i64, 0 as i32)
}

let g = make()

fn main() -> i32 {
    if slice_len[i32](&g) != 2 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("global let should allow inferred non-scalar type");
    }

    #[test]
    fn collection_view_constraints_are_relaxed() {
        let src = r#"
module main

fn main() -> i32 {
    let m = make_map[i32, []i32](4 as i64)
    let v = make_slice[i32](2 as i64, 0 as i32)
    map_set[i32, []i32](&mut m, 1, v)
    let _ = map_get[i32, []i32](&m, 1)

    let _c = make_chan[[]i32](1 as i32)

    let _arr: [2][]i32 = [make_slice[i32](1 as i64, 0 as i32), make_slice[i32](1 as i64, 0 as i32)]
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("collection/view relaxed constraints should compile");
    }

    #[test]
    fn for_in_by_value_accepts_non_copy_slice_elements() {
        let src = r#"
module main

fn one(v: i32) -> []i32 {
    let s = make_slice[i32](0 as i64, 0 as i64)
    slice_push[i32](&mut s, v)
    return s
}

fn main() -> i32 {
    let outer = make_slice[[]i32](0 as i64, 0 as i64)
    slice_push[[]i32](&mut outer, one(1))
    slice_push[[]i32](&mut outer, one(2))

    let total = 0
    for inner in outer {
        total += slice_len[i32](&inner)
    }
    if total != 2 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("for-in by-value should support non-Copy slice elements");
    }

    #[test]
    fn for_in_borrowed_map_rejects_mutation_of_same_map() {
        let src = r#"
module main

fn main() -> i32 {
    let m = make_map[i32, i32](4 as i64)
    map_set[i32, i32](&mut m, 1, 10)
    for k, v in &m {
        let _ = v
        map_set[i32, i32](&mut m, k, 11)
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("mutating borrowed map during for-in should be rejected");
        assert!(
            err.contains("mutable borrow conflicts with existing borrows"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn own_alias_intrinsics_compile_and_preserve_value() {
        let src = r#"
module main

fn main() -> i32 {
    let o = own_new[i32](1)
    {
        let m = own_borrow_mut[i32](&mut o)
        *m = 7
    }
    let a = freeze[i32](o)
    let r = alias_borrow[i32](&a)
    if *r != 7 {
        return 1
    }
    let o2 = own_new[i32](9)
    let v = own_into_value[i32](o2)
    if v != 9 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("own/alias intrinsics should compile and type-check");
    }

    #[test]
    fn own_and_alias_can_escape_in_struct_and_return() {
        let src = r#"
module main

struct S {
    owner: own[i32]
    ro: alias[i32]
}

fn build() -> S {
    let o = own_new[i32](3)
    let a = freeze[i32](own_new[i32](4))
    return S { owner = o, ro = a }
}

fn main() -> i32 {
    let s = build()
    let r = alias_borrow[i32](&s.ro)
    if *r != 4 {
        return 1
    }
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("own/alias should be allowed in escaping positions");
    }

    #[test]
    fn alias_cannot_be_mutably_borrowed_as_owner() {
        let src = r#"
module main

fn main() -> i32 {
    let a = freeze[i32](own_new[i32](1))
    let _ = own_borrow_mut[i32](&mut a)
    return 0
}
"#;
        let file = TempSource::new(src);
        let err = compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect_err("alias mutable owner borrow must be rejected");
        assert!(
            err.contains("cannot mutably borrow alias[T]"),
            "unexpected diagnostic: {err}"
        );
    }

    #[test]
    fn ref_to_unit_type_is_codegen_compatible() {
        let src = r#"
module main

fn make() {
}

fn touch(p: ref[unit]) {
    let _ = *p
}

fn main() -> i32 {
    let u = make()
    touch(&u)
    return 0
}
"#;
        let file = TempSource::new(src);
        compile_to_llvm(file.path(), ModMode::Readonly, true, false)
            .expect("ref[unit] should compile through codegen");
    }

    #[test]
    fn compiler_reports_diverse_error_categories() {
        struct Case<'a> {
            name: &'a str,
            src: &'a str,
            expected: &'a [&'a str],
        }

        let cases = vec![
            Case {
                name: "undefined_name",
                src: r#"
module main

fn main() -> i32 {
    let x = missing_name
    return 0
}
"#,
                expected: &["E1002", "undefined name `missing_name`"],
            },
            Case {
                name: "unknown_type",
                src: r#"
module main

fn main() -> i32 {
    let x: strnig = "a"
    let _ = x
    return 0
}
"#,
                expected: &["E1003", "unknown type `strnig`"],
            },
            Case {
                name: "unknown_function",
                src: r#"
module main

fn main() -> i32 {
    let xs = make_slice[i32](0 as i64, 0 as i64)
    let _ = filter(iter(&xs), pritn)
    return 0
}
"#,
                expected: &["E1004", "unknown function `pritn`"],
            },
            Case {
                name: "unknown_field",
                src: r#"
module main

struct S {
    x: i32
}

fn main() -> i32 {
    let s = S { x = 1 }
    let _ = s.y
    return 0
}
"#,
                expected: &["E1101", "unknown field `y`"],
            },
            Case {
                name: "unknown_method",
                src: r#"
module main

struct S {
    x: i32
}

fn main() -> i32 {
    let s = S { x = 1 }
    s.nope()
    return 0
}
"#,
                expected: &["E1102", "unknown method `nope`"],
            },
            Case {
                name: "unknown_variant",
                src: r#"
module main

enum E {
    A
}

fn main() -> i32 {
    let _ = E.B
    return 0
}
"#,
                expected: &["E1103", "unknown enum variant"],
            },
            Case {
                name: "receiver_not_addressable",
                src: r#"
module main

struct P {
    x: i32
}

fn read(self: ref[P]) -> i32 {
    return self.x
}

fn make() -> P {
    return P { x = 1 }
}

                fn main() -> i32 {
    let _ = make().read()
    return 0
}
"#,
                expected: &["E1104", "temporary value is not addressable"],
            },
            Case {
                name: "break_outside_loop",
                src: r#"
module main

fn main() -> i32 {
    break
    return 0
}
"#,
                expected: &["break outside loop"],
            },
            Case {
                name: "return_type_mismatch",
                src: r#"
module main

fn main() -> i32 {
    return true
}
"#,
                expected: &["return type mismatch"],
            },
            Case {
                name: "map_iteration_mutation_conflict",
                src: r#"
module main

fn main() -> i32 {
    let m = make_map[i32, i32](4 as i64)
    map_set[i32, i32](&mut m, 1, 10)
    for k, v in &m {
        let _ = v
        map_set[i32, i32](&mut m, k, 11)
    }
    return 0
}
"#,
                expected: &["mutable borrow conflicts with existing borrows"],
            },
        ];

        for case in cases {
            let file = TempSource::new(case.src);
            let err =
                compile_to_llvm(file.path(), ModMode::Readonly, true, false).expect_err(case.name);
            for needle in case.expected {
                assert!(
                    err.contains(needle),
                    "case `{}` expected diagnostic containing `{}` but got:\n{}",
                    case.name,
                    needle,
                    err
                );
            }
        }
    }
}
