use std::fs;
use std::path::{Path, PathBuf};

use crate::frontend::diagnostic::{format_diagnostic, Diagnostic, Diagnostics};
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::ast::{Block, Expr, ExprKind, FileAst, Item, Stmt};
use crate::sema::analyze;
use crate::pkg::resolve::{self, ModMode};

pub fn compile_to_llvm(
    path: &Path,
    mode: ModMode,
    offline: bool,
    want_fetch: bool,
) -> Result<String, String> {
    let source = fs::read_to_string(path).map_err(|e| e.to_string())?;
    let mut next_expr_id = 0;
    let (file, next_after_main) =
        parse_source_with_ids(&source, next_expr_id, Some(path))
            .map_err(|e| format!("{}:\n{}", path.display(), e))?;
    next_expr_id = next_after_main;
    let mut imported_items = Vec::new();
    let mut visited = std::collections::HashSet::new();
    let mut std_funcs = std::collections::HashSet::new();
    let cwd = path.parent().map(PathBuf::from).unwrap_or_else(|| PathBuf::from("."));
    match resolve::try_load_ctx_from_entry(cwd, mode, offline, want_fetch) {
        Ok(Some(ctx)) => {
            load_imports_mod(
                &ctx,
                &file.imports,
                &mut next_expr_id,
                &mut imported_items,
                &mut visited,
                &mut std_funcs,
            )?;
        }
        Ok(None) => {
            let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            load_imports(
                &manifest_dir,
                &file.imports,
                &mut next_expr_id,
                &mut imported_items,
                &mut visited,
                &mut std_funcs,
            )?;
        }
        Err(err) => {
            let mut diags = Diagnostics::default();
            let d = Diagnostic::new(format!("{err}"), None).code("E2000");
            diags.push_diag(d);
            return Err(render_diags(&diags, &source, Some(path)));
        }
    }
    let mut items = imported_items;
    items.extend(file.items.clone());
    let merged = FileAst {
        package: file.package.clone(),
        imports: file.imports.clone(),
        items,
    };
    let program = match analyze(&merged, &std_funcs) {
        Ok(program) => program,
        Err(diags) => return Err(render_diags(&diags, &source, Some(path))),
    };
    if file_contains_asm_labels_or_goto(&program.file) {
        let module = crate::codegen::emit_llvm(&program).map_err(|e| e.to_string())?;
        return Ok(module.text);
    }
    let mut mir = crate::mir::lower::lower_program(&program).map_err(|e| e.to_string())?;
    for func in &mut mir.functions {
        crate::mir::passes::build_cleanup_chains(func).map_err(|e| e.to_string())?;
        crate::mir::passes::linear_check(func).map_err(|e| e.to_string())?;
        crate::mir::passes::verify_mir_strict(func).map_err(|e| e.to_string())?;
    }
    let module =
        crate::codegen::emit_llvm_from_mir(&program, &mir).map_err(|e| e.to_string())?;
    Ok(module.text)
}

fn file_contains_asm_labels_or_goto(file: &FileAst) -> bool {
    for item in &file.items {
        if let Item::Function(func) = item {
            if block_contains_asm_labels_or_goto(&func.body) {
                return true;
            }
        }
    }
    false
}

fn block_contains_asm_labels_or_goto(block: &Block) -> bool {
    for stmt in &block.stmts {
        match stmt {
            Stmt::Let { init, .. } => {
                if expr_contains_asm_labels_or_goto(init) {
                    return true;
                }
            }
            Stmt::Assign { target, value, .. } => {
                if expr_contains_asm_labels_or_goto(target)
                    || expr_contains_asm_labels_or_goto(value)
                {
                    return true;
                }
            }
            Stmt::Expr { expr, .. } | Stmt::Go { expr, .. } | Stmt::Defer { expr, .. } => {
                if expr_contains_asm_labels_or_goto(expr) {
                    return true;
                }
            }
            Stmt::Return { expr, .. } => {
                if let Some(expr) = expr {
                    if expr_contains_asm_labels_or_goto(expr) {
                        return true;
                    }
                }
            }
            Stmt::While { cond, body, .. } => {
                if expr_contains_asm_labels_or_goto(cond)
                    || block_contains_asm_labels_or_goto(body)
                {
                    return true;
                }
            }
            Stmt::Loop { body, .. } => {
                if block_contains_asm_labels_or_goto(body) {
                    return true;
                }
            }
            Stmt::ForIn { iter, body, .. } => {
                if expr_contains_asm_labels_or_goto(iter)
                    || block_contains_asm_labels_or_goto(body)
                {
                    return true;
                }
            }
            Stmt::Select { arms, .. } => {
                for arm in arms {
                    use crate::frontend::ast::BlockOrExpr;
                    let body_hit = match &arm.body {
                        BlockOrExpr::Block(block) => block_contains_asm_labels_or_goto(block),
                        BlockOrExpr::Expr(expr) => expr_contains_asm_labels_or_goto(expr),
                    };
                    if body_hit {
                        return true;
                    }
                }
            }
            Stmt::Break { .. } | Stmt::Continue { .. } => {}
        }
    }
    if let Some(tail) = &block.tail {
        if expr_contains_asm_labels_or_goto(tail) {
            return true;
        }
    }
    false
}

fn expr_contains_asm_labels_or_goto(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { callee, args, .. } => {
            if matches!(&callee.kind, ExprKind::Ident(name) if name == "asm_label" || name == "asm_goto")
            {
                return true;
            }
            if expr_contains_asm_labels_or_goto(callee) {
                return true;
            }
            for arg in args {
                if expr_contains_asm_labels_or_goto(arg) {
                    return true;
                }
            }
            false
        }
        ExprKind::StructLit { fields, .. } => fields
            .iter()
            .any(|(_, e)| expr_contains_asm_labels_or_goto(e)),
        ExprKind::Tuple(items) => items.iter().any(expr_contains_asm_labels_or_goto),
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
            block_contains_asm_labels_or_goto(block)
        }
        ExprKind::If {
            cond,
            then_block,
            else_block,
        } => {
            expr_contains_asm_labels_or_goto(cond)
                || block_contains_asm_labels_or_goto(then_block)
                || else_block
                    .as_ref()
                    .map(|b| block_contains_asm_labels_or_goto(b))
                    .unwrap_or(false)
        }
        ExprKind::Match { scrutinee, arms } => {
            if expr_contains_asm_labels_or_goto(scrutinee) {
                return true;
            }
            for arm in arms {
                use crate::frontend::ast::BlockOrExpr;
                let hit = match &arm.body {
                    BlockOrExpr::Block(block) => block_contains_asm_labels_or_goto(block),
                    BlockOrExpr::Expr(expr) => expr_contains_asm_labels_or_goto(expr),
                };
                if hit {
                    return true;
                }
            }
            false
        }
        ExprKind::Field { base, .. } => expr_contains_asm_labels_or_goto(base),
        ExprKind::Index { base, index } => {
            expr_contains_asm_labels_or_goto(base) || expr_contains_asm_labels_or_goto(index)
        }
        ExprKind::Unary { expr, .. }
        | ExprKind::Borrow { expr, .. }
        | ExprKind::Deref { expr }
        | ExprKind::Try { expr } => expr_contains_asm_labels_or_goto(expr),
        ExprKind::Binary { left, right, .. } => {
            expr_contains_asm_labels_or_goto(left) || expr_contains_asm_labels_or_goto(right)
        }
        ExprKind::Send { chan, value } => {
            expr_contains_asm_labels_or_goto(chan) || expr_contains_asm_labels_or_goto(value)
        }
        ExprKind::Recv { chan } | ExprKind::Close { chan } => expr_contains_asm_labels_or_goto(chan),
        ExprKind::After { ms } => expr_contains_asm_labels_or_goto(ms),
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil
        | ExprKind::Ident(_) => false,
    }
}

fn parse_source_with_ids(
    source: &str,
    next_expr_id: usize,
    file_path: Option<&Path>,
) -> Result<(FileAst, usize), String> {
    let tokens = Lexer::new(source).lex_all();
    let mut parser = Parser::new_with_expr_id(tokens, next_expr_id);
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
    imports: &[String],
    next_expr_id: &mut usize,
    items: &mut Vec<Item>,
    visited: &mut std::collections::HashSet<String>,
    std_funcs: &mut std::collections::HashSet<String>,
) -> Result<(), String> {
    for import in imports {
        if !visited.insert(import.clone()) {
            continue;
        }
        let is_std = import.starts_with("std/");
        let paths = resolve_import_paths(root, import)?;
        for path in paths {
            let source = fs::read_to_string(&path).map_err(|e| e.to_string())?;
            let (file, next) = parse_source_with_ids(&source, *next_expr_id, Some(&path))
                .map_err(|e| format!("{}:\n{}", path.display(), e))?;
            if let Some(expected) = import.split('/').last() {
                if file.package != expected {
                    return Err(format!(
                        "import {}: package name mismatch (expected {}, found {})",
                        import, expected, file.package
                    ));
                }
            }
            *next_expr_id = next;
            if is_std {
                for item in &file.items {
                    if let Item::Function(func) = item {
                        std_funcs.insert(func.name.clone());
                    }
                }
            }
            items.extend(file.items);
        }
    }
    Ok(())
}

fn load_imports_mod(
    ctx: &resolve::ResolveCtx,
    imports: &[String],
    next_expr_id: &mut usize,
    items: &mut Vec<Item>,
    visited: &mut std::collections::HashSet<String>,
    std_funcs: &mut std::collections::HashSet<String>,
) -> Result<(), String> {
    for import in imports {
        if !visited.insert(import.clone()) {
            continue;
        }
        let is_std = import.starts_with("std/");
        let pkg = resolve::resolve_import_to_package(ctx, import)
            .map_err(|e| e.to_string())?;
        for path in pkg.files {
            let source = fs::read_to_string(&path).map_err(|e| e.to_string())?;
            let (file, next) = parse_source_with_ids(&source, *next_expr_id, Some(&path))
                .map_err(|e| format!("{}:\n{}", path.display(), e))?;
            if let Some(expected) = import.split('/').last() {
                if file.package != expected {
                    return Err(format!(
                        "import {}: package name mismatch (expected {}, found {})",
                        import, expected, file.package
                    ));
                }
            }
            *next_expr_id = next;
            if is_std {
                for item in &file.items {
                    if let Item::Function(func) = item {
                        std_funcs.insert(func.name.clone());
                    }
                }
            }
            items.extend(file.items);
            load_imports_mod(ctx, &file.imports, next_expr_id, items, visited, std_funcs)?;
        }
    }
    Ok(())
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
