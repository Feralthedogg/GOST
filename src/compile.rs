use std::fs;
use std::path::{Path, PathBuf};

use crate::frontend::diagnostic::{format_diagnostic, Diagnostic, Diagnostics};
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::ast::{FileAst, Item};
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
