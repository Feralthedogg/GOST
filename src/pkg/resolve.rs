// Purpose: Resolve imports/modules against workspace, lockfile, cache, and VCS sources.
// Inputs/Outputs: Produces resolution context and materialized dependency mappings.
// Invariants: Resolution honors mod mode and offline policy without hidden network side effects.
// Gotchas: Path/module precedence is subtle; keep std/local/external ordering stable.

use anyhow::{Context, bail};
use std::collections::{HashMap, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};
use strsim::jaro_winkler;

use crate::pkg::cache::{CacheLock, cache_root, ensure_dir};
use crate::pkg::lockfile::{LockFile, LockedModule};
use crate::pkg::modfile::{ModFile, Require};
use crate::pkg::vcs;

#[derive(Clone, Copy, Debug)]
pub enum ModMode {
    Readonly,
    Mod,
}

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    pub module: String,
    pub root: PathBuf,
    pub source: String,
    pub requested: String,
    pub rev: Option<String>,
    pub is_local: bool,
}

#[derive(Debug)]
pub struct ResolveCtx {
    pub main_root: PathBuf,
    pub main_module: String,
    pub std_root: PathBuf,
    pub modules: HashMap<String, ResolvedModule>,
    pub lock: LockFile,
}

#[derive(Debug, Clone)]
pub struct Package {
    pub dir: PathBuf,
    pub files: Vec<PathBuf>,
}

fn read_text(p: &Path) -> anyhow::Result<String> {
    fs::read_to_string(p).with_context(|| format!("read {}", p.display()))
}

fn find_project_root(mut p: PathBuf) -> anyhow::Result<PathBuf> {
    loop {
        if p.join("gost.mod").exists() {
            return Ok(p);
        }
        if !p.pop() {
            break;
        }
    }
    bail!("gost.mod not found in parents")
}

fn find_project_root_opt(mut p: PathBuf) -> Option<PathBuf> {
    loop {
        if p.join("gost.mod").exists() {
            return Some(p);
        }
        if !p.pop() {
            break;
        }
    }
    None
}

fn std_root_for(project_root: &Path) -> anyhow::Result<PathBuf> {
    let local = project_root.join("std");
    if local.exists() {
        return Ok(local);
    }
    if let Ok(home) = std::env::var("GOST_HOME") {
        let p = PathBuf::from(home).join("std");
        if p.exists() {
            return Ok(p);
        }
    }
    bail!("std root not found (create ./std or set GOST_HOME)")
}

fn module_source_url(mf: &ModFile, module: &str) -> String {
    if let Some(s) = mf.source.iter().find(|s| s.module == module) {
        return s.url.clone();
    }
    vcs::default_source_url(module)
}

fn module_replace_path(mf: &ModFile, module: &str, main_root: &Path) -> Option<PathBuf> {
    mf.replace
        .iter()
        .find(|r| r.module == module)
        .map(|r| main_root.join(&r.path))
}

pub fn load_ctx(
    cwd: PathBuf,
    mode: ModMode,
    offline: bool,
    want_fetch: bool,
) -> anyhow::Result<ResolveCtx> {
    let main_root = find_project_root(cwd)?;
    let mod_path = main_root.join("gost.mod");
    let lock_path = main_root.join("gost.lock");

    let mf = ModFile::parse(&read_text(&mod_path)?)?;
    let mut lock = if lock_path.exists() {
        serde_json::from_str::<LockFile>(&read_text(&lock_path)?)?
    } else {
        match mode {
            ModMode::Readonly => bail!("gost.lock missing (readonly mode)"),
            ModMode::Mod => LockFile::empty(&mf.module),
        }
    };

    let std_root = std_root_for(&main_root)?;
    let cache = cache_root()?;
    ensure_dir(&cache)?;
    let _guard = CacheLock::acquire(&cache)?;

    let mut q: VecDeque<Require> = VecDeque::new();
    for r in mf.require.iter().cloned() {
        q.push_back(r);
    }

    let mut resolved: HashMap<String, ResolvedModule> = HashMap::new();

    while let Some(req) = q.pop_front() {
        if resolved.contains_key(&req.module) {
            continue;
        }

        if let Some(local_path) = module_replace_path(&mf, &req.module, &main_root) {
            let root = local_path
                .canonicalize()
                .with_context(|| format!("replace path {}", local_path.display()))?;
            resolved.insert(
                req.module.clone(),
                ResolvedModule {
                    module: req.module.clone(),
                    root: root.clone(),
                    source: "local".to_string(),
                    requested: req.version.clone(),
                    rev: None,
                    is_local: true,
                },
            );
            let sub = root.join("gost.mod");
            if sub.exists() {
                let submf = ModFile::parse(&read_text(&sub)?)?;
                for r in submf.require {
                    q.push_back(r);
                }
            }
            continue;
        }

        let source_url = module_source_url(&mf, &req.module);

        if let ModMode::Readonly = mode
            && lock.get(&req.module).is_none()
        {
            bail!("missing {} in gost.lock (readonly mode)", req.module);
        }

        let (mut rev, requested) = if let Some(lm) = lock.get(&req.module) {
            if lm.requested != req.version {
                match mode {
                    ModMode::Readonly => bail!(
                        "gost.mod requires {}@{}, but lock has {}",
                        req.module,
                        req.version,
                        lm.requested
                    ),
                    ModMode::Mod => (String::new(), req.version.clone()),
                }
            } else {
                (lm.rev.clone(), lm.requested.clone())
            }
        } else {
            (String::new(), req.version.clone())
        };

        let allow_fetch = want_fetch && !offline;
        let mirror = vcs::ensure_mirror(&cache, &source_url, allow_fetch)?;
        if rev.is_empty() {
            rev = vcs::resolve_rev(&mirror, &requested)?;
        }

        let root = vcs::checkout_module(&cache, &req.module, &rev, &mirror)?;

        if let ModMode::Mod = mode {
            lock.upsert(LockedModule {
                module: req.module.clone(),
                source: source_url.clone(),
                requested: requested.clone(),
                rev: rev.clone(),
                checksum: None,
                local: None,
            });
        }

        resolved.insert(
            req.module.clone(),
            ResolvedModule {
                module: req.module.clone(),
                root: root.clone(),
                source: source_url,
                requested,
                rev: Some(rev),
                is_local: false,
            },
        );

        let sub = root.join("gost.mod");
        if sub.exists() {
            let submf = ModFile::parse(&read_text(&sub)?)?;
            for r in submf.require {
                q.push_back(r);
            }
        }
    }

    if let ModMode::Mod = mode {
        let txt = serde_json::to_string_pretty(&lock)?;
        fs::write(lock_path, txt)?;
    }

    Ok(ResolveCtx {
        main_root,
        main_module: mf.module,
        std_root,
        modules: resolved,
        lock,
    })
}

// Precondition: `entry` points to a path inside (or near) a potential Gost workspace.
// Postcondition: Returns `Some(ctx)` only when a project root with module files is discoverable.
// Side effects: May read mod/lock/cache files and optionally trigger fetch/checkouts per policy.
pub fn try_load_ctx_from_entry(
    entry: PathBuf,
    mode: ModMode,
    offline: bool,
    want_fetch: bool,
) -> anyhow::Result<Option<ResolveCtx>> {
    let base = entry
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."));
    let root = match find_project_root_opt(base) {
        Some(r) => r,
        None => return Ok(None),
    };
    let ctx = load_ctx(root, mode, offline, want_fetch)?;
    Ok(Some(ctx))
}

pub fn find_module_for_import<'a>(
    ctx: &'a ResolveCtx,
    import: &str,
) -> Option<(&'a str, &'a ResolvedModule)> {
    let mut best: Option<(&str, &ResolvedModule)> = None;
    for (m, rm) in ctx.modules.iter() {
        if (import == m || import.starts_with(&(m.clone() + "/")))
            && best.as_ref().map(|(bm, _)| bm.len()).unwrap_or(0) < m.len()
        {
            best = Some((m.as_str(), rm));
        }
    }
    best
}

fn list_subdirs(dir: &Path) -> anyhow::Result<Vec<String>> {
    let mut out = Vec::new();
    if !dir.exists() {
        return Ok(out);
    }
    for ent in fs::read_dir(dir).with_context(|| format!("read_dir {}", dir.display()))? {
        let ent = ent?;
        let p = ent.path();
        if p.is_dir()
            && let Some(name) = p.file_name().and_then(|s| s.to_str())
        {
            out.push(name.to_string());
        }
    }
    out.sort();
    Ok(out)
}

fn best_name_match<'a>(needle: &str, candidates: &'a [String]) -> Option<&'a str> {
    let mut best: Option<(&str, f64)> = None;
    for c in candidates {
        let score = jaro_winkler(needle, c);
        if best.map(|(_, s)| score > s).unwrap_or(true) {
            best = Some((c.as_str(), score));
        }
    }
    match best {
        Some((name, score)) if score >= 0.84 => Some(name),
        _ => None,
    }
}

fn import_parent_and_leaf(import: &str) -> (String, String) {
    match import.rfind('/') {
        Some(i) => (import[..i].to_string(), import[i + 1..].to_string()),
        None => ("".to_string(), import.to_string()),
    }
}

fn help_did_you_mean(suggestion: &str) -> String {
    format!("help: did you mean \"{}\"?", suggestion)
}

fn collect_gs_files(dir: &Path) -> anyhow::Result<Vec<PathBuf>> {
    let mut v = vec![];
    if !dir.exists() {
        return Ok(v);
    }
    for ent in fs::read_dir(dir).with_context(|| format!("read_dir {}", dir.display()))? {
        let ent = ent?;
        let p = ent.path();
        if p.extension().and_then(|s| s.to_str()) == Some("gs") {
            v.push(p);
        }
    }
    v.sort();
    Ok(v)
}

fn err_import_not_found(import: &str, dir: &Path, base_list_dir: &Path) -> anyhow::Error {
    let (_parent_imp, leaf) = import_parent_and_leaf(import);
    let candidates = list_subdirs(base_list_dir).unwrap_or_default();
    if let Some(best) = best_name_match(&leaf, &candidates) {
        let suggested = match import.rfind('/') {
            Some(i) => format!("{}/{}", &import[..i], best),
            None => best.to_string(),
        };
        return anyhow::anyhow!(
            "import {}: directory not found: {}\n{}",
            import,
            dir.display(),
            help_did_you_mean(&suggested)
        );
    }
    anyhow::anyhow!("import {}: directory not found: {}", import, dir.display())
}

fn err_import_empty(import: &str, dir: &Path, base_list_dir: &Path) -> anyhow::Error {
    let (_parent_imp, leaf) = import_parent_and_leaf(import);
    let candidates = list_subdirs(base_list_dir).unwrap_or_default();
    if let Some(best) = best_name_match(&leaf, &candidates) {
        let suggested = match import.rfind('/') {
            Some(i) => format!("{}/{}", &import[..i], best),
            None => best.to_string(),
        };
        return anyhow::anyhow!(
            "import {}: no .gs files found in {}\n{}",
            import,
            dir.display(),
            help_did_you_mean(&suggested)
        );
    }
    anyhow::anyhow!("import {}: no .gs files found in {}", import, dir.display())
}

pub fn resolve_import_to_package(ctx: &ResolveCtx, import: &str) -> anyhow::Result<Package> {
    if import.starts_with("std/") {
        let rel = import.strip_prefix("std/").unwrap();
        let dir = ctx.std_root.join(rel);
        if !dir.exists() {
            let parent = dir.parent().unwrap_or(&ctx.std_root);
            return Err(err_import_not_found(import, &dir, parent));
        }
        let files = collect_gs_files(&dir)?;
        if files.is_empty() {
            let parent = dir.parent().unwrap_or(&ctx.std_root);
            return Err(err_import_empty(import, &dir, parent));
        }
        return Ok(Package { dir, files });
    }

    if import == ctx.main_module || import.starts_with(&(ctx.main_module.clone() + "/")) {
        let rel = import
            .strip_prefix(&(ctx.main_module.clone() + "/"))
            .unwrap_or("");
        let dir = ctx.main_root.join(rel);
        if !dir.exists() {
            let parent = dir.parent().unwrap_or(&ctx.main_root);
            return Err(err_import_not_found(import, &dir, parent));
        }
        let files = collect_gs_files(&dir)?;
        if files.is_empty() {
            let parent = dir.parent().unwrap_or(&ctx.main_root);
            return Err(err_import_empty(import, &dir, parent));
        }
        return Ok(Package { dir, files });
    }

    let (m, rm) = find_module_for_import(ctx, import)
        .ok_or_else(|| anyhow::anyhow!("unknown module for import {}", import))?;

    let rel = import.strip_prefix(m).unwrap();
    let rel = rel.strip_prefix('/').unwrap_or(rel);
    let dir = rm.root.join(rel);
    if !dir.exists() {
        let parent = dir.parent().unwrap_or(&rm.root);
        return Err(err_import_not_found(import, &dir, parent));
    }
    let files = collect_gs_files(&dir)?;
    if files.is_empty() {
        let parent = dir.parent().unwrap_or(&rm.root);
        return Err(err_import_empty(import, &dir, parent));
    }
    Ok(Package { dir, files })
}
