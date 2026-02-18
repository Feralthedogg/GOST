use anyhow::{Context, bail};
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::pkg::cache::{cache_root, escape_module, url_hash};
use crate::pkg::import_scan;
use crate::pkg::lockfile::LockFile;
use crate::pkg::modfile::{ModFile, Require};
use crate::pkg::resolve::{self, ModMode};

fn read_text(p: &Path) -> anyhow::Result<String> {
    fs::read_to_string(p).with_context(|| format!("read {}", p.display()))
}

fn write_text(p: &Path, s: &str) -> anyhow::Result<()> {
    fs::write(p, s).with_context(|| format!("write {}", p.display()))?;
    Ok(())
}

pub fn find_mod_root(mut p: PathBuf) -> Option<PathBuf> {
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

fn git(cmd_args: &[&str], cwd: &Path) -> anyhow::Result<String> {
    let out = Command::new("git")
        .args(cmd_args)
        .current_dir(cwd)
        .output()
        .context("failed to execute git")?;
    if !out.status.success() {
        bail!(
            "git {:?} failed: {}",
            cmd_args,
            String::from_utf8_lossy(&out.stderr)
        );
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

fn guess_module_from_git(cwd: &Path) -> Option<String> {
    let top = git(&["rev-parse", "--show-toplevel"], cwd).ok()?;
    let top = PathBuf::from(top);
    let url = git(&["remote", "get-url", "origin"], &top).ok()?;

    let mut s = url.trim().to_string();
    if let Some(rest) = s.strip_prefix("ssh://") {
        s = rest.to_string();
    }
    if let Some(rest) = s.strip_prefix("https://") {
        s = rest.to_string();
    }
    if let Some(rest) = s.strip_prefix("http://") {
        s = rest.to_string();
    }

    if let Some(idx) = s.find('@')
        && s[..idx].contains("git")
    {
        s = s[idx + 1..].to_string();
    }

    if let Some(idx) = s.find(':') {
        let host = &s[..idx];
        let path = &s[idx + 1..];
        s = format!("{}/{}", host, path);
    }

    if let Some(rest) = s.strip_suffix(".git") {
        s = rest.to_string();
    }

    let seg: Vec<&str> = s.split('/').filter(|x| !x.is_empty()).collect();
    if seg.len() >= 3 {
        Some(format!("{}/{}/{}", seg[0], seg[1], seg[2]))
    } else {
        None
    }
}

pub fn cmd_init(cwd: PathBuf, module_opt: Option<String>) -> anyhow::Result<()> {
    let root = find_mod_root(cwd.clone()).unwrap_or(cwd);
    let mod_path = root.join("gost.mod");
    if mod_path.exists() {
        bail!("gost.mod already exists at {}", mod_path.display());
    }

    let module = module_opt
        .or_else(|| guess_module_from_git(&root))
        .unwrap_or_else(|| "example.com/you/project".to_string());

    let mut mf = ModFile {
        module,
        require: vec![],
        replace: vec![],
        source: vec![],
    };
    mf.sort_deterministic();
    write_text(&mod_path, &mf.to_pretty_toml())?;

    let lock_path = root.join("gost.lock");
    if !lock_path.exists() {
        let lf = LockFile::empty(&mf.module);
        write_text(&lock_path, &serde_json::to_string_pretty(&lf)?)?;
    }

    eprintln!("initialized gost.mod at {}", mod_path.display());
    Ok(())
}

fn collect_gs_files_recursive(root: &Path) -> anyhow::Result<Vec<PathBuf>> {
    fn is_skip_dir(name: &str) -> bool {
        matches!(
            name,
            ".git" | "target" | "node_modules" | ".gost" | ".idea" | ".vscode"
        )
    }

    fn walk(dir: &Path, out: &mut Vec<PathBuf>) -> anyhow::Result<()> {
        for ent in fs::read_dir(dir)? {
            let ent = ent?;
            let p = ent.path();
            if p.is_dir() {
                if let Some(name) = p.file_name().and_then(|s| s.to_str())
                    && is_skip_dir(name)
                {
                    continue;
                }
                walk(&p, out)?;
            } else if p.extension().and_then(|s| s.to_str()) == Some("gs") {
                out.push(p);
            }
        }
        Ok(())
    }

    let mut out = vec![];
    walk(root, &mut out)?;
    out.sort();
    Ok(out)
}

fn infer_module_root(import: &str) -> Option<String> {
    if import.starts_with("std/") {
        return None;
    }
    if import.starts_with("./") || import.starts_with("../") {
        return None;
    }
    if import.contains(':') {
        return None;
    }
    let seg: Vec<&str> = import.split('/').filter(|x| !x.is_empty()).collect();
    if seg.len() >= 3 && seg[0].contains('.') {
        return Some(format!("{}/{}/{}", seg[0], seg[1], seg[2]));
    }
    None
}

pub fn cmd_tidy(cwd: PathBuf, mode: ModMode, offline: bool) -> anyhow::Result<()> {
    let root = find_mod_root(cwd).context("gost.mod not found (run `gs mod init` first)")?;
    let mod_path = root.join("gost.mod");
    let lock_path = root.join("gost.lock");

    let mut mf = ModFile::parse(&read_text(&mod_path)?)?;
    let lock: Option<LockFile> = if lock_path.exists() {
        Some(serde_json::from_str::<LockFile>(&read_text(&lock_path)?)?)
    } else {
        None
    };

    let used_roots = compute_required_modules(&root, &mf)?;

    let mut req_map: BTreeMap<String, String> = BTreeMap::new();
    for r in &mf.require {
        req_map.insert(r.module.clone(), r.version.clone());
    }

    for m in &used_roots {
        if req_map.contains_key(m) {
            continue;
        }
        if let ModMode::Readonly = mode {
            bail!("missing require for module {}", m);
        }
        let ver = lock
            .as_ref()
            .and_then(|l| l.get(m).map(|x| x.requested.clone()))
            .unwrap_or_else(|| "main".to_string());
        req_map.insert(m.clone(), ver);
    }

    let replace_set: BTreeSet<String> = mf.replace.iter().map(|r| r.module.clone()).collect();
    if let ModMode::Readonly = mode {
        for m in req_map.keys() {
            if !used_roots.contains(m) && !replace_set.contains(m) {
                eprintln!("warning: unused require {}", m);
            }
        }
    } else {
        req_map.retain(|m, _| used_roots.contains(m) || replace_set.contains(m));
    }

    mf.require = req_map
        .into_iter()
        .map(|(module, version)| Require { module, version })
        .collect();
    mf.sort_deterministic();
    if let ModMode::Readonly = mode {
        verify_lock(&mf, lock.as_ref())?;
        eprintln!("tidy verify OK");
        return Ok(());
    }
    write_text(&mod_path, &mf.to_pretty_toml())?;
    eprintln!("tidy wrote {}", mod_path.display());

    if let ModMode::Mod = mode {
        let _ = resolve::load_ctx(root.clone(), ModMode::Mod, offline, true)?;
        eprintln!("updated gost.lock");
    }
    Ok(())
}

pub fn cmd_download(cwd: PathBuf, offline: bool, online: bool) -> anyhow::Result<()> {
    let root = find_mod_root(cwd).context("gost.mod not found (run `gs mod init` first)")?;
    let want_fetch = online;
    let offline = offline || !online;
    let _ = resolve::load_ctx(root, ModMode::Readonly, offline, want_fetch)?;
    eprintln!("download OK");
    Ok(())
}

pub fn cmd_graph(cwd: PathBuf, _offline: bool) -> anyhow::Result<()> {
    let root = find_mod_root(cwd).context("gost.mod not found (run `gs mod init` first)")?;
    let mod_path = root.join("gost.mod");
    let lock_path = root.join("gost.lock");
    let mf = ModFile::parse(&read_text(&mod_path)?)?;
    let lock: Option<LockFile> = if lock_path.exists() {
        Some(serde_json::from_str::<LockFile>(&read_text(&lock_path)?)?)
    } else {
        None
    };

    let cache = cache_root().ok();

    let mut direct: BTreeSet<String> = BTreeSet::new();
    for r in &mf.require {
        direct.insert(r.module.clone());
    }

    eprintln!("main: {}", mf.module);
    eprintln!("direct:");
    if mf.require.is_empty() {
        eprintln!("  (none)");
    }
    for r in &mf.require {
        let mut line = format!("  {} @ {}", r.module, r.version);
        if let Some(l) = lock.as_ref().and_then(|l| l.get(&r.module)) {
            line.push_str(&format!(" -> {}", &l.rev[..12.min(l.rev.len())]));
            if let Some(local) = &l.local {
                line.push_str(&format!(" (local:{})", local));
            } else {
                line.push_str(&format!(" ({})", l.source));
            }
            if let Some(croot) = cache.as_ref() {
                let mirror = if l.local.is_some() {
                    false
                } else {
                    croot.join("vcs").join(url_hash(&l.source)).exists()
                };
                let mod_dir = if l.local.is_some() {
                    false
                } else {
                    let rev = &l.rev;
                    let rev = &rev[..12.min(rev.len())];
                    croot
                        .join("mod")
                        .join(format!("{}@{}", escape_module(&l.module), rev))
                        .exists()
                };
                line.push_str(&format!(
                    " [cache: mirror={}, mod={}]",
                    if mirror { "yes" } else { "no" },
                    if mod_dir { "yes" } else { "no" }
                ));
            }
        } else if lock.is_some() {
            line.push_str(" (missing in gost.lock)");
        }
        eprintln!("{}", line);
    }

    if let Some(lock) = lock.as_ref() {
        let mut trans: Vec<&str> = lock
            .modules
            .iter()
            .map(|m| m.module.as_str())
            .filter(|m| !direct.contains(*m))
            .collect();
        trans.sort();
        if !trans.is_empty() {
            eprintln!("transitive:");
            for m in trans {
                let l = lock.get(m).unwrap();
                let mut line = format!("  {} @ {}", l.module, l.requested);
                line.push_str(&format!(" -> {}", &l.rev[..12.min(l.rev.len())]));
                if let Some(local) = &l.local {
                    line.push_str(&format!(" (local:{})", local));
                } else {
                    line.push_str(&format!(" ({})", l.source));
                }
                if let Some(croot) = cache.as_ref() {
                    let mirror = if l.local.is_some() {
                        false
                    } else {
                        croot.join("vcs").join(url_hash(&l.source)).exists()
                    };
                    let rev = &l.rev[..12.min(l.rev.len())];
                    let mod_dir = if l.local.is_some() {
                        false
                    } else {
                        croot
                            .join("mod")
                            .join(format!("{}@{}", escape_module(&l.module), rev))
                            .exists()
                    };
                    line.push_str(&format!(
                        " [cache: mirror={}, mod={}]",
                        if mirror { "yes" } else { "no" },
                        if mod_dir { "yes" } else { "no" }
                    ));
                }
                eprintln!("{}", line);
            }
        } else {
            eprintln!("transitive:");
            eprintln!("  (none)");
        }
    }
    Ok(())
}

pub fn cmd_verify(cwd: PathBuf, online: bool, _offline: bool) -> anyhow::Result<()> {
    // 1) pure readonly verification (no network)
    cmd_tidy(cwd.clone(), ModMode::Readonly, true)?;

    // 2) resolve check (cache-only or online fetch)
    let root = find_mod_root(cwd).context("gost.mod not found (run `gs mod init` first)")?;
    let offline = !online;
    let _ = resolve::load_ctx(root, ModMode::Readonly, offline, online)?;
    eprintln!("verify OK");
    Ok(())
}

fn compute_required_modules(root: &Path, mf: &ModFile) -> anyhow::Result<BTreeSet<String>> {
    let gs_files = collect_gs_files_recursive(root)?;
    if gs_files.is_empty() {
        bail!("no .gs files found under {}", root.display());
    }

    let mut all_imports: HashSet<String> = HashSet::new();
    for f in &gs_files {
        for im in import_scan::scan_imports_in_file(f)? {
            all_imports.insert(im);
        }
    }

    let mut used_roots: BTreeSet<String> = BTreeSet::new();
    for im in &all_imports {
        if im == &mf.module || im.starts_with(&(mf.module.clone() + "/")) {
            continue;
        }
        if let Some(r) = infer_module_root(im) {
            used_roots.insert(r);
        }
    }
    Ok(used_roots)
}

fn verify_lock(mf: &ModFile, lock: Option<&LockFile>) -> anyhow::Result<()> {
    let lock = lock.context("gost.lock missing (readonly mode)")?;
    if lock.main.module != mf.module {
        bail!(
            "gost.lock main mismatch: mod has {}, lock has {}",
            mf.module,
            lock.main.module
        );
    }
    for r in &mf.require {
        let lm = lock
            .get(&r.module)
            .with_context(|| format!("missing {} in gost.lock", r.module))?;
        if lm.requested != r.version {
            bail!(
                "gost.lock mismatch for {}: mod requires {}, lock has {}",
                r.module,
                r.version,
                lm.requested
            );
        }
    }
    Ok(())
}
