use anyhow::{bail, Context};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::pkg::cache::{ensure_dir, escape_module, url_hash};

fn run_git(args: &[&str], cwd: Option<&Path>) -> anyhow::Result<String> {
    let mut cmd = Command::new("git");
    cmd.args(args);
    if let Some(c) = cwd {
        cmd.current_dir(c);
    }
    let out = cmd.output().context("failed to execute git")?;
    if !out.status.success() {
        let stderr = String::from_utf8_lossy(&out.stderr);
        bail!("git {:?} failed: {}", args, stderr);
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

pub fn default_source_url(module: &str) -> String {
    format!("https://{}.git", module)
}

pub fn ensure_mirror(
    cache_root: &Path,
    source_url: &str,
    allow_fetch: bool,
) -> anyhow::Result<PathBuf> {
    let vcs_dir = cache_root.join("vcs");
    ensure_dir(&vcs_dir)?;
    let mirror = vcs_dir.join(url_hash(source_url));
    if mirror.exists() {
        if allow_fetch {
            let _ = run_git(
                &[
                    "-C",
                    mirror.to_str().unwrap(),
                    "fetch",
                    "--prune",
                    "--tags",
                ],
                None,
            )?;
        }
        return Ok(mirror);
    }
    if !allow_fetch {
        bail!("missing mirror for {} (readonly/offline)", source_url);
    }
    let _ = run_git(&["clone", "--mirror", source_url, mirror.to_str().unwrap()], None)?;
    Ok(mirror)
}

fn looks_like_sha(s: &str) -> bool {
    let n = s.len();
    (7..=40).contains(&n) && s.bytes().all(|c| c.is_ascii_hexdigit())
}

pub fn resolve_rev(mirror: &Path, requested: &str) -> anyhow::Result<String> {
    if looks_like_sha(requested) {
        let full = run_git(
            &[
                "-C",
                mirror.to_str().unwrap(),
                "rev-parse",
                &format!("{}^{{commit}}", requested),
            ],
            None,
        )?;
        return Ok(full);
    }
    if requested.starts_with('v') {
        if let Ok(full) = run_git(
            &[
                "-C",
                mirror.to_str().unwrap(),
                "rev-parse",
                &format!("refs/tags/{}^{{commit}}", requested),
            ],
            None,
        ) {
            return Ok(full);
        }
    }
    let full = run_git(
        &[
            "-C",
            mirror.to_str().unwrap(),
            "rev-parse",
            &format!("refs/remotes/origin/{}^{{commit}}", requested),
        ],
        None,
    )?;
    Ok(full)
}

pub fn checkout_module(
    cache_root: &Path,
    module: &str,
    rev: &str,
    mirror: &Path,
) -> anyhow::Result<PathBuf> {
    let mod_dir = cache_root.join("mod");
    ensure_dir(&mod_dir)?;
    let dst = mod_dir.join(format!(
        "{}@{}",
        escape_module(module),
        &rev[..12.min(rev.len())]
    ));

    if dst.exists() {
        let cur = run_git(
            &["-C", dst.to_str().unwrap(), "rev-parse", "HEAD"],
            None,
        )
        .unwrap_or_default();
        if cur == rev {
            return Ok(dst);
        }
        fs::remove_dir_all(&dst).ok();
    }

    let _ = run_git(&["clone", mirror.to_str().unwrap(), dst.to_str().unwrap()], None)?;
    let _ = run_git(
        &["-C", dst.to_str().unwrap(), "checkout", "--force", rev],
        None,
    )?;
    Ok(dst)
}
