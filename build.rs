use std::env;
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-env-changed=GOST_GIT_COMMIT");

    if let Ok(v) = env::var("GOST_GIT_COMMIT") {
        let v = v.trim();
        if !v.is_empty() {
            println!("cargo:rustc-env=GOST_GIT_COMMIT={v}");
            return;
        }
    }

    println!("cargo:rerun-if-changed=.git/HEAD");
    println!("cargo:rerun-if-changed=.git/index");

    let Some(commit) = git_commit() else {
        return;
    };
    let mut commit_tag = commit;
    if git_dirty().unwrap_or(false) {
        commit_tag.push_str("-dirty");
    }
    println!("cargo:rustc-env=GOST_GIT_COMMIT={commit_tag}");
}

fn git_commit() -> Option<String> {
    let out = Command::new("git")
        .args(["rev-parse", "--short=12", "HEAD"])
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    let s = String::from_utf8_lossy(&out.stdout).trim().to_string();
    if s.is_empty() { None } else { Some(s) }
}

fn git_dirty() -> Option<bool> {
    let wt = Command::new("git")
        .args(["diff", "--no-ext-diff", "--quiet", "--exit-code"])
        .status()
        .ok()?;
    if !wt.success() {
        return Some(true);
    }
    let idx = Command::new("git")
        .args([
            "diff",
            "--cached",
            "--no-ext-diff",
            "--quiet",
            "--exit-code",
        ])
        .status()
        .ok()?;
    Some(!idx.success())
}
