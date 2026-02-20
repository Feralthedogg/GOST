// Purpose: Implement module cache read/write primitives for resolved dependencies.
// Inputs/Outputs: Persists and loads cached module metadata/artifacts on disk.
// Invariants: Cache format and locking behavior must prevent partial-write corruption.
// Gotchas: File-open flags and truncation policy are critical for Windows compatibility.

use anyhow::Context;
use directories::ProjectDirs;
use fs2::FileExt;
use sha2::{Digest, Sha256};
use std::fs;
use std::fs::{File, OpenOptions};
use std::io::Read;
use std::path::{Path, PathBuf};

pub fn cache_root() -> anyhow::Result<PathBuf> {
    if let Ok(p) = std::env::var("GOST_CACHE_DIR") {
        return Ok(PathBuf::from(p));
    }
    let pd =
        ProjectDirs::from("dev", "gost", "gost").context("cannot determine OS cache directory")?;
    Ok(pd.cache_dir().to_path_buf())
}

pub fn ensure_dir(p: &Path) -> anyhow::Result<()> {
    fs::create_dir_all(p)?;
    Ok(())
}

pub fn url_hash(url: &str) -> String {
    let mut h = Sha256::new();
    h.update(url.as_bytes());
    hex::encode(h.finalize())
}

pub fn escape_module(m: &str) -> String {
    m.replace(['/', '\\'], "!")
}

fn collect_files(base: &Path, dir: &Path, out: &mut Vec<PathBuf>) -> anyhow::Result<()> {
    for ent in fs::read_dir(dir).with_context(|| format!("read_dir {}", dir.display()))? {
        let ent = ent?;
        let p = ent.path();
        if p.is_dir() {
            if p.file_name().and_then(|s| s.to_str()) == Some(".git") {
                continue;
            }
            collect_files(base, &p, out)?;
            continue;
        }
        if p.is_file() {
            let rel = p
                .strip_prefix(base)
                .with_context(|| format!("strip_prefix {}", p.display()))?
                .to_path_buf();
            out.push(rel);
        }
    }
    Ok(())
}

pub fn dir_checksum_sha256(dir: &Path) -> anyhow::Result<String> {
    let mut files = Vec::<PathBuf>::new();
    collect_files(dir, dir, &mut files)?;
    files.sort_by_key(|p| p.to_string_lossy().replace('\\', "/"));

    let mut hasher = Sha256::new();
    for rel in files {
        let rel_norm = rel.to_string_lossy().replace('\\', "/");
        hasher.update(b"F\0");
        hasher.update(rel_norm.as_bytes());
        hasher.update(b"\0");

        let mut f = File::open(dir.join(&rel))
            .with_context(|| format!("open {}", dir.join(&rel).display()))?;
        let mut buf = [0u8; 8192];
        loop {
            let n = f.read(&mut buf)?;
            if n == 0 {
                break;
            }
            hasher.update(&buf[..n]);
        }
    }
    Ok(hex::encode(hasher.finalize()))
}

pub struct CacheLock {
    _file: File,
}

impl CacheLock {
    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn acquire(root: &Path) -> anyhow::Result<Self> {
        ensure_dir(root)?;
        let lock_path = root.join("cache.lock");
        let f = OpenOptions::new()
            .create(true)
            .truncate(false)
            .read(true)
            .write(true)
            .open(lock_path)?;
        f.lock_exclusive()?;
        Ok(Self { _file: f })
    }
}

#[cfg(test)]
mod tests {
    use super::dir_checksum_sha256;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_dir(prefix: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time drift")
            .as_nanos();
        std::env::temp_dir().join(format!("gost-{}-{}-{}", prefix, std::process::id(), nonce))
    }

    #[test]
    fn dir_checksum_is_stable_and_detects_content_change() {
        let root = temp_dir("checksum-stable");
        fs::create_dir_all(root.join("sub")).expect("mkdir");
        fs::write(root.join("a.txt"), "hello").expect("write a");
        fs::write(root.join("sub").join("b.txt"), "world").expect("write b");

        let c1 = dir_checksum_sha256(&root).expect("checksum #1");
        let c2 = dir_checksum_sha256(&root).expect("checksum #2");
        assert_eq!(c1, c2, "checksum should be deterministic for same content");

        fs::write(root.join("sub").join("b.txt"), "WORLD").expect("rewrite b");
        let c3 = dir_checksum_sha256(&root).expect("checksum #3");
        assert_ne!(c1, c3, "checksum must change when file content changes");

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn dir_checksum_ignores_git_directory() {
        let root = temp_dir("checksum-git-ignore");
        fs::create_dir_all(root.join(".git")).expect("mkdir .git");
        fs::write(root.join("mod.gs"), "module m\n").expect("write module");
        fs::write(root.join(".git").join("config"), "first").expect("write config #1");

        let c1 = dir_checksum_sha256(&root).expect("checksum #1");
        fs::write(root.join(".git").join("config"), "second").expect("write config #2");
        let c2 = dir_checksum_sha256(&root).expect("checksum #2");
        assert_eq!(c1, c2, ".git content should not affect module checksum");

        let _ = fs::remove_dir_all(root);
    }
}
