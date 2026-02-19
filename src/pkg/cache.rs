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
