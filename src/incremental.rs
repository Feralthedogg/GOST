// Purpose: Provide incremental build graph hashing and cache primitives.
// Inputs/Outputs: Tracks package/file metadata and loads/stores cached LLVM/object/link artifacts.
// Invariants: Hashes must be deterministic for identical source+settings state.
// Gotchas: Interface hash is intentionally conservative; content hash drives correctness decisions.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::UNIX_EPOCH;

use crate::frontend::ast::{
    AssignOp, BinaryOp, Block, BlockOrExpr, ClosureParam, Expr, ExprKind, FileAst, Item,
    LayoutAttr, Pattern, ReprInt, SelectArm, SelectArmKind, Stmt, TraitMethod, TypeAst,
    TypeAstKind, TypeParam, UnaryOp,
};
use crate::pkg::cache::{cache_root, ensure_dir};
use crate::pkg::resolve::ModMode;

pub const ENTRY_PACKAGE: &str = "__entry__";
const GRAPH_SCHEMA: u32 = 2;
static SIDECAR_HASH_DISABLED: AtomicBool = AtomicBool::new(false);
static INCREMENTAL_CACHE_DISABLED: AtomicBool = AtomicBool::new(false);

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct BuildGraphFile {
    pub path: String,
    pub content_hash: String,
    pub interface_hash: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct BuildGraphPackage {
    pub package: String,
    #[serde(default)]
    pub file_hashes: Vec<BuildGraphFile>,
    pub files: Vec<String>,
    pub imports: Vec<String>,
    pub content_hash: String,
    pub interface_hash: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct BuildGraph {
    pub schema: u32,
    pub entry: String,
    pub mode: String,
    pub offline: bool,
    pub want_fetch: bool,
    pub compiler_fingerprint: String,
    pub packages: Vec<BuildGraphPackage>,
    pub content_graph_hash: String,
    pub interface_graph_hash: String,
    pub mod_hash: Option<String>,
    pub lock_hash: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq, Eq)]
struct CachedWarnings {
    #[serde(default)]
    warnings: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
struct LatestGraphIndex {
    schema: u32,
    entry: String,
    content_graph_hash: String,
}

impl BuildGraph {
    pub fn package_codegen_levels(&self) -> Vec<Vec<String>> {
        if self.packages.is_empty() {
            return Vec::new();
        }
        let known = self
            .packages
            .iter()
            .map(|p| p.package.clone())
            .collect::<BTreeSet<_>>();
        let mut indegree = BTreeMap::<String, usize>::new();
        let mut outgoing = BTreeMap::<String, BTreeSet<String>>::new();
        for pkg in &self.packages {
            indegree.entry(pkg.package.clone()).or_insert(0);
            for dep in pkg.imports.iter().filter(|dep| known.contains(*dep)) {
                indegree
                    .entry(pkg.package.clone())
                    .and_modify(|v| *v += 1)
                    .or_insert(1);
                outgoing
                    .entry(dep.clone())
                    .or_default()
                    .insert(pkg.package.clone());
            }
        }

        let mut ready = indegree
            .iter()
            .filter_map(|(pkg, deg)| if *deg == 0 { Some(pkg.clone()) } else { None })
            .collect::<Vec<_>>();
        ready.sort();

        let mut levels = Vec::<Vec<String>>::new();
        let mut processed = BTreeSet::<String>::new();
        while !ready.is_empty() {
            levels.push(ready.clone());
            let mut next = BTreeSet::<String>::new();
            for done in ready {
                if !processed.insert(done.clone()) {
                    continue;
                }
                if let Some(children) = outgoing.get(&done) {
                    for child in children {
                        if let Some(deg) = indegree.get_mut(child)
                            && *deg > 0
                        {
                            *deg -= 1;
                            if *deg == 0 {
                                next.insert(child.clone());
                            }
                        }
                    }
                }
            }
            ready = next.into_iter().collect();
        }

        if processed.len() != known.len() {
            // Preserve progress even if graph contains cycles.
            let mut unresolved = known
                .iter()
                .filter(|pkg| !processed.contains(*pkg))
                .cloned()
                .collect::<Vec<_>>();
            unresolved.sort();
            for pkg in unresolved {
                levels.push(vec![pkg]);
            }
        }
        levels
    }

    pub fn max_parallel_codegen_width(&self) -> usize {
        self.package_codegen_levels()
            .iter()
            .map(std::vec::Vec::len)
            .max()
            .unwrap_or(1)
            .max(1)
    }
}

#[derive(Debug, Clone, Default)]
struct PackageAcc {
    files: BTreeMap<String, FileAcc>,
    imports: BTreeSet<String>,
}

#[derive(Debug, Clone)]
struct FileAcc {
    content_hash: String,
    interface_signature: String,
}

#[derive(Debug)]
pub struct BuildCtx {
    entry_path: PathBuf,
    cache_root: PathBuf,
    mode: ModMode,
    offline: bool,
    want_fetch: bool,
    compiler_fingerprint: String,
    mod_hash: Option<String>,
    lock_hash: Option<String>,
    packages: BTreeMap<String, PackageAcc>,
    cached_graph: Option<BuildGraph>,
}

impl BuildCtx {
    pub fn new(entry_path: &Path, mode: ModMode, offline: bool, want_fetch: bool) -> Self {
        let cache_root = build_cache_root_for_entry(entry_path)
            .or_else(|_| cache_root().map(|p| p.join("build")))
            .unwrap_or_else(|_| std::env::temp_dir().join("gost-cache").join("build"));
        let _ = ensure_dir(&cache_root);
        let (mod_hash, lock_hash) = project_mod_lock_hashes(entry_path);
        Self {
            entry_path: entry_path.to_path_buf(),
            cache_root,
            mode,
            offline,
            want_fetch,
            compiler_fingerprint: compiler_fingerprint(),
            mod_hash,
            lock_hash,
            packages: BTreeMap::new(),
            cached_graph: None,
        }
    }

    pub fn record_source_file(&mut self, package: &str, path: &Path, source: &str, file: &FileAst) {
        self.record_source_file_with_hash(package, path, source, file, None)
    }

    pub fn record_source_file_with_hash(
        &mut self,
        package: &str,
        path: &Path,
        source: &str,
        file: &FileAst,
        interface_hash: Option<&str>,
    ) {
        let pkg = self.packages.entry(package.to_string()).or_default();
        let path_key = normalize_path(path);
        let content_hash = hash_bytes_sha256(source.as_bytes());
        let canary_enabled = sidecar_canary_enabled();
        let mut interface_signature = if let Some(hash) = interface_hash {
            if !hash.is_empty() && !SIDECAR_HASH_DISABLED.load(Ordering::Relaxed) {
                hash.to_string()
            } else {
                file_interface_signature(file)
            }
        } else {
            file_interface_signature(file)
        };
        if canary_enabled
            && let Some(sidecar_hash) = interface_hash
            && !sidecar_hash.is_empty()
            && !SIDECAR_HASH_DISABLED.load(Ordering::Relaxed)
        {
            let legacy = file_interface_signature(file);
            if sidecar_hash != legacy {
                trace(&format!(
                    "sidecar hash mismatch at {} (sidecar={}, legacy={}); disabling incremental cache for process",
                    path.display(),
                    sidecar_hash,
                    legacy
                ));
                SIDECAR_HASH_DISABLED.store(true, Ordering::Relaxed);
                INCREMENTAL_CACHE_DISABLED.store(true, Ordering::Relaxed);
                interface_signature = legacy;
            }
        }
        pkg.files.insert(
            path_key,
            FileAcc {
                content_hash,
                interface_signature,
            },
        );
        for import in &file.imports {
            pkg.imports.insert(import.path.clone());
        }
        self.cached_graph = None;
    }

    pub fn graph(&mut self) -> BuildGraph {
        if let Some(g) = &self.cached_graph {
            return g.clone();
        }
        // Resolver in `-mod=mod` can write gost.lock during the same compile.
        // Re-snapshot these hashes right before graph materialization so cache keys
        // reflect the post-resolve project state.
        let (mod_hash, lock_hash) = project_mod_lock_hashes(&self.entry_path);
        self.mod_hash = mod_hash;
        self.lock_hash = lock_hash;
        let mut packages = Vec::<BuildGraphPackage>::new();
        for (pkg_name, pkg) in &self.packages {
            let mut content_hasher = Sha256::new();
            let mut interface_hasher = Sha256::new();
            let mut files = Vec::<String>::new();
            let mut file_hashes = Vec::<BuildGraphFile>::new();
            for (path, file) in &pkg.files {
                files.push(path.clone());
                file_hashes.push(BuildGraphFile {
                    path: path.clone(),
                    content_hash: file.content_hash.clone(),
                    interface_hash: hash_bytes_sha256(file.interface_signature.as_bytes()),
                });
                content_hasher.update(path.as_bytes());
                content_hasher.update(b"\0");
                content_hasher.update(file.content_hash.as_bytes());
                content_hasher.update(b"\0");
                interface_hasher.update(path.as_bytes());
                interface_hasher.update(b"\0");
                interface_hasher.update(file.interface_signature.as_bytes());
                interface_hasher.update(b"\0");
            }
            let imports = pkg.imports.iter().cloned().collect::<Vec<_>>();
            packages.push(BuildGraphPackage {
                package: pkg_name.clone(),
                file_hashes,
                files,
                imports,
                content_hash: hex::encode(content_hasher.finalize()),
                interface_hash: hex::encode(interface_hasher.finalize()),
            });
        }
        packages.sort_by(|a, b| a.package.cmp(&b.package));

        let mut content_graph = Sha256::new();
        content_graph.update(b"gost-inc-content-v1");
        content_graph.update(self.compiler_fingerprint.as_bytes());
        content_graph.update(mode_label(self.mode).as_bytes());
        content_graph.update(if self.offline { b"1" } else { b"0" });
        content_graph.update(if self.want_fetch { b"1" } else { b"0" });
        if let Some(h) = &self.mod_hash {
            content_graph.update(b"mod:");
            content_graph.update(h.as_bytes());
        }
        if let Some(h) = &self.lock_hash {
            content_graph.update(b"lock:");
            content_graph.update(h.as_bytes());
        }
        for pkg in &packages {
            content_graph.update(pkg.package.as_bytes());
            content_graph.update(b"\0");
            content_graph.update(pkg.content_hash.as_bytes());
            content_graph.update(b"\0");
            for imp in &pkg.imports {
                content_graph.update(imp.as_bytes());
                content_graph.update(b"\0");
            }
        }

        let mut iface_graph = Sha256::new();
        iface_graph.update(b"gost-inc-interface-v1");
        iface_graph.update(self.compiler_fingerprint.as_bytes());
        for pkg in &packages {
            iface_graph.update(pkg.package.as_bytes());
            iface_graph.update(b"\0");
            iface_graph.update(pkg.interface_hash.as_bytes());
            iface_graph.update(b"\0");
            for imp in &pkg.imports {
                iface_graph.update(imp.as_bytes());
                iface_graph.update(b"\0");
            }
        }

        let graph = BuildGraph {
            schema: GRAPH_SCHEMA,
            entry: normalize_path(&self.entry_path),
            mode: mode_label(self.mode).to_string(),
            offline: self.offline,
            want_fetch: self.want_fetch,
            compiler_fingerprint: self.compiler_fingerprint.clone(),
            packages,
            content_graph_hash: hex::encode(content_graph.finalize()),
            interface_graph_hash: hex::encode(iface_graph.finalize()),
            mod_hash: self.mod_hash.clone(),
            lock_hash: self.lock_hash.clone(),
        };
        self.cached_graph = Some(graph.clone());
        graph
    }

    pub fn try_load_cached_llvm(&mut self) -> Result<Option<String>> {
        if INCREMENTAL_CACHE_DISABLED.load(Ordering::Relaxed) {
            return Ok(None);
        }
        let cache_dir = self.llvm_cache_dir()?;
        let ll_path = cache_dir.join("module.ll");
        if !ll_path.exists() {
            return Ok(None);
        }
        trace(&format!(
            "llvm cache hit: {}",
            cache_dir.join("module.ll").display()
        ));
        let text =
            fs::read_to_string(&ll_path).with_context(|| format!("read {}", ll_path.display()))?;
        Ok(Some(text))
    }

    pub fn try_load_cached_warnings(&mut self) -> Result<Vec<String>> {
        if INCREMENTAL_CACHE_DISABLED.load(Ordering::Relaxed) {
            return Ok(Vec::new());
        }
        let cache_dir = self.llvm_cache_dir()?;
        read_cached_warnings(&cache_dir)
    }

    pub fn store_cached_llvm(&mut self, llvm: &str) -> Result<()> {
        self.store_cached_llvm_with_warnings(llvm, &[])
    }

    pub fn store_cached_llvm_with_warnings(
        &mut self,
        llvm: &str,
        warnings: &[String],
    ) -> Result<()> {
        if INCREMENTAL_CACHE_DISABLED.load(Ordering::Relaxed) {
            return Ok(());
        }
        let cache_dir = self.llvm_cache_dir()?;
        let ll_path = cache_dir.join("module.ll");
        let graph_path = cache_dir.join("graph.json");
        fs::write(&ll_path, llvm).with_context(|| format!("write {}", ll_path.display()))?;
        let graph = self.graph();
        fs::write(&graph_path, serde_json::to_string_pretty(&graph)?)
            .with_context(|| format!("write {}", graph_path.display()))?;
        write_cached_warnings(&cache_dir, warnings)?;
        write_latest_graph_index(&self.cache_root, &self.entry_path, &graph)?;
        trace(&format!("llvm cache store: {}", ll_path.display()));
        Ok(())
    }

    fn llvm_cache_dir(&mut self) -> Result<PathBuf> {
        let key = self.graph().content_graph_hash;
        let dir = self.cache_root.join("llvm").join(key);
        ensure_dir(&dir)?;
        Ok(dir)
    }
}

pub fn build_cache_root_for_entry(entry_path: &Path) -> Result<PathBuf> {
    let root = if let Some(project_root) = find_project_root(entry_path) {
        project_root.join(".gost").join("cache").join("build")
    } else {
        cache_root()?.join("build")
    };
    ensure_dir(&root)?;
    Ok(root)
}

pub fn ensure_cache_namespace(entry_path: &Path, namespace: &str) -> Result<PathBuf> {
    let root = build_cache_root_for_entry(entry_path)?;
    let dir = root.join(namespace);
    ensure_dir(&dir)?;
    Ok(dir)
}

pub fn hash_bytes_sha256(bytes: &[u8]) -> String {
    let mut h = Sha256::new();
    h.update(bytes);
    hex::encode(h.finalize())
}

pub fn hash_file_sha256(path: &Path) -> Result<String> {
    let bytes = fs::read(path).with_context(|| format!("read {}", path.display()))?;
    Ok(hash_bytes_sha256(&bytes))
}

pub(crate) fn sidecar_interface_hash(file: &FileAst) -> [u8; 32] {
    hex_to_array32(&file_interface_signature(file))
}

pub(crate) fn sidecar_public_item_hashes(file: &FileAst) -> Vec<[u8; 32]> {
    let mut parts = Vec::<String>::new();
    for item in &file.items {
        if let Some(sig) = public_item_signature(item) {
            parts.push(sig);
        }
    }
    parts.sort();
    parts
        .into_iter()
        .map(|sig| hex_to_array32(&hash_bytes_sha256(sig.as_bytes())))
        .collect()
}

pub fn try_load_cached_llvm_fast(
    entry_path: &Path,
    mode: ModMode,
    offline: bool,
    want_fetch: bool,
) -> Result<Option<String>> {
    Ok(
        try_load_cached_llvm_fast_with_warnings(entry_path, mode, offline, want_fetch)?
            .map(|(llvm, _warnings)| llvm),
    )
}

pub fn try_load_cached_llvm_fast_with_warnings(
    entry_path: &Path,
    mode: ModMode,
    offline: bool,
    want_fetch: bool,
) -> Result<Option<(String, Vec<String>)>> {
    if INCREMENTAL_CACHE_DISABLED.load(Ordering::Relaxed) {
        return Ok(None);
    }
    let cache_root = build_cache_root_for_entry(entry_path)
        .or_else(|_| cache_root().map(|p| p.join("build")))
        .unwrap_or_else(|_| std::env::temp_dir().join("gost-cache").join("build"));

    let latest_path = latest_graph_index_path(&cache_root, entry_path);
    if !latest_path.exists() {
        return Ok(None);
    }
    let latest_text = match fs::read_to_string(&latest_path) {
        Ok(v) => v,
        Err(err) => {
            trace(&format!(
                "fast cache probe miss: failed to read latest index {}: {}",
                latest_path.display(),
                err
            ));
            return Ok(None);
        }
    };
    let latest: LatestGraphIndex = match serde_json::from_str(&latest_text) {
        Ok(v) => v,
        Err(err) => {
            trace(&format!(
                "fast cache probe miss: failed to decode latest index {}: {}",
                latest_path.display(),
                err
            ));
            return Ok(None);
        }
    };
    if latest.schema != GRAPH_SCHEMA {
        return Ok(None);
    }

    let cache_dir = cache_root.join("llvm").join(&latest.content_graph_hash);
    let graph_path = cache_dir.join("graph.json");
    let ll_path = cache_dir.join("module.ll");
    if !graph_path.exists() || !ll_path.exists() {
        return Ok(None);
    }

    let graph_text = match fs::read_to_string(&graph_path) {
        Ok(v) => v,
        Err(err) => {
            trace(&format!(
                "fast cache probe miss: failed to read graph {}: {}",
                graph_path.display(),
                err
            ));
            return Ok(None);
        }
    };
    let graph: BuildGraph = match serde_json::from_str(&graph_text) {
        Ok(v) => v,
        Err(err) => {
            trace(&format!(
                "fast cache probe miss: failed to decode graph {}: {}",
                graph_path.display(),
                err
            ));
            return Ok(None);
        }
    };

    if graph.schema != GRAPH_SCHEMA {
        return Ok(None);
    }
    if graph.entry != normalize_path(entry_path) {
        return Ok(None);
    }
    if graph.mode != mode_label(mode) || graph.offline != offline || graph.want_fetch != want_fetch
    {
        return Ok(None);
    }
    if graph.compiler_fingerprint != compiler_fingerprint() {
        return Ok(None);
    }
    let (mod_hash, lock_hash) = project_mod_lock_hashes(entry_path);
    if graph.mod_hash != mod_hash || graph.lock_hash != lock_hash {
        return Ok(None);
    }

    for pkg in &graph.packages {
        if pkg.file_hashes.is_empty() {
            return Ok(None);
        }
        for file in &pkg.file_hashes {
            let file_path = PathBuf::from(&file.path);
            if !file_path.exists() {
                return Ok(None);
            }
            let actual_hash = match hash_file_sha256(&file_path) {
                Ok(v) => v,
                Err(err) => {
                    trace(&format!(
                        "fast cache probe miss: failed to hash {}: {}",
                        file_path.display(),
                        err
                    ));
                    return Ok(None);
                }
            };
            if actual_hash != file.content_hash {
                return Ok(None);
            }
        }
    }

    trace(&format!("fast llvm cache hit: {}", ll_path.display()));
    let text =
        fs::read_to_string(&ll_path).with_context(|| format!("read {}", ll_path.display()))?;
    let warnings = read_cached_warnings(&cache_dir)?;
    Ok(Some((text, warnings)))
}

pub fn trace_enabled() -> bool {
    std::env::var("GOST_INCREMENTAL_TRACE")
        .ok()
        .as_deref()
        .map(|v| v == "1")
        .unwrap_or(false)
}

pub fn trace(msg: &str) {
    if trace_enabled() {
        eprintln!("[incremental] {}", msg);
    }
}

fn sidecar_canary_enabled() -> bool {
    std::env::var("GOST_INCREMENTAL_SIDECAR_CANARY")
        .ok()
        .as_deref()
        .map(|v| v != "0")
        .unwrap_or(true)
}

#[cfg(test)]
pub(crate) fn reset_incremental_killswitch_for_tests() {
    SIDECAR_HASH_DISABLED.store(false, Ordering::Relaxed);
    INCREMENTAL_CACHE_DISABLED.store(false, Ordering::Relaxed);
}

fn project_mod_lock_hashes(entry_path: &Path) -> (Option<String>, Option<String>) {
    let Some(root) = find_project_root(entry_path) else {
        return (None, None);
    };
    let mod_hash = hash_if_exists(&root.join("gost.mod"));
    let lock_hash = hash_if_exists(&root.join("gost.lock"));
    (mod_hash, lock_hash)
}

fn hash_if_exists(path: &Path) -> Option<String> {
    if !path.exists() {
        return None;
    }
    hash_file_sha256(path).ok()
}

fn compiler_fingerprint() -> String {
    let mut out = format!("gost/{}", env!("CARGO_PKG_VERSION"));
    if let Some(commit) = option_env!("GOST_GIT_COMMIT") {
        let commit = commit.trim();
        if !commit.is_empty() {
            out.push('#');
            out.push_str(commit);
        }
    }
    if let Ok(exe) = std::env::current_exe()
        && let Ok(meta) = fs::metadata(exe)
        && let Ok(modified) = meta.modified()
        && let Ok(dur) = modified.duration_since(UNIX_EPOCH)
    {
        out.push('@');
        out.push_str(&dur.as_secs().to_string());
    }
    out
}

fn find_project_root(entry_path: &Path) -> Option<PathBuf> {
    let mut cur = if entry_path.is_dir() {
        entry_path.to_path_buf()
    } else {
        entry_path.parent()?.to_path_buf()
    };
    loop {
        if cur.join("gost.mod").exists() {
            return Some(cur);
        }
        if !cur.pop() {
            break;
        }
    }
    None
}

fn mode_label(mode: ModMode) -> &'static str {
    match mode {
        ModMode::Readonly => "readonly",
        ModMode::Mod => "mod",
    }
}

fn normalize_path(path: &Path) -> String {
    path.canonicalize()
        .unwrap_or_else(|_| path.to_path_buf())
        .to_string_lossy()
        .replace('\\', "/")
}

fn hex_to_array32(hex_text: &str) -> [u8; 32] {
    let mut out = [0u8; 32];
    if let Ok(bytes) = hex::decode(hex_text)
        && bytes.len() == 32
    {
        out.copy_from_slice(&bytes);
    }
    out
}

fn file_interface_signature(file: &FileAst) -> String {
    let mut parts = Vec::<String>::new();
    for item in &file.items {
        if let Some(sig) = public_item_signature(item) {
            parts.push(sig);
        }
    }
    parts.sort();
    let mut out = String::new();
    out.push_str("package:");
    out.push_str(&file.package);
    out.push('\n');
    for part in parts {
        out.push_str(&part);
        out.push('\n');
    }
    hash_bytes_sha256(out.as_bytes())
}

fn public_item_signature(item: &Item) -> Option<String> {
    match item {
        Item::Function(f) => {
            if !f.vis.is_public() {
                return None;
            }
            let params = f
                .params
                .iter()
                .map(|p| format!("{}:{}", p.name, type_signature(&p.ty)))
                .collect::<Vec<_>>()
                .join(",");
            let tparams = type_params_signature(&f.type_params);
            let ret = f
                .ret_type
                .as_ref()
                .map(type_signature)
                .unwrap_or_else(|| "unit".to_string());
            Some(format!(
                "fn:{}{}({})->{}|variadic={}|extern={}|unsafe={}|abi={}",
                f.name,
                tparams,
                params,
                ret,
                if f.is_variadic { "1" } else { "0" },
                if f.is_extern { "1" } else { "0" },
                if f.is_unsafe { "1" } else { "0" },
                f.extern_abi.as_deref().unwrap_or("")
            ))
        }
        Item::ExternGlobal(g) => {
            if !g.vis.is_public() {
                return None;
            }
            Some(format!(
                "extern-global:{}:{}:{}",
                g.name,
                type_signature(&g.ty),
                g.extern_abi.as_deref().unwrap_or("")
            ))
        }
        Item::TypeAlias(a) => {
            if !a.vis.is_public() {
                return None;
            }
            let trait_methods = if a.is_trait {
                a.trait_methods
                    .iter()
                    .map(trait_method_signature)
                    .collect::<Vec<_>>()
                    .join(";")
            } else {
                String::new()
            };
            Some(format!(
                "type:{}:{}|trait={}|methods={}",
                a.name,
                type_signature(&a.ty),
                if a.is_trait { "1" } else { "0" },
                trait_methods
            ))
        }
        Item::Global(g) => {
            if !g.vis.is_public() {
                return None;
            }
            let ty =
                g.ty.as_ref()
                    .map(type_signature)
                    .unwrap_or_else(|| "_".to_string());
            Some(format!("global:{}:{}", g.name, ty))
        }
        Item::Const(c) => {
            if !c.vis.is_public() {
                return None;
            }
            let ty =
                c.ty.as_ref()
                    .map(type_signature)
                    .unwrap_or_else(|| "_".to_string());
            Some(format!("const:{}:{}", c.name, ty))
        }
        Item::Struct(s) => {
            if !s.vis.is_public() {
                return None;
            }
            let fields = s
                .fields
                .iter()
                .map(|f| {
                    format!(
                        "{}:{}:{}",
                        if f.vis.is_public() { "pub" } else { "priv" },
                        f.name,
                        type_signature(&f.ty)
                    )
                })
                .collect::<Vec<_>>()
                .join(",");
            Some(format!(
                "struct:{}|copy={}|layout={}|fields={}",
                s.name,
                if s.is_copy { "1" } else { "0" },
                layout_signature(&s.layout),
                fields
            ))
        }
        Item::Enum(e) => {
            if !e.vis.is_public() {
                return None;
            }
            let variants = e
                .variants
                .iter()
                .map(|v| {
                    let fields = v
                        .fields
                        .iter()
                        .map(type_signature)
                        .collect::<Vec<_>>()
                        .join(",");
                    format!("{}({})", v.name, fields)
                })
                .collect::<Vec<_>>()
                .join("|");
            Some(format!(
                "enum:{}|copy={}|layout={}|variants={}",
                e.name,
                if e.is_copy { "1" } else { "0" },
                layout_signature(&e.layout),
                variants
            ))
        }
    }
}

fn trait_method_signature(method: &TraitMethod) -> String {
    let tparams = type_params_signature(&method.type_params);
    let params = method
        .params
        .iter()
        .map(|p| format!("{}:{}", p.name, type_signature(&p.ty)))
        .collect::<Vec<_>>()
        .join(",");
    let default_body_sig = method
        .default_body
        .as_ref()
        .map(hash_block_signature)
        .unwrap_or_else(|| "-".to_string());
    format!(
        "{}{}({})->{}|variadic={}|default={}|default_sig={}",
        method.name,
        tparams,
        params,
        type_signature(&method.ret_type),
        if method.is_variadic { "1" } else { "0" },
        if method.default_body.is_some() {
            "1"
        } else {
            "0"
        },
        default_body_sig
    )
}

fn hash_block_signature(block: &Block) -> String {
    let mut h = Sha256::new();
    hash_block_into(&mut h, block);
    hex::encode(h.finalize())
}

fn hash_block_into(h: &mut Sha256, block: &Block) {
    hash_tag(h, "block");
    hash_usize(h, block.stmts.len());
    for stmt in &block.stmts {
        hash_stmt_into(h, stmt);
    }
    match &block.tail {
        Some(t) => {
            hash_tag(h, "tail");
            hash_expr_into(h, t);
        }
        None => hash_tag(h, "notail"),
    }
}

fn hash_stmt_into(h: &mut Sha256, stmt: &Stmt) {
    match stmt {
        Stmt::Let { name, ty, init, .. } => {
            hash_tag(h, "let");
            hash_str(h, name);
            hash_type_opt(h, ty);
            hash_expr_into(h, init);
        }
        Stmt::Const { name, ty, init, .. } => {
            hash_tag(h, "const");
            hash_str(h, name);
            hash_type_opt(h, ty);
            hash_expr_into(h, init);
        }
        Stmt::Assign {
            op, target, value, ..
        } => {
            hash_tag(h, "assign");
            hash_tag(h, assign_op_label(op));
            hash_expr_into(h, target);
            hash_expr_into(h, value);
        }
        Stmt::Expr { expr, .. } => {
            hash_tag(h, "expr");
            hash_expr_into(h, expr);
        }
        Stmt::Return { expr, .. } => {
            hash_tag(h, "return");
            match expr {
                Some(e) => hash_expr_into(h, e),
                None => hash_tag(h, "none"),
            }
        }
        Stmt::Break { label, .. } => {
            hash_tag(h, "break");
            hash_opt_str(h, label.as_deref());
        }
        Stmt::Continue { label, .. } => {
            hash_tag(h, "continue");
            hash_opt_str(h, label.as_deref());
        }
        Stmt::While {
            label, cond, body, ..
        } => {
            hash_tag(h, "while");
            hash_opt_str(h, label.as_deref());
            hash_expr_into(h, cond);
            hash_block_into(h, body);
        }
        Stmt::Loop { label, body, .. } => {
            hash_tag(h, "loop");
            hash_opt_str(h, label.as_deref());
            hash_block_into(h, body);
        }
        Stmt::ForIn {
            label,
            name,
            index,
            iter,
            body,
            ..
        } => {
            hash_tag(h, "forin");
            hash_opt_str(h, label.as_deref());
            hash_str(h, name);
            hash_opt_str(h, index.as_deref());
            hash_expr_into(h, iter);
            hash_block_into(h, body);
        }
        Stmt::ForRange {
            label,
            name,
            index,
            start,
            end,
            inclusive,
            body,
            ..
        } => {
            hash_tag(h, "forrange");
            hash_opt_str(h, label.as_deref());
            hash_str(h, name);
            hash_opt_str(h, index.as_deref());
            hash_expr_into(h, start);
            hash_expr_into(h, end);
            hash_bool(h, *inclusive);
            hash_block_into(h, body);
        }
        Stmt::Select { arms, .. } => {
            hash_tag(h, "select");
            hash_usize(h, arms.len());
            for arm in arms {
                hash_select_arm_into(h, arm);
            }
        }
        Stmt::Go { expr, .. } => {
            hash_tag(h, "go");
            hash_expr_into(h, expr);
        }
        Stmt::Defer { expr, .. } => {
            hash_tag(h, "defer");
            hash_expr_into(h, expr);
        }
    }
}

fn hash_select_arm_into(h: &mut Sha256, arm: &SelectArm) {
    hash_tag(h, "arm");
    match &arm.kind {
        SelectArmKind::Send { chan, value } => {
            hash_tag(h, "send");
            hash_expr_into(h, chan);
            hash_expr_into(h, value);
        }
        SelectArmKind::Recv { chan, bind } => {
            hash_tag(h, "recv");
            hash_expr_into(h, chan);
            match bind {
                Some((a, b)) => {
                    hash_tag(h, "bind");
                    hash_str(h, a);
                    hash_str(h, b);
                }
                None => hash_tag(h, "nobind"),
            }
        }
        SelectArmKind::After { ms } => {
            hash_tag(h, "after");
            hash_expr_into(h, ms);
        }
        SelectArmKind::Default => hash_tag(h, "default"),
    }
    hash_block_or_expr_into(h, &arm.body);
}

fn hash_block_or_expr_into(h: &mut Sha256, body: &BlockOrExpr) {
    match body {
        BlockOrExpr::Block(b) => {
            hash_tag(h, "body_block");
            hash_block_into(h, b);
        }
        BlockOrExpr::Expr(e) => {
            hash_tag(h, "body_expr");
            hash_expr_into(h, e);
        }
    }
}

fn hash_expr_into(h: &mut Sha256, expr: &Expr) {
    match &expr.kind {
        ExprKind::Bool(v) => {
            hash_tag(h, "bool");
            hash_bool(h, *v);
        }
        ExprKind::Int(v) => {
            hash_tag(h, "int");
            hash_str(h, v);
        }
        ExprKind::Float(v) => {
            hash_tag(h, "float");
            if let Ok(parsed) = v.parse::<f64>() {
                hash_tag(h, "bits");
                h.update(parsed.to_bits().to_le_bytes());
                h.update([0]);
            } else {
                hash_tag(h, "lex");
                hash_str(h, v);
            }
        }
        ExprKind::Char(v) => {
            hash_tag(h, "char");
            hash_u32(h, *v as u32);
        }
        ExprKind::String(v) => {
            hash_tag(h, "string");
            hash_str(h, v);
        }
        ExprKind::Nil => hash_tag(h, "nil"),
        ExprKind::Ident(v) => {
            hash_tag(h, "ident");
            hash_str(h, v);
        }
        ExprKind::StructLit { name, fields } => {
            hash_tag(h, "struct_lit");
            hash_str(h, name);
            hash_usize(h, fields.len());
            for (n, e) in fields {
                hash_str(h, n);
                hash_expr_into(h, e);
            }
        }
        ExprKind::ArrayLit(items) => {
            hash_tag(h, "array_lit");
            hash_usize(h, items.len());
            for item in items {
                hash_expr_into(h, item);
            }
        }
        ExprKind::Tuple(items) => {
            hash_tag(h, "tuple");
            hash_usize(h, items.len());
            for item in items {
                hash_expr_into(h, item);
            }
        }
        ExprKind::Block(b) => {
            hash_tag(h, "expr_block");
            hash_block_into(h, b);
        }
        ExprKind::UnsafeBlock(b) => {
            hash_tag(h, "unsafe_block");
            hash_block_into(h, b);
        }
        ExprKind::If {
            cond,
            then_block,
            else_block,
        } => {
            hash_tag(h, "if");
            hash_expr_into(h, cond);
            hash_block_into(h, then_block);
            match else_block {
                Some(b) => {
                    hash_tag(h, "else");
                    hash_block_into(h, b);
                }
                None => hash_tag(h, "noelse"),
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            hash_tag(h, "match");
            hash_expr_into(h, scrutinee);
            hash_usize(h, arms.len());
            for arm in arms {
                hash_pattern_into(h, &arm.pattern);
                match &arm.guard {
                    Some(g) => {
                        hash_tag(h, "guard");
                        hash_expr_into(h, g);
                    }
                    None => hash_tag(h, "noguard"),
                }
                hash_block_or_expr_into(h, &arm.body);
            }
        }
        ExprKind::Closure { params, body } => {
            hash_tag(h, "closure");
            hash_usize(h, params.len());
            for p in params {
                hash_closure_param_into(h, p);
            }
            hash_block_or_expr_into(h, body);
        }
        ExprKind::Call {
            callee,
            type_args,
            args,
        } => {
            hash_tag(h, "call");
            hash_expr_into(h, callee);
            hash_usize(h, type_args.len());
            for ty in type_args {
                hash_type(h, ty);
            }
            hash_usize(h, args.len());
            for a in args {
                hash_expr_into(h, a);
            }
        }
        ExprKind::Field { base, name } => {
            hash_tag(h, "field");
            hash_expr_into(h, base);
            hash_str(h, name);
        }
        ExprKind::Index { base, index } => {
            hash_tag(h, "index");
            hash_expr_into(h, base);
            hash_expr_into(h, index);
        }
        ExprKind::Unary { op, expr } => {
            hash_tag(h, "unary");
            hash_tag(h, unary_op_label(op));
            hash_expr_into(h, expr);
        }
        ExprKind::Cast { expr, ty } => {
            hash_tag(h, "cast");
            hash_expr_into(h, expr);
            hash_type(h, ty);
        }
        ExprKind::Binary { op, left, right } => {
            hash_tag(h, "binary");
            hash_tag(h, binary_op_label(op));
            hash_expr_into(h, left);
            hash_expr_into(h, right);
        }
        ExprKind::Borrow { is_mut, expr } => {
            hash_tag(h, "borrow");
            hash_bool(h, *is_mut);
            hash_expr_into(h, expr);
        }
        ExprKind::Deref { expr } => {
            hash_tag(h, "deref");
            hash_expr_into(h, expr);
        }
        ExprKind::Try { expr } => {
            hash_tag(h, "try");
            hash_expr_into(h, expr);
        }
        ExprKind::Send { chan, value } => {
            hash_tag(h, "send");
            hash_expr_into(h, chan);
            hash_expr_into(h, value);
        }
        ExprKind::Recv { chan } => {
            hash_tag(h, "recv");
            hash_expr_into(h, chan);
        }
        ExprKind::Close { chan } => {
            hash_tag(h, "close");
            hash_expr_into(h, chan);
        }
        ExprKind::After { ms } => {
            hash_tag(h, "after");
            hash_expr_into(h, ms);
        }
    }
}

fn hash_pattern_into(h: &mut Sha256, pattern: &Pattern) {
    match pattern {
        Pattern::Wildcard => hash_tag(h, "wildcard"),
        Pattern::Bool(v) => {
            hash_tag(h, "bool");
            hash_bool(h, *v);
        }
        Pattern::Int(v) => {
            hash_tag(h, "int");
            hash_str(h, v);
        }
        Pattern::Ident(v) => {
            hash_tag(h, "ident");
            hash_str(h, v);
        }
        Pattern::Or(items) => {
            hash_tag(h, "or");
            hash_usize(h, items.len());
            for p in items {
                hash_pattern_into(h, p);
            }
        }
        Pattern::Variant {
            enum_name,
            variant,
            binds,
        } => {
            hash_tag(h, "variant");
            hash_str(h, enum_name);
            hash_str(h, variant);
            hash_usize(h, binds.len());
            for b in binds {
                hash_str(h, b);
            }
        }
    }
}

fn hash_closure_param_into(h: &mut Sha256, param: &ClosureParam) {
    hash_tag(h, "closure_param");
    hash_str(h, &param.name);
    hash_type_opt(h, &param.ty);
}

fn hash_type_opt(h: &mut Sha256, ty: &Option<TypeAst>) {
    match ty {
        Some(t) => {
            hash_tag(h, "some");
            hash_type(h, t);
        }
        None => hash_tag(h, "none"),
    }
}

fn hash_type(h: &mut Sha256, ty: &TypeAst) {
    hash_tag(h, "type");
    hash_str(h, &type_signature(ty));
}

fn hash_tag(h: &mut Sha256, tag: &str) {
    h.update(tag.as_bytes());
    h.update([0]);
}

fn hash_str(h: &mut Sha256, v: &str) {
    h.update(v.len().to_string().as_bytes());
    h.update(b":");
    h.update(v.as_bytes());
    h.update([0]);
}

fn hash_opt_str(h: &mut Sha256, v: Option<&str>) {
    match v {
        Some(s) => {
            hash_tag(h, "some");
            hash_str(h, s);
        }
        None => hash_tag(h, "none"),
    }
}

fn hash_usize(h: &mut Sha256, v: usize) {
    h.update(v.to_string().as_bytes());
    h.update([0]);
}

fn hash_u32(h: &mut Sha256, v: u32) {
    h.update(v.to_string().as_bytes());
    h.update([0]);
}

fn hash_bool(h: &mut Sha256, v: bool) {
    h.update(if v { b"1" } else { b"0" });
    h.update([0]);
}

fn assign_op_label(op: &AssignOp) -> &'static str {
    match op {
        AssignOp::Assign => "=",
        AssignOp::AddAssign => "+=",
        AssignOp::SubAssign => "-=",
        AssignOp::MulAssign => "*=",
        AssignOp::DivAssign => "/=",
        AssignOp::RemAssign => "%=",
        AssignOp::BitAndAssign => "&=",
        AssignOp::BitOrAssign => "|=",
        AssignOp::BitXorAssign => "^=",
        AssignOp::ShlAssign => "<<=",
        AssignOp::ShrAssign => ">>=",
    }
}

fn unary_op_label(op: &UnaryOp) -> &'static str {
    match op {
        UnaryOp::Neg => "-",
        UnaryOp::Not => "!",
        UnaryOp::BitNot => "~",
    }
}

fn binary_op_label(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::Rem => "%",
        BinaryOp::Eq => "==",
        BinaryOp::NotEq => "!=",
        BinaryOp::Lt => "<",
        BinaryOp::Lte => "<=",
        BinaryOp::Gt => ">",
        BinaryOp::Gte => ">=",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
        BinaryOp::BitAnd => "&",
        BinaryOp::BitOr => "|",
        BinaryOp::BitXor => "^",
        BinaryOp::Shl => "<<",
        BinaryOp::Shr => ">>",
    }
}

fn type_params_signature(params: &[TypeParam]) -> String {
    if params.is_empty() {
        return String::new();
    }
    let body = params
        .iter()
        .map(|p| {
            if p.bounds.is_empty() {
                p.name.clone()
            } else {
                let bounds = p
                    .bounds
                    .iter()
                    .map(type_signature)
                    .collect::<Vec<_>>()
                    .join("+");
                format!("{}:{}", p.name, bounds)
            }
        })
        .collect::<Vec<_>>()
        .join(",");
    format!("[{}]", body)
}

fn type_signature(ty: &TypeAst) -> String {
    match &ty.kind {
        TypeAstKind::Named(name) => name.clone(),
        TypeAstKind::Ref(inner) => format!("&{}", type_signature(inner)),
        TypeAstKind::MutRef(inner) => format!("&mut {}", type_signature(inner)),
        TypeAstKind::Own(inner) => format!("own<{}>", type_signature(inner)),
        TypeAstKind::Alias(inner) => format!("alias<{}>", type_signature(inner)),
        TypeAstKind::Slice(inner) => format!("slice<{}>", type_signature(inner)),
        TypeAstKind::Array(inner, len) => format!("[{};{}]", type_signature(inner), len),
        TypeAstKind::Map(k, v) => format!("map<{},{}>", type_signature(k), type_signature(v)),
        TypeAstKind::Result(ok, err) => {
            format!("result<{},{}>", type_signature(ok), type_signature(err))
        }
        TypeAstKind::Chan(inner) => format!("chan<{}>", type_signature(inner)),
        TypeAstKind::Shared(inner) => format!("shared<{}>", type_signature(inner)),
        TypeAstKind::Interface => "interface".to_string(),
        TypeAstKind::Tuple(items) => {
            let joined = items
                .iter()
                .map(type_signature)
                .collect::<Vec<_>>()
                .join(",");
            format!("({})", joined)
        }
        TypeAstKind::FnPtr {
            params,
            ret,
            is_variadic,
        } => {
            let p = params
                .iter()
                .map(type_signature)
                .collect::<Vec<_>>()
                .join(",");
            format!(
                "fnptr({}{})->{}",
                p,
                if *is_variadic {
                    if p.is_empty() { "..." } else { ",..." }
                } else {
                    ""
                },
                type_signature(ret)
            )
        }
        TypeAstKind::Closure {
            params,
            ret,
            is_variadic,
        } => {
            let p = params
                .iter()
                .map(type_signature)
                .collect::<Vec<_>>()
                .join(",");
            format!(
                "closure({}{})->{}",
                p,
                if *is_variadic {
                    if p.is_empty() { "..." } else { ",..." }
                } else {
                    ""
                },
                type_signature(ret)
            )
        }
    }
}

fn layout_signature(layout: &LayoutAttr) -> String {
    let repr_int = layout.repr_int.as_ref().map(repr_int_label).unwrap_or("");
    format!(
        "c={};transparent={};int={};other={};pack={};bitfield={}",
        if layout.repr_c { "1" } else { "0" },
        if layout.repr_transparent { "1" } else { "0" },
        repr_int,
        layout.repr_other.as_deref().unwrap_or(""),
        layout
            .pack
            .map(|v| v.to_string())
            .unwrap_or_else(|| "".to_string()),
        if layout.bitfield { "1" } else { "0" }
    )
}

fn repr_int_label(repr: &ReprInt) -> &'static str {
    match repr {
        ReprInt::I8 => "i8",
        ReprInt::I16 => "i16",
        ReprInt::I32 => "i32",
        ReprInt::I64 => "i64",
        ReprInt::Isize => "isize",
        ReprInt::U8 => "u8",
        ReprInt::U16 => "u16",
        ReprInt::U32 => "u32",
        ReprInt::U64 => "u64",
        ReprInt::Usize => "usize",
    }
}

fn latest_graph_index_path(cache_root: &Path, entry_path: &Path) -> PathBuf {
    let key = hash_bytes_sha256(normalize_path(entry_path).as_bytes());
    cache_root
        .join("llvm")
        .join("latest")
        .join(format!("{key}.json"))
}

fn warnings_sidecar_path(cache_dir: &Path) -> PathBuf {
    cache_dir.join("warnings.json")
}

fn read_cached_warnings(cache_dir: &Path) -> Result<Vec<String>> {
    let warn_path = warnings_sidecar_path(cache_dir);
    if !warn_path.exists() {
        return Ok(Vec::new());
    }
    let text =
        fs::read_to_string(&warn_path).with_context(|| format!("read {}", warn_path.display()))?;
    let parsed = serde_json::from_str::<CachedWarnings>(&text)
        .with_context(|| format!("decode {}", warn_path.display()))?;
    Ok(parsed.warnings)
}

fn write_cached_warnings(cache_dir: &Path, warnings: &[String]) -> Result<()> {
    let warn_path = warnings_sidecar_path(cache_dir);
    if warnings.is_empty() {
        let _ = fs::remove_file(&warn_path);
        return Ok(());
    }
    let mut deduped = Vec::<String>::new();
    let mut seen = BTreeSet::<String>::new();
    for line in warnings {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        if seen.insert(trimmed.to_string()) {
            deduped.push(trimmed.to_string());
        }
    }
    if deduped.is_empty() {
        let _ = fs::remove_file(&warn_path);
        return Ok(());
    }
    let payload = CachedWarnings { warnings: deduped };
    fs::write(&warn_path, serde_json::to_string_pretty(&payload)?)
        .with_context(|| format!("write {}", warn_path.display()))?;
    Ok(())
}

fn write_latest_graph_index(
    cache_root: &Path,
    entry_path: &Path,
    graph: &BuildGraph,
) -> Result<()> {
    let idx_path = latest_graph_index_path(cache_root, entry_path);
    if let Some(parent) = idx_path.parent() {
        ensure_dir(parent)?;
    }
    let payload = LatestGraphIndex {
        schema: GRAPH_SCHEMA,
        entry: normalize_path(entry_path),
        content_graph_hash: graph.content_graph_hash.clone(),
    };
    fs::write(&idx_path, serde_json::to_string_pretty(&payload)?)
        .with_context(|| format!("write {}", idx_path.display()))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{
        BuildCtx, BuildGraph, BuildGraphPackage, ENTRY_PACKAGE, build_cache_root_for_entry,
        reset_incremental_killswitch_for_tests, try_load_cached_llvm_fast,
        try_load_cached_llvm_fast_with_warnings,
    };
    use crate::frontend::ast::FileAst;
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;
    use crate::pkg::resolve::ModMode;
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_dir(prefix: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time drift")
            .as_nanos();
        std::env::temp_dir().join(format!(
            "gost-inc-{}-{}-{}",
            prefix,
            std::process::id(),
            nonce
        ))
    }

    fn parse_file(source: &str, path: &Path) -> FileAst {
        let tokens = Lexer::new(source).lex_all();
        let mut parser = Parser::new(tokens);
        parser.set_module_disambiguator(path.to_string_lossy().into_owned());
        let file = parser.parse_file().expect("parse file");
        assert!(
            parser.diags.is_empty(),
            "unexpected parser diagnostics in test"
        );
        file
    }

    fn package_hashes(ctx: &mut BuildCtx, package: &str) -> (String, String) {
        let graph = ctx.graph();
        let pkg = graph
            .packages
            .iter()
            .find(|p| p.package == package)
            .expect("package exists");
        (pkg.content_hash.clone(), pkg.interface_hash.clone())
    }

    #[test]
    fn interface_hash_ignores_private_function_body_changes() {
        let root = temp_dir("iface-private");
        fs::create_dir_all(&root).expect("mkdir");
        fs::write(root.join("gost.mod"), "module = \"example.com/app\"\n").expect("mod");
        let path = root.join("main.gs");

        let src_a = "module main\npub fn api(x: i64) -> i64 { return x }\nfn hidden() -> i64 { return 1 }\n";
        let src_b = "module main\npub fn api(x: i64) -> i64 { return x }\nfn hidden() -> i64 { return 999 }\n";

        let mut a = BuildCtx::new(&path, ModMode::Readonly, true, false);
        let file_a = parse_file(src_a, &path);
        a.record_source_file(ENTRY_PACKAGE, &path, src_a, &file_a);
        let (content_a, iface_a) = package_hashes(&mut a, ENTRY_PACKAGE);

        let mut b = BuildCtx::new(&path, ModMode::Readonly, true, false);
        let file_b = parse_file(src_b, &path);
        b.record_source_file(ENTRY_PACKAGE, &path, src_b, &file_b);
        let (content_b, iface_b) = package_hashes(&mut b, ENTRY_PACKAGE);

        assert_ne!(content_a, content_b);
        assert_eq!(iface_a, iface_b);

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn interface_hash_changes_for_public_signature_change() {
        let root = temp_dir("iface-public");
        fs::create_dir_all(&root).expect("mkdir");
        fs::write(root.join("gost.mod"), "module = \"example.com/app\"\n").expect("mod");
        let path = root.join("main.gs");

        let src_a = "module main\npub fn api(x: i64) -> i64 { return x }\n";
        let src_b = "module main\npub fn api(x: i64, y: i64) -> i64 { return x + y }\n";

        let mut a = BuildCtx::new(&path, ModMode::Readonly, true, false);
        let file_a = parse_file(src_a, &path);
        a.record_source_file(ENTRY_PACKAGE, &path, src_a, &file_a);
        let (_, iface_a) = package_hashes(&mut a, ENTRY_PACKAGE);

        let mut b = BuildCtx::new(&path, ModMode::Readonly, true, false);
        let file_b = parse_file(src_b, &path);
        b.record_source_file(ENTRY_PACKAGE, &path, src_b, &file_b);
        let (_, iface_b) = package_hashes(&mut b, ENTRY_PACKAGE);

        assert_ne!(iface_a, iface_b);
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn interface_hash_ignores_public_item_order_changes() {
        let root = temp_dir("iface-order");
        fs::create_dir_all(&root).expect("mkdir");
        fs::write(root.join("gost.mod"), "module = \"example.com/app\"\n").expect("mod");
        let path = root.join("main.gs");

        let src_a = "module main\npub fn a(x: i64) -> i64 { return x }\npub fn b(y: i64) -> i64 { return y }\n";
        let src_b = "module main\npub fn b(y: i64) -> i64 { return y }\npub fn a(x: i64) -> i64 { return x }\n";

        let mut a = BuildCtx::new(&path, ModMode::Readonly, true, false);
        let file_a = parse_file(src_a, &path);
        a.record_source_file(ENTRY_PACKAGE, &path, src_a, &file_a);
        let (_, iface_a) = package_hashes(&mut a, ENTRY_PACKAGE);

        let mut b = BuildCtx::new(&path, ModMode::Readonly, true, false);
        let file_b = parse_file(src_b, &path);
        b.record_source_file(ENTRY_PACKAGE, &path, src_b, &file_b);
        let (_, iface_b) = package_hashes(&mut b, ENTRY_PACKAGE);

        assert_eq!(iface_a, iface_b);
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn interface_hash_changes_for_trait_default_body_change() {
        let root = temp_dir("iface-trait-default");
        fs::create_dir_all(&root).expect("mkdir");
        fs::write(root.join("gost.mod"), "module = \"example.com/app\"\n").expect("mod");
        let path = root.join("main.gs");

        let src_a = "module main\n\ntrait Greeter {\n    fn greet() -> i64 {\n        return self.id + 1\n    }\n}\n\ncopy struct User {\n    id: i64\n}\n";
        let src_b = "module main\n\ntrait Greeter {\n    fn greet() -> i64 {\n        return self.id + 2\n    }\n}\n\ncopy struct User {\n    id: i64\n}\n";

        let mut a = BuildCtx::new(&path, ModMode::Readonly, true, false);
        let file_a = parse_file(src_a, &path);
        a.record_source_file(ENTRY_PACKAGE, &path, src_a, &file_a);
        let (_, iface_a) = package_hashes(&mut a, ENTRY_PACKAGE);

        let mut b = BuildCtx::new(&path, ModMode::Readonly, true, false);
        let file_b = parse_file(src_b, &path);
        b.record_source_file(ENTRY_PACKAGE, &path, src_b, &file_b);
        let (_, iface_b) = package_hashes(&mut b, ENTRY_PACKAGE);

        assert_ne!(iface_a, iface_b);
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn llvm_cache_roundtrip_works_with_same_graph() {
        let root = temp_dir("cache-roundtrip");
        fs::create_dir_all(&root).expect("mkdir");
        fs::write(root.join("gost.mod"), "module = \"example.com/app\"\n").expect("mod");
        let path = root.join("main.gs");
        let src = "module main\nfn main() -> i32 { return 0 }\n";
        let file = parse_file(src, &path);

        let mut writer = BuildCtx::new(&path, ModMode::Readonly, true, false);
        writer.record_source_file(ENTRY_PACKAGE, &path, src, &file);
        writer
            .store_cached_llvm("; cached llvm module\n")
            .expect("store");

        let mut reader = BuildCtx::new(&path, ModMode::Readonly, true, false);
        reader.record_source_file(ENTRY_PACKAGE, &path, src, &file);
        let loaded = reader.try_load_cached_llvm().expect("load");
        assert_eq!(loaded.as_deref(), Some("; cached llvm module\n"));

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn cache_root_prefers_project_local_directory() {
        let root = temp_dir("cache-root");
        fs::create_dir_all(&root).expect("mkdir");
        fs::write(root.join("gost.mod"), "module = \"example.com/app\"\n").expect("mod");
        let entry = root.join("main.gs");
        fs::write(&entry, "module main\nfn main() -> i32 { return 0 }\n").expect("entry");

        let cache = build_cache_root_for_entry(&entry).expect("cache root");
        assert!(cache.ends_with(".gost/cache/build"));

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn fast_cache_probe_hits_when_sources_match() {
        let root = temp_dir("fast-hit");
        fs::create_dir_all(&root).expect("mkdir");
        fs::write(root.join("gost.mod"), "module = \"example.com/app\"\n").expect("mod");
        let entry = root.join("main.gs");
        let src = "module main\nfn main() -> i32 { return 0 }\n";
        fs::write(&entry, src).expect("entry");
        let file = parse_file(src, &entry);

        let mut writer = BuildCtx::new(&entry, ModMode::Readonly, true, false);
        writer.record_source_file(ENTRY_PACKAGE, &entry, src, &file);
        writer
            .store_cached_llvm("; fast cached llvm module\n")
            .expect("store");

        let loaded =
            try_load_cached_llvm_fast(&entry, ModMode::Readonly, true, false).expect("probe");
        assert_eq!(loaded.as_deref(), Some("; fast cached llvm module\n"));
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn fast_cache_probe_misses_when_source_hash_changes() {
        let root = temp_dir("fast-miss");
        fs::create_dir_all(&root).expect("mkdir");
        fs::write(root.join("gost.mod"), "module = \"example.com/app\"\n").expect("mod");
        let entry = root.join("main.gs");
        let src = "module main\nfn main() -> i32 { return 0 }\n";
        fs::write(&entry, src).expect("entry");
        let file = parse_file(src, &entry);

        let mut writer = BuildCtx::new(&entry, ModMode::Readonly, true, false);
        writer.record_source_file(ENTRY_PACKAGE, &entry, src, &file);
        writer
            .store_cached_llvm("; fast cached llvm module\n")
            .expect("store");

        fs::write(&entry, "module main\nfn main() -> i32 { return 1 }\n").expect("mutate");
        let loaded =
            try_load_cached_llvm_fast(&entry, ModMode::Readonly, true, false).expect("probe");
        assert!(loaded.is_none());
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn graph_resnapshots_lock_hash_before_materialization() {
        let root = temp_dir("lock-resnapshot");
        fs::create_dir_all(&root).expect("mkdir");
        fs::write(root.join("gost.mod"), "module = \"example.com/app\"\n").expect("mod");
        let entry = root.join("main.gs");
        let src = "module main\nfn main() -> i32 { return 0 }\n";
        fs::write(&entry, src).expect("entry");
        let file = parse_file(src, &entry);

        let mut ctx = BuildCtx::new(&entry, ModMode::Mod, true, false);
        // Simulate resolver writing gost.lock after BuildCtx::new captured initial hashes.
        fs::write(
            root.join("gost.lock"),
            "module = \"example.com/app\"\n[[modules]]\nmodule = \"x\"\nrequested = \"v0.1.0\"\nrev = \"abc\"\nsource = \"git\"\nchecksum = \"deadbeef\"\n",
        )
        .expect("lock");
        ctx.record_source_file(ENTRY_PACKAGE, &entry, src, &file);
        let graph = ctx.graph();
        assert!(graph.lock_hash.is_some());
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn build_graph_levels_group_independent_packages() {
        let graph = BuildGraph {
            schema: 2,
            entry: "entry".into(),
            mode: "readonly".into(),
            offline: true,
            want_fetch: false,
            compiler_fingerprint: "test".into(),
            packages: vec![
                BuildGraphPackage {
                    package: "__entry__".into(),
                    file_hashes: vec![],
                    files: vec![],
                    imports: vec!["a".into(), "b".into()],
                    content_hash: "x".into(),
                    interface_hash: "x".into(),
                },
                BuildGraphPackage {
                    package: "a".into(),
                    file_hashes: vec![],
                    files: vec![],
                    imports: vec!["c".into()],
                    content_hash: "x".into(),
                    interface_hash: "x".into(),
                },
                BuildGraphPackage {
                    package: "b".into(),
                    file_hashes: vec![],
                    files: vec![],
                    imports: vec!["c".into()],
                    content_hash: "x".into(),
                    interface_hash: "x".into(),
                },
                BuildGraphPackage {
                    package: "c".into(),
                    file_hashes: vec![],
                    files: vec![],
                    imports: vec![],
                    content_hash: "x".into(),
                    interface_hash: "x".into(),
                },
            ],
            content_graph_hash: "x".into(),
            interface_graph_hash: "x".into(),
            mod_hash: None,
            lock_hash: None,
        };
        let levels = graph.package_codegen_levels();
        assert!(
            !levels.is_empty(),
            "package_codegen_levels should produce at least one level"
        );
        assert_eq!(
            levels[0],
            vec!["c".to_string()],
            "root dependency should be first"
        );
        assert!(
            levels
                .iter()
                .any(|lvl| lvl == &vec!["a".to_string(), "b".to_string()]),
            "independent package siblings should share a level"
        );
        assert_eq!(graph.max_parallel_codegen_width(), 2);
    }

    #[test]
    fn cached_warnings_roundtrip_for_llvm_cache() {
        let root = temp_dir("warning-roundtrip");
        fs::create_dir_all(&root).expect("mkdir");
        fs::write(root.join("gost.mod"), "module = \"example.com/app\"\n").expect("mod");
        let entry = root.join("main.gs");
        let src = "module main\nfn main() -> i32 { return 0 }\n";
        fs::write(&entry, src).expect("entry");
        let file = parse_file(src, &entry);

        let warnings = vec![
            "warning: sample persisted warning".to_string(),
            "warning: sample persisted warning".to_string(),
            "warning: another warning".to_string(),
        ];
        let mut writer = BuildCtx::new(&entry, ModMode::Readonly, true, false);
        writer.record_source_file(ENTRY_PACKAGE, &entry, src, &file);
        writer
            .store_cached_llvm_with_warnings("; cached llvm module\n", &warnings)
            .expect("store");

        let mut reader = BuildCtx::new(&entry, ModMode::Readonly, true, false);
        reader.record_source_file(ENTRY_PACKAGE, &entry, src, &file);
        let got = reader.try_load_cached_warnings().expect("read warnings");
        assert_eq!(
            got,
            vec![
                "warning: sample persisted warning".to_string(),
                "warning: another warning".to_string()
            ]
        );

        let fast = try_load_cached_llvm_fast_with_warnings(&entry, ModMode::Readonly, true, false)
            .expect("fast probe")
            .expect("cached result");
        assert_eq!(fast.1, got);
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn sidecar_canary_mismatch_disables_incremental_cache() {
        reset_incremental_killswitch_for_tests();
        let root = temp_dir("sidecar-canary-mismatch");
        fs::create_dir_all(&root).expect("mkdir");
        fs::write(root.join("gost.mod"), "module = \"example.com/app\"\n").expect("mod");
        let entry = root.join("main.gs");
        let src = "module main\nfn main() -> i32 { return 0 }\n";
        fs::write(&entry, src).expect("entry");
        let file = parse_file(src, &entry);

        let mut ctx = BuildCtx::new(&entry, ModMode::Readonly, true, false);
        let bogus = "0000000000000000000000000000000000000000000000000000000000000000";
        ctx.record_source_file_with_hash(ENTRY_PACKAGE, &entry, src, &file, Some(bogus));

        ctx.store_cached_llvm("; should not persist while cache disabled\n")
            .expect("store noop");
        let loaded = ctx.try_load_cached_llvm().expect("load");
        assert!(loaded.is_none());

        reset_incremental_killswitch_for_tests();
        let _ = fs::remove_dir_all(root);
    }
}
