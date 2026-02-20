// Purpose: Provide VCS interaction helpers used by module resolution/fetch operations.
// Inputs/Outputs: Executes git-oriented commands and normalizes source/revision information.
// Invariants: VCS calls must be policy-aware and deterministic under offline constraints.
// Gotchas: URL normalization and transport handling can affect cache identity.

use anyhow::{Context, bail};
use semver::{BuildMetadata, Op, Version, VersionReq};
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::{io::Cursor, path::Component};
use zip::ZipArchive;

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

fn escape_ref(r: &str) -> String {
    r.chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || matches!(c, '.' | '_' | '-') {
                c
            } else {
                '!'
            }
        })
        .collect()
}

#[derive(Debug, Clone)]
enum SourceBackend {
    Git(String),
    Proxy(String),
}

fn parse_source_backend(source_url: &str) -> SourceBackend {
    if let Some(rest) = source_url.strip_prefix("git+") {
        return SourceBackend::Git(rest.to_string());
    }
    if let Some(rest) = source_url.strip_prefix("proxy+") {
        return SourceBackend::Proxy(rest.trim_end_matches('/').to_string());
    }
    SourceBackend::Git(source_url.to_string())
}

fn file_url_to_path(url: &str) -> Option<PathBuf> {
    let rest = url.strip_prefix("file://")?;
    #[cfg(windows)]
    {
        if rest.len() >= 3 && rest.starts_with('/') && rest.as_bytes()[2] == b':' {
            return Some(PathBuf::from(&rest[1..]));
        }
    }
    Some(PathBuf::from(rest))
}

fn proxy_read_text(base: &str, rel: &str) -> anyhow::Result<String> {
    if let Some(base_dir) = file_url_to_path(base) {
        let p = base_dir.join(rel);
        return fs::read_to_string(&p).with_context(|| format!("read {}", p.display()));
    }
    let url = format!("{}/{}", base.trim_end_matches('/'), rel);
    let resp = ureq::get(&url)
        .call()
        .map_err(|e| anyhow::anyhow!("http GET {} failed: {}", url, e))?;
    let mut body = String::new();
    resp.into_reader().read_to_string(&mut body)?;
    Ok(body)
}

fn proxy_read_bytes(base: &str, rel: &str) -> anyhow::Result<Vec<u8>> {
    if let Some(base_dir) = file_url_to_path(base) {
        let p = base_dir.join(rel);
        return fs::read(&p).with_context(|| format!("read {}", p.display()));
    }
    let url = format!("{}/{}", base.trim_end_matches('/'), rel);
    let resp = ureq::get(&url)
        .call()
        .map_err(|e| anyhow::anyhow!("http GET {} failed: {}", url, e))?;
    let mut buf = Vec::new();
    let mut reader = resp.into_reader();
    reader.read_to_end(&mut buf)?;
    Ok(buf)
}

fn safe_rel_path(p: &Path) -> anyhow::Result<PathBuf> {
    let mut out = PathBuf::new();
    for c in p.components() {
        match c {
            Component::Normal(seg) => out.push(seg),
            Component::CurDir => {}
            Component::Prefix(_) | Component::RootDir | Component::ParentDir => {
                bail!("unsafe path in archive entry: {}", p.display())
            }
        }
    }
    Ok(out)
}

fn unzip_module(bytes: &[u8], dst: &Path) -> anyhow::Result<()> {
    ensure_dir(dst)?;
    let cursor = Cursor::new(bytes.to_vec());
    let mut zip = ZipArchive::new(cursor).context("invalid zip archive")?;
    let mut common_prefix: Option<Vec<std::ffi::OsString>> = None;
    let mut max_depth = 0usize;

    for i in 0..zip.len() {
        let f = zip.by_index(i)?;
        let name = f.name().to_string();
        if name.is_empty() {
            continue;
        }
        let rel = safe_rel_path(Path::new(&name))?;
        if rel.as_os_str().is_empty() {
            continue;
        }

        let mut comps = Vec::<std::ffi::OsString>::new();
        for c in rel.components() {
            match c {
                Component::Normal(seg) => comps.push(seg.to_os_string()),
                _ => {
                    comps.clear();
                    break;
                }
            }
        }
        if comps.is_empty() {
            continue;
        }
        max_depth = max_depth.max(comps.len());

        match &mut common_prefix {
            None => common_prefix = Some(comps),
            Some(prefix) => {
                let mut keep = 0usize;
                while keep < prefix.len() && keep < comps.len() && prefix[keep] == comps[keep] {
                    keep += 1;
                }
                prefix.truncate(keep);
            }
        }
    }
    let root_prefix = common_prefix.and_then(|parts| {
        if parts.is_empty() || max_depth <= parts.len() {
            return None;
        }
        let mut p = PathBuf::new();
        for seg in parts {
            p.push(seg);
        }
        Some(p)
    });

    for i in 0..zip.len() {
        let mut f = zip.by_index(i)?;
        let name = f.name().to_string();
        if name.is_empty() {
            continue;
        }
        let mut rel = safe_rel_path(Path::new(&name))?;
        if let Some(root) = root_prefix.as_ref()
            && let Ok(stripped) = rel.strip_prefix(root)
        {
            rel = stripped.to_path_buf();
        }
        if rel.as_os_str().is_empty() {
            continue;
        }
        let out = dst.join(rel);
        if f.is_dir() {
            ensure_dir(&out)?;
            continue;
        }
        if let Some(parent) = out.parent() {
            ensure_dir(parent)?;
        }
        let mut w = fs::File::create(&out).with_context(|| format!("create {}", out.display()))?;
        std::io::copy(&mut f, &mut w)?;
    }
    Ok(())
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
                &["-C", mirror.to_str().unwrap(), "fetch", "--prune", "--tags"],
                None,
            )?;
        }
        return Ok(mirror);
    }
    if !allow_fetch {
        bail!("missing mirror for {} (readonly/offline)", source_url);
    }
    let _ = run_git(
        &["clone", "--mirror", source_url, mirror.to_str().unwrap()],
        None,
    )?;
    Ok(mirror)
}

fn looks_like_sha(s: &str) -> bool {
    let n = s.len();
    (7..=40).contains(&n) && s.bytes().all(|c| c.is_ascii_hexdigit())
}

fn parse_version_loose(raw: &str) -> Option<Version> {
    let t = raw.trim();
    if t.is_empty() {
        return None;
    }
    let t = t.strip_prefix('v').unwrap_or(t);
    Version::parse(t).ok()
}

fn is_semver_boundary(ch: char) -> bool {
    ch.is_ascii_whitespace() || matches!(ch, ',' | '<' | '>' | '=' | '^' | '~')
}

fn normalize_req_for_semver(raw: &str) -> String {
    let chars: Vec<char> = raw.trim().chars().collect();
    let mut out = String::with_capacity(chars.len());
    for (i, &ch) in chars.iter().enumerate() {
        if ch == 'v'
            && i + 1 < chars.len()
            && chars[i + 1].is_ascii_digit()
            && (i == 0 || is_semver_boundary(chars[i - 1]))
        {
            continue;
        }
        out.push(ch);
    }
    out
}

fn parse_version_req_loose(raw: &str) -> Option<VersionReq> {
    let t = raw.trim();
    if t.is_empty() {
        return None;
    }
    if let Some(v) = parse_version_loose(t) {
        return VersionReq::parse(&format!("={}", v)).ok();
    }
    let normalized = normalize_req_for_semver(t);
    VersionReq::parse(&normalized).ok()
}

fn req_lower_bound(req: &VersionReq) -> Version {
    let mut best = Version::new(0, 0, 0);
    for c in &req.comparators {
        let mut v = Version {
            major: c.major,
            minor: c.minor.unwrap_or(0),
            patch: c.patch.unwrap_or(0),
            pre: c.pre.clone(),
            build: BuildMetadata::EMPTY,
        };
        match c.op {
            Op::Exact | Op::GreaterEq | Op::Caret | Op::Tilde | Op::Wildcard => {}
            Op::Greater => {
                if c.patch.is_some() {
                    v.patch = v.patch.saturating_add(1);
                    v.pre = Default::default();
                } else if c.minor.is_some() {
                    v.minor = v.minor.saturating_add(1);
                    v.patch = 0;
                    v.pre = Default::default();
                } else {
                    v.major = v.major.saturating_add(1);
                    v.minor = 0;
                    v.patch = 0;
                    v.pre = Default::default();
                }
            }
            Op::Less | Op::LessEq => {
                continue;
            }
            _ => {}
        }
        if v > best {
            best = v;
        }
    }
    best
}

pub fn requirement_min_version(raw: &str) -> Option<Version> {
    let req = parse_version_req_loose(raw)?;
    Some(req_lower_bound(&req))
}

pub fn merge_requested_versions(current: &str, candidate: &str) -> String {
    if current == candidate {
        return current.to_string();
    }
    match (
        requirement_min_version(current),
        requirement_min_version(candidate),
    ) {
        (Some(a), Some(b)) => {
            if b > a {
                candidate.to_string()
            } else {
                current.to_string()
            }
        }
        _ => current.to_string(),
    }
}

pub fn is_mvs_compatible(required: &str, selected: &str) -> bool {
    if required == selected {
        return true;
    }
    match (
        requirement_min_version(required),
        requirement_min_version(selected),
    ) {
        (Some(req_min), Some(sel_min)) => sel_min >= req_min,
        _ => false,
    }
}

fn list_semver_tags(mirror: &Path) -> anyhow::Result<Vec<(Version, String)>> {
    let out = run_git(&["-C", mirror.to_str().unwrap(), "tag", "--list"], None)?;
    let mut tags = Vec::new();
    for line in out.lines() {
        let t = line.trim();
        if t.is_empty() {
            continue;
        }
        if let Some(ver) = parse_version_loose(t) {
            tags.push((ver, t.to_string()));
        }
    }
    tags.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(tags)
}

fn select_semver_from_labels(requested: &str, labels: &[String]) -> anyhow::Result<String> {
    let req = parse_version_req_loose(requested)
        .ok_or_else(|| anyhow::anyhow!("invalid semver requirement `{}`", requested))?;
    let mut best: Option<(Version, String)> = None;
    for label in labels {
        if let Some(ver) = parse_version_loose(label)
            && req.matches(&ver)
        {
            match &best {
                Some((bver, _)) if ver <= *bver => {}
                _ => best = Some((ver, label.clone())),
            }
        }
    }
    best.map(|(_, s)| s)
        .ok_or_else(|| anyhow::anyhow!("no matching semver version for `{}`", requested))
}

fn list_proxy_versions(base: &str, module: &str) -> anyhow::Result<Vec<String>> {
    let rel = format!("{}/@v/list", module);
    let list = proxy_read_text(base, &rel)?;
    let mut out = Vec::new();
    for line in list.lines() {
        let t = line.trim();
        if !t.is_empty() {
            out.push(t.to_string());
        }
    }
    Ok(out)
}

fn resolve_proxy_version(base: &str, module: &str, requested: &str) -> anyhow::Result<String> {
    if parse_version_req_loose(requested).is_none() {
        return Ok(requested.to_string());
    }
    let list = list_proxy_versions(base, module)?;
    if list.is_empty() {
        bail!("proxy has no versions listed for {}", module);
    }
    select_semver_from_labels(requested, &list)
}

fn materialize_proxy_module(
    cache_root: &Path,
    module: &str,
    base: &str,
    requested: &str,
    locked_rev: Option<&str>,
    allow_fetch: bool,
) -> anyhow::Result<(PathBuf, String)> {
    let version = match locked_rev {
        Some(v) => v.to_string(),
        None => resolve_proxy_version(base, module, requested)?,
    };

    let mod_dir = cache_root.join("mod");
    ensure_dir(&mod_dir)?;
    let dst = mod_dir.join(format!(
        "{}@{}",
        escape_module(module),
        escape_ref(&version)
    ));
    if dst.exists() {
        return Ok((dst, version));
    }
    if !allow_fetch {
        bail!(
            "missing cached proxy module {}@{} (readonly/offline)",
            module,
            version
        );
    }

    let rel = format!("{}/@v/{}.zip", module, version);
    let zip_bytes = proxy_read_bytes(base, &rel)?;
    let tmp = dst.with_extension("tmp");
    if tmp.exists() {
        fs::remove_dir_all(&tmp).ok();
    }
    ensure_dir(&tmp)?;
    unzip_module(&zip_bytes, &tmp)?;
    if dst.exists() {
        fs::remove_dir_all(&dst).ok();
    }
    fs::rename(&tmp, &dst)
        .or_else(|_| {
            if dst.exists() {
                fs::remove_dir_all(&dst).ok();
            }
            fs::rename(&tmp, &dst)
        })
        .with_context(|| format!("finalize {}", dst.display()))?;
    Ok((dst, version))
}

pub fn materialize_module(
    cache_root: &Path,
    module: &str,
    source_url: &str,
    requested: &str,
    locked_rev: Option<&str>,
    allow_fetch: bool,
) -> anyhow::Result<(PathBuf, String)> {
    match parse_source_backend(source_url) {
        SourceBackend::Git(url) => {
            let mirror = ensure_mirror(cache_root, &url, allow_fetch)?;
            let rev = if let Some(r) = locked_rev {
                r.to_string()
            } else {
                resolve_rev(&mirror, requested)?
            };
            let root = checkout_module(cache_root, module, &rev, &mirror)?;
            Ok((root, rev))
        }
        SourceBackend::Proxy(base) => materialize_proxy_module(
            cache_root,
            module,
            &base,
            requested,
            locked_rev,
            allow_fetch,
        ),
    }
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
    if let Some(req) = parse_version_req_loose(requested) {
        let tags = list_semver_tags(mirror)?;
        let mut best: Option<(Version, String)> = None;
        for (ver, tag) in tags {
            if req.matches(&ver) {
                match &best {
                    Some((bver, _)) if ver <= *bver => {}
                    _ => best = Some((ver, tag)),
                }
            }
        }
        if let Some((_, tag)) = best {
            return run_git(
                &[
                    "-C",
                    mirror.to_str().unwrap(),
                    "rev-parse",
                    &format!("refs/tags/{}^{{commit}}", tag),
                ],
                None,
            );
        }
        bail!("no matching semver tag for requirement `{}`", requested);
    }
    if requested.starts_with('v')
        && let Ok(full) = run_git(
            &[
                "-C",
                mirror.to_str().unwrap(),
                "rev-parse",
                &format!("refs/tags/{}^{{commit}}", requested),
            ],
            None,
        )
    {
        return Ok(full);
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
        let cur =
            run_git(&["-C", dst.to_str().unwrap(), "rev-parse", "HEAD"], None).unwrap_or_default();
        if cur == rev {
            return Ok(dst);
        }
        fs::remove_dir_all(&dst).ok();
    }

    let _ = run_git(
        &["clone", mirror.to_str().unwrap(), dst.to_str().unwrap()],
        None,
    )?;
    let _ = run_git(
        &["-C", dst.to_str().unwrap(), "checkout", "--force", rev],
        None,
    )?;
    Ok(dst)
}

#[cfg(test)]
mod tests {
    use super::{
        is_mvs_compatible, materialize_module, merge_requested_versions, requirement_min_version,
    };
    use semver::Version;
    use std::fs;
    use std::io::Write;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};
    use zip::write::SimpleFileOptions;

    fn temp_dir(prefix: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time drift")
            .as_nanos();
        std::env::temp_dir().join(format!("gost-{}-{}-{}", prefix, std::process::id(), nonce))
    }

    #[test]
    fn requirement_min_version_parses_v_prefix_and_ranges() {
        assert_eq!(
            requirement_min_version("v1.2.3").expect("min for exact"),
            Version::new(1, 2, 3)
        );
        assert_eq!(
            requirement_min_version("^v1.2").expect("min for caret"),
            Version::new(1, 2, 0)
        );
        assert_eq!(
            requirement_min_version(">=v1.4, <2.0").expect("min for comparator range"),
            Version::new(1, 4, 0)
        );
    }

    #[test]
    fn merge_requested_versions_prefers_higher_semver_floor() {
        assert_eq!(merge_requested_versions("v1.2.0", "^1.3"), "^1.3");
        assert_eq!(merge_requested_versions("^1.3", "v1.2.0"), "^1.3");
        assert_eq!(merge_requested_versions("^1.2", "^1.2"), "^1.2");
    }

    #[test]
    fn mvs_compatibility_checks_minimum_bounds() {
        assert!(is_mvs_compatible("^1.2", "^1.3"));
        assert!(is_mvs_compatible("v1.2.0", "^1.2"));
        assert!(!is_mvs_compatible("^1.3", "^1.2"));
        assert!(!is_mvs_compatible("main", "^1.2"));
    }

    #[test]
    fn materialize_module_supports_file_proxy_semver_selection() {
        let base = temp_dir("proxy");
        let cache = temp_dir("cache");
        let module = "example.com/acme/lib";
        let vdir = base.join(module).join("@v");
        fs::create_dir_all(&vdir).expect("create proxy dir");
        fs::write(vdir.join("list"), "v1.2.0\nv1.3.1\n").expect("write list");

        let mut zip = zip::ZipWriter::new(std::io::Cursor::new(Vec::<u8>::new()));
        let opts = SimpleFileOptions::default();
        zip.start_file("gost.mod", opts).expect("start gost.mod");
        zip.write_all(b"module = \"example.com/acme/lib\"\n")
            .expect("write gost.mod");
        let bytes = zip.finish().expect("finish zip").into_inner();
        fs::write(vdir.join("v1.3.1.zip"), bytes).expect("write zip");

        let source = format!("proxy+file://{}", base.to_string_lossy().replace('\\', "/"));
        let (root, rev) =
            materialize_module(&cache, module, &source, "^1.2", None, true).expect("materialize");
        assert_eq!(rev, "v1.3.1");
        assert!(root.join("gost.mod").exists());

        let _ = fs::remove_dir_all(base);
        let _ = fs::remove_dir_all(cache);
    }

    #[test]
    fn materialize_module_strips_common_prefix_from_proxy_zip_entries() {
        let base = temp_dir("proxy-prefix");
        let cache = temp_dir("cache-prefix");
        let module = "example.com/acme/lib";
        let vdir = base.join(module).join("@v");
        fs::create_dir_all(&vdir).expect("create proxy dir");
        fs::write(vdir.join("list"), "v1.4.0\n").expect("write list");

        let mut zip = zip::ZipWriter::new(std::io::Cursor::new(Vec::<u8>::new()));
        let opts = SimpleFileOptions::default();
        let prefix = "example.com/acme/lib@v1.4.0";
        zip.start_file(format!("{}/gost.mod", prefix), opts)
            .expect("start gost.mod");
        zip.write_all(b"module = \"example.com/acme/lib\"\n")
            .expect("write gost.mod");
        zip.start_file(format!("{}/src/p.gs", prefix), opts)
            .expect("start source");
        zip.write_all(b"module lib\n").expect("write source");
        let bytes = zip.finish().expect("finish zip").into_inner();
        fs::write(vdir.join("v1.4.0.zip"), bytes).expect("write zip");

        let source = format!("proxy+file://{}", base.to_string_lossy().replace('\\', "/"));
        let (root, rev) =
            materialize_module(&cache, module, &source, "v1.4.0", None, true).expect("materialize");
        assert_eq!(rev, "v1.4.0");
        assert!(root.join("gost.mod").exists());
        assert!(root.join("src").join("p.gs").exists());

        let _ = fs::remove_dir_all(base);
        let _ = fs::remove_dir_all(cache);
    }
}
