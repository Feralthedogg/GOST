// Purpose: Scan source trees to collect import declarations for module workflows.
// Inputs/Outputs: Consumes file paths/source text and returns normalized import paths.
// Invariants: Scanner must be conservative and avoid false negatives for dependency analysis.
// Gotchas: Parsing shortcuts must stay aligned with supported import syntax variants.

use regex::Regex;
use std::collections::HashSet;
use std::fs;
use std::path::Path;

pub fn scan_imports_in_text(src: &str) -> HashSet<String> {
    let re = Regex::new(r#"(?m)^\s*import\s+"([^"]+)""#).unwrap();
    re.captures_iter(src)
        .filter_map(|c| c.get(1).map(|m| m.as_str().to_string()))
        .collect()
}

pub fn scan_imports_in_file(p: &Path) -> anyhow::Result<HashSet<String>> {
    let s = fs::read_to_string(p)?;
    Ok(scan_imports_in_text(&s))
}
