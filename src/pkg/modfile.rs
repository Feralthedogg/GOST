// Purpose: Parse and write gost.mod definitions and dependency directives.
// Inputs/Outputs: Converts TOML module config into typed structures for resolver use.
// Invariants: Parsed model must preserve semantic intent of require/replace/source directives.
// Gotchas: Formatting/order changes can affect diffs; keep writer deterministic.

use serde::Deserialize;

#[derive(Debug, Clone, Deserialize)]
pub struct ModFile {
    pub module: String,
    #[serde(default)]
    pub require: Vec<Require>,
    #[serde(default)]
    pub replace: Vec<Replace>,
    #[serde(default)]
    pub source: Vec<Source>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Require {
    pub module: String,
    pub version: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Replace {
    pub module: String,
    pub path: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Source {
    pub module: String,
    pub url: String,
}

impl ModFile {
    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn parse(toml_text: &str) -> anyhow::Result<Self> {
        Ok(toml::from_str::<ModFile>(toml_text)?)
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn to_pretty_toml(&self) -> String {
        let mut out = String::new();
        out.push_str(&format!("module = {:?}\n", self.module));

        if !self.require.is_empty() {
            out.push('\n');
            for r in &self.require {
                out.push_str("[[require]]\n");
                out.push_str(&format!("module = {:?}\n", r.module));
                out.push_str(&format!("version = {:?}\n\n", r.version));
            }
        }

        if !self.replace.is_empty() {
            out.push('\n');
            for r in &self.replace {
                out.push_str("[[replace]]\n");
                out.push_str(&format!("module = {:?}\n", r.module));
                out.push_str(&format!("path = {:?}\n\n", r.path));
            }
        }

        if !self.source.is_empty() {
            out.push('\n');
            for s in &self.source {
                out.push_str("[[source]]\n");
                out.push_str(&format!("module = {:?}\n", s.module));
                out.push_str(&format!("url = {:?}\n\n", s.url));
            }
        }

        if !out.ends_with('\n') {
            out.push('\n');
        }
        out
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn sort_deterministic(&mut self) {
        self.require.sort_by(|a, b| a.module.cmp(&b.module));
        self.replace.sort_by(|a, b| a.module.cmp(&b.module));
        self.source.sort_by(|a, b| a.module.cmp(&b.module));
    }
}
