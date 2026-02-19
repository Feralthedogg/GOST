// Purpose: Model and serialize/deserialize gost.lock dependency lock state.
// Inputs/Outputs: Converts lock JSON to strongly-typed structures and back.
// Invariants: Schema/version handling must remain deterministic and backward-safe.
// Gotchas: Field renames break reproducibility and should be version-gated.

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockFile {
    pub schema: u32,
    pub main: LockMain,
    pub modules: Vec<LockedModule>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockMain {
    pub module: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockedModule {
    pub module: String,
    pub source: String,
    pub requested: String,
    pub rev: String,
    #[serde(default)]
    pub checksum: Option<String>,
    #[serde(default)]
    pub local: Option<String>,
}

impl LockFile {
    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn empty(main_module: &str) -> Self {
        Self {
            schema: 1,
            main: LockMain {
                module: main_module.to_string(),
            },
            modules: vec![],
        }
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn get(&self, module: &str) -> Option<&LockedModule> {
        self.modules.iter().find(|m| m.module == module)
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn upsert(&mut self, m: LockedModule) {
        if let Some(pos) = self.modules.iter().position(|x| x.module == m.module) {
            self.modules[pos] = m;
        } else {
            self.modules.push(m);
        }
    }
}
