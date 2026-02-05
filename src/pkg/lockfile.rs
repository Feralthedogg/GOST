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
    pub fn empty(main_module: &str) -> Self {
        Self {
            schema: 1,
            main: LockMain {
                module: main_module.to_string(),
            },
            modules: vec![],
        }
    }

    pub fn get(&self, module: &str) -> Option<&LockedModule> {
        self.modules.iter().find(|m| m.module == module)
    }

    pub fn upsert(&mut self, m: LockedModule) {
        if let Some(pos) = self.modules.iter().position(|x| x.module == m.module) {
            self.modules[pos] = m;
        } else {
            self.modules.push(m);
        }
    }
}
