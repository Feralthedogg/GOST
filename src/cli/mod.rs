// Purpose: Parse CLI commands/options and orchestrate compile/run/mod subcommands.
// Inputs/Outputs: Consumes argv/environment and returns user-facing exit codes/messages.
// Invariants: Option precedence and policy flags (offline/online/mod mode) are centralized here.
// Gotchas: Keep help text, parser behavior, and default mode policy in sync.

use std::borrow::Cow;
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use crate::incremental::{self, trace as inc_trace};
use crate::pkg::resolve::ModMode;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CliMode {
    Compile,
    Run,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum OptLevel {
    O0,
    O1,
    O2,
    O3,
}

impl OptLevel {
    fn as_digit(self) -> u8 {
        match self {
            Self::O0 => 0,
            Self::O1 => 1,
            Self::O2 => 2,
            Self::O3 => 3,
        }
    }

    fn from_level_value(raw: &str) -> Option<Self> {
        let v = raw.trim().to_ascii_lowercase();
        match v.as_str() {
            "0" | "o0" => Some(Self::O0),
            "1" | "o1" | "o" => Some(Self::O1),
            "2" | "o2" => Some(Self::O2),
            "3" | "o3" => Some(Self::O3),
            _ => None,
        }
    }

    fn from_flag(arg: &str) -> Option<Self> {
        match arg {
            "-O" => Some(Self::O1),
            "-O0" | "-o0" => Some(Self::O0),
            "-O1" | "-o1" => Some(Self::O1),
            "-O2" | "-o2" => Some(Self::O2),
            "-O3" | "-o3" => Some(Self::O3),
            _ => None,
        }
    }

    fn clang_flag(self) -> String {
        format!("-O{}", self.as_digit())
    }

    fn llc_flag(self) -> String {
        format!("-O={}", self.as_digit())
    }

    fn is_optimized(self) -> bool {
        !matches!(self, Self::O0)
    }
}

fn default_opt_level() -> OptLevel {
    if let Ok(v) = std::env::var("GOST_OPT_LEVEL")
        && let Some(level) = OptLevel::from_level_value(&v)
    {
        return level;
    }
    if let Ok(v) = std::env::var("GOST_OPT")
        && let Some(level) = OptLevel::from_level_value(&v)
    {
        return level;
    }
    let release = std::env::var("GOST_RELEASE")
        .map(|v| v != "0")
        .unwrap_or(true);
    if release { OptLevel::O3 } else { OptLevel::O0 }
}

pub fn run_cli<I>(args: I) -> i32
where
    I: IntoIterator<Item = String>,
{
    let exe = std::env::args()
        .next()
        .unwrap_or_default()
        .to_ascii_lowercase();
    let default_mode = if exe.ends_with("gs.exe") || exe.ends_with("gs") {
        CliMode::Run
    } else {
        CliMode::Compile
    };
    run_cli_with_mode(args, default_mode)
}

// Precondition: `args` must include executable name as the first element.
// Postcondition: Returns normalized process exit code for all CLI paths.
// Side effects: Reads environment variables, prints diagnostics/help, and may invoke build/run flows.
pub fn run_cli_with_mode<I>(args: I, default_mode: CliMode) -> i32
where
    I: IntoIterator<Item = String>,
{
    let mut args = args.into_iter().peekable();
    let mut global_offline = false;
    let mut global_online = false;
    let mut first = match args.next() {
        Some(arg) => arg,
        None => {
            print_help_global();
            return 1;
        }
    };
    loop {
        if first == "--offline" {
            global_offline = true;
        } else if first == "--online" {
            global_online = true;
        } else if let Some(code) = first.strip_prefix("--explain=") {
            return explain_code(code);
        } else {
            break;
        }
        first = match args.next() {
            Some(arg) => arg,
            None => {
                print_help_global();
                return 1;
            }
        };
    }
    if first == "--help" || first == "-h" || first == "help" {
        print_help_global();
        return 0;
    }
    if first == "--version" || first == "-V" || first == "version" {
        println!("gost {}", env!("CARGO_PKG_VERSION"));
        return 0;
    }
    if first == "--explain" || first == "explain" {
        let code = match args.next() {
            Some(arg) => arg,
            None => {
                eprintln!("error: expected diagnostic code after --explain");
                return 1;
            }
        };
        return explain_code(&code);
    }
    if first == "lsp" {
        let mut lsp_help = false;
        for arg in args.by_ref() {
            if arg == "--help" || arg == "-h" {
                lsp_help = true;
                continue;
            }
            eprintln!("unknown argument: {}", arg);
            return 1;
        }
        if lsp_help {
            print_help_lsp();
            return 0;
        }
        return run_external_lsp();
    }
    if first == "mod" {
        let sub = match args.next() {
            Some(arg) => arg,
            None => {
                print_help_mod();
                return 1;
            }
        };
        if sub == "--help" || sub == "-h" || sub == "help" {
            print_help_mod();
            return 0;
        }
        let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        let mut mod_mode = ModMode::Mod;
        let mut offline = false;
        let mut online = false;
        let mut module_arg: Option<String> = None;
        let mut unknown: Vec<String> = Vec::new();

        let mut sub_help = false;
        for arg in args.by_ref() {
            if arg == "--help" || arg == "-h" {
                sub_help = true;
                continue;
            }
            if arg == "--offline" {
                offline = true;
            } else if arg == "--online" {
                online = true;
            } else if arg == "--readonly" || arg == "-mod=readonly" {
                mod_mode = ModMode::Readonly;
            } else if let Some(val) = arg.strip_prefix("-mod=") {
                mod_mode = match val {
                    "mod" => ModMode::Mod,
                    "readonly" => ModMode::Readonly,
                    _ => {
                        eprintln!("invalid -mod value: {}", val);
                        return 1;
                    }
                };
            } else if module_arg.is_none() && sub == "init" {
                module_arg = Some(arg);
            } else {
                unknown.push(arg);
            }
        }
        if sub_help {
            match sub.as_str() {
                "init" => print_help_mod_init(),
                "tidy" => print_help_mod_tidy(),
                "verify" => print_help_mod_verify(),
                "download" => print_help_mod_download(),
                "graph" => print_help_mod_graph(),
                _ => print_help_mod(),
            }
            return 0;
        }
        if offline && online {
            eprintln!(
                "error: conflicting flags: --offline disables network, so --online can't be used (remove --offline)."
            );
            return 1;
        }
        if global_offline && online {
            eprintln!(
                "error: --offline is set: network is disabled (remove --offline to use --online)."
            );
            return 1;
        }
        if global_online {
            online = true;
        }
        offline = offline || global_offline;
        if !unknown.is_empty() {
            eprintln!("unknown arguments: {}", unknown.join(" "));
            return 1;
        }
        let result = match sub.as_str() {
            "init" => crate::pkg::modcmd::cmd_init(cwd, module_arg),
            "tidy" => crate::pkg::modcmd::cmd_tidy(cwd, mod_mode, offline),
            "download" => crate::pkg::modcmd::cmd_download(cwd, offline, online),
            "verify" => crate::pkg::modcmd::cmd_verify(cwd, online, offline),
            "graph" => crate::pkg::modcmd::cmd_graph(cwd, offline),
            _ => {
                print_help_mod();
                return 1;
            }
        };
        if let Err(err) = result {
            eprintln!("{}", err);
            return 1;
        }
        return 0;
    }
    if first == "bindgen" {
        let mut header: Option<PathBuf> = None;
        let mut out: Option<PathBuf> = None;
        let mut module_name: Option<String> = None;
        let mut abi = "C".to_string();
        let mut bindgen_help = false;
        while let Some(arg) = args.next() {
            if arg == "--help" || arg == "-h" {
                bindgen_help = true;
                continue;
            }
            if arg == "-o" {
                let Some(path) = args.next() else {
                    eprintln!("error: expected output path after -o");
                    return 1;
                };
                out = Some(PathBuf::from(path));
                continue;
            }
            if arg == "--module" {
                let Some(name) = args.next() else {
                    eprintln!("error: expected module name after --module");
                    return 1;
                };
                module_name = Some(name);
                continue;
            }
            if arg == "--abi" {
                let Some(name) = args.next() else {
                    eprintln!("error: expected ABI name after --abi");
                    return 1;
                };
                abi = name;
                continue;
            }
            if header.is_none() {
                header = Some(PathBuf::from(arg));
            } else {
                eprintln!("unknown argument: {}", arg);
                return 1;
            }
        }
        if bindgen_help {
            print_help_bindgen();
            return 0;
        }
        let Some(header) = header else {
            print_help_bindgen();
            return 1;
        };
        let stem = header
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("bindings");
        let module_name = module_name.unwrap_or_else(|| sanitize_module_name(stem));
        let out = out.unwrap_or_else(|| PathBuf::from(format!("{}.gs", stem)));
        if let Err(err) = run_bindgen(&header, &out, &module_name, &abi) {
            eprintln!("error: {}", err);
            return 1;
        }
        println!("generated {}", out.display());
        return 0;
    }
    let (mode, input) = if first == "run" {
        let next = match args.next() {
            Some(arg) => arg,
            None => {
                print_help_run();
                return 1;
            }
        };
        if next == "--help" || next == "-h" {
            print_help_run();
            return 0;
        }
        (CliMode::Run, next)
    } else if first == "build" {
        let next = match args.next() {
            Some(arg) => arg,
            None => {
                print_help_build();
                return 1;
            }
        };
        if next == "--help" || next == "-h" {
            print_help_build();
            return 0;
        }
        (CliMode::Compile, next)
    } else {
        if first == "--help" || first == "-h" {
            print_help_global();
            return 0;
        }
        (default_mode, first)
    };
    let mut output = None;
    let mut opt_level = default_opt_level();
    let mut mod_mode = if mode == CliMode::Run {
        ModMode::Mod
    } else {
        ModMode::Readonly
    };
    let mut offline = false;
    while let Some(arg) = args.next() {
        if arg == "-o" {
            let next = args.peek().map(|s| s.as_str());
            if next.is_none() || next.is_some_and(|s| s.starts_with('-')) {
                opt_level = OptLevel::O1;
                continue;
            }
            if let Some(level) = next.and_then(OptLevel::from_level_value) {
                let _ = args.next();
                opt_level = level;
                continue;
            }
            let Some(path) = args.next() else {
                eprintln!("expected output after -o");
                return 1;
            };
            output = Some(PathBuf::from(path));
        } else if arg == "--output" {
            match args.next() {
                Some(path) => output = Some(PathBuf::from(path)),
                None => {
                    eprintln!("expected output after --output");
                    return 1;
                }
            }
        } else if arg == "--opt" {
            let Some(level_raw) = args.next() else {
                eprintln!("expected optimization level after --opt (0|1|2|3)");
                return 1;
            };
            let Some(level) = OptLevel::from_level_value(&level_raw) else {
                eprintln!(
                    "invalid optimization level `{}` (expected 0|1|2|3)",
                    level_raw
                );
                return 1;
            };
            opt_level = level;
        } else if let Some(level_raw) = arg.strip_prefix("--opt=") {
            let Some(level) = OptLevel::from_level_value(level_raw) else {
                eprintln!(
                    "invalid optimization level `{}` (expected 0|1|2|3)",
                    level_raw
                );
                return 1;
            };
            opt_level = level;
        } else if let Some(level) = OptLevel::from_flag(&arg) {
            opt_level = level;
        } else if arg == "--offline" {
            offline = true;
        } else if let Some(val) = arg.strip_prefix("-mod=") {
            match val {
                "mod" => mod_mode = ModMode::Mod,
                "readonly" => mod_mode = ModMode::Readonly,
                _ => {
                    eprintln!("invalid -mod value: {}", val);
                    return 1;
                }
            }
        } else if let Some(val) = arg.strip_prefix("--mod=") {
            match val {
                "mod" => mod_mode = ModMode::Mod,
                "readonly" => mod_mode = ModMode::Readonly,
                _ => {
                    eprintln!("invalid --mod value: {}", val);
                    return 1;
                }
            }
        } else {
            eprintln!("unknown argument: {}", arg);
            return 1;
        }
    }
    if global_online {
        eprintln!("error: --online is only supported for 'gs mod download' and 'gs mod verify'.");
        return 1;
    }
    offline = offline || global_offline;
    let input_path = PathBuf::from(&input);
    if input_path.extension().and_then(|s| s.to_str()) != Some("gs") {
        eprintln!("expected .gs source file");
        return 1;
    }
    let want_fetch = matches!(mod_mode, ModMode::Mod);
    let llvm = match crate::compile::compile_to_llvm(&input_path, mod_mode, offline, want_fetch) {
        Ok(text) => text,
        Err(err) => {
            eprintln!("{}", err);
            return 1;
        }
    };
    let ll_path = output.unwrap_or_else(|| input_path.with_extension("ll"));
    if let Err(err) = std::fs::write(&ll_path, llvm) {
        eprintln!("failed to write {}: {}", ll_path.display(), err);
        return 1;
    }
    if mode == CliMode::Run
        && let Err(err) = link_and_run(&input_path, &ll_path, opt_level)
    {
        eprintln!("{}", err);
        return 1;
    }
    0
}

fn print_help_global() {
    eprintln!("gost script runner / builder");
    eprintln!();
    eprintln!("USAGE:");
    eprintln!("  gs [OPTIONS] <COMMAND>");
    eprintln!();
    eprintln!("COMMANDS:");
    eprintln!("  run       Run a .gs entry file");
    eprintln!("  build     Build a .gs entry file");
    eprintln!("  lsp       Run Gost language server over stdio");
    eprintln!("  mod       Module/dependency management");
    eprintln!("  bindgen   Generate gost extern bindings from C headers");
    eprintln!("  explain   Show detailed explanation for a diagnostic code");
    eprintln!("  help      Print this message or the help of the given subcommand(s)");
    eprintln!();
    eprintln!("GLOBAL OPTIONS:");
    eprintln!(
        "  --offline         Disallow any network access (git fetch/clone). Highest priority."
    );
    eprintln!("  -V, --version     Print version information");
    eprintln!("  -h, --help        Print help information");
    eprintln!();
    eprintln!("POLICY:");
    eprintln!("  - --offline is global and overrides subcommand-specific online behavior.");
    eprintln!("  - Using --offline together with --online is an error.");
    eprintln!(
        "  - Network access (fetch/clone) is only allowed when explicitly enabled by subcommands that support it."
    );
    eprintln!();
    eprintln!("EXAMPLES:");
    eprintln!("  gs run main.gs");
    eprintln!("  gs run main.gs -mod=readonly --offline");
    eprintln!("  gs build main.gs");
    eprintln!("  gs mod init github.com/you/project");
    eprintln!("  gs mod tidy");
    eprintln!("  gs mod verify");
    eprintln!("  gs mod verify --online");
    eprintln!("  gs mod download");
    eprintln!("  gs mod download --online");
    eprintln!("  gs mod graph");
    eprintln!("  gs bindgen native.h -o native.gs --module native");
    eprintln!("  gs lsp");
    eprintln!("  gs explain E1101");
    eprintln!("  gs explain mono-depth");
    eprintln!("  gs --explain E1101");
}

fn print_help_bindgen() {
    eprintln!("Generate Gost extern bindings from a C header.");
    eprintln!();
    eprintln!("USAGE:");
    eprintln!("  gs bindgen [OPTIONS] <header.h>");
    eprintln!();
    eprintln!("OPTIONS:");
    eprintln!("  -o <file.gs>         Output file path (default: <header-stem>.gs)");
    eprintln!("  --module <name>      Module name for generated file (default: header stem)");
    eprintln!("  --abi <name>         Extern ABI string (default: C)");
    eprintln!("  -h, --help           Print help");
}

fn print_help_lsp() {
    eprintln!("Run Gost Language Server Protocol endpoint over stdio.");
    eprintln!();
    eprintln!("USAGE:");
    eprintln!("  gs lsp");
    eprintln!();
    eprintln!("OPTIONS:");
    eprintln!("  -h, --help        Print help");
    eprintln!();
    eprintln!("ENV:");
    eprintln!("  GOST_LSP_BIN      Path to gost-lsp executable (optional)");
    eprintln!();
    eprintln!("NOTES:");
    eprintln!("  - Launches external `gost-lsp` process and proxies stdio.");
    eprintln!("  - Use with extensions that send LSP JSON-RPC over stdin/stdout.");
}

#[cfg(windows)]
const LSP_BIN_NAME: &str = "gost-lsp.exe";
#[cfg(not(windows))]
const LSP_BIN_NAME: &str = "gost-lsp";

fn run_external_lsp() -> i32 {
    if let Some(bin) = std::env::var_os("GOST_LSP_BIN") {
        let mut cmd = Command::new(&bin);
        return match run_lsp_command(&mut cmd) {
            Ok(code) => code,
            Err(err) => {
                eprintln!(
                    "error: failed to launch GOST_LSP_BIN `{}`: {}",
                    PathBuf::from(bin).display(),
                    err
                );
                1
            }
        };
    }

    let mut last_error: Option<String> = None;
    for candidate in lsp_binary_candidates() {
        let mut cmd = Command::new(&candidate);
        match run_lsp_command(&mut cmd) {
            Ok(code) => return code,
            Err(err) => {
                last_error = Some(format!("{} ({})", err, candidate.display()));
            }
        }
    }

    let mut cmd = Command::new("gost-lsp");
    match run_lsp_command(&mut cmd) {
        Ok(code) => code,
        Err(err) => {
            eprintln!("error: failed to launch `gost-lsp`: {}", err);
            if let Some(prev) = last_error {
                eprintln!("note: also tried candidate binaries: {}", prev);
            }
            eprintln!("hint: build/install gost-lsp or set GOST_LSP_BIN to its executable path.");
            1
        }
    }
}

fn lsp_binary_candidates() -> Vec<PathBuf> {
    let mut out = Vec::<PathBuf>::new();

    if let Ok(exe) = std::env::current_exe()
        && let Some(dir) = exe.parent()
    {
        out.push(dir.join(LSP_BIN_NAME));
    }

    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    if let Some(projects_dir) = manifest.parent() {
        let lsp_dir = projects_dir.join("gost-lsp");
        out.push(lsp_dir.join("target").join("debug").join(LSP_BIN_NAME));
        out.push(lsp_dir.join("target").join("release").join(LSP_BIN_NAME));
    }

    out.retain(|path| path.is_file());
    out.sort();
    out.dedup();
    out
}

fn run_lsp_command(cmd: &mut Command) -> Result<i32, String> {
    let status = cmd
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .map_err(|e| e.to_string())?;
    Ok(status.code().unwrap_or(1))
}

fn explain_code(code: &str) -> i32 {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("docs")
        .join("diagnostics.md");
    let Ok(text) = std::fs::read_to_string(&path) else {
        eprintln!("error: failed to read {}", path.display());
        return 1;
    };
    let target = normalize_explain_key(code.trim());
    if target.is_empty() {
        eprintln!("error: expected diagnostic code");
        return 1;
    }

    let mut out = String::new();
    let mut in_section = false;
    for line in text.lines() {
        if let Some(rest) = line.strip_prefix("## ") {
            let title = rest.trim();
            if in_section {
                break;
            }
            if title.eq_ignore_ascii_case(&target) {
                in_section = true;
                out.push_str(line);
                out.push('\n');
                continue;
            }
        }
        if in_section {
            out.push_str(line);
            out.push('\n');
        }
    }
    if in_section {
        print!("{}", out);
        return 0;
    }

    for line in text.lines() {
        let l = line.trim_start();
        if l.starts_with("- ")
            && l.to_ascii_lowercase()
                .contains(&target.to_ascii_lowercase())
        {
            println!("{}:", target);
            println!("{}", l.trim_start_matches("- ").trim());
            println!();
            println!("note: no detailed explanation section found for {}", target);
            return 0;
        }
    }

    eprintln!("error: no explanation found for {}", target);
    1
}

fn normalize_explain_key(s: &str) -> String {
    let t = s.trim().to_ascii_lowercase();
    match t.as_str() {
        "unknown-field" | "field" => "E1101".to_string(),
        "unknown-method" | "method" => "E1102".to_string(),
        "unknown-variant" | "variant" => "E1103".to_string(),
        "unknown-name" | "undefined-name" | "name" => "E1002".to_string(),
        "unknown-type" | "type" => "E1003".to_string(),
        "monomorphization-depth" | "mono-depth" | "generic-depth" => "E1201".to_string(),
        "import-not-found" | "import" => "E2001".to_string(),
        "package-empty" | "empty-package" => "E2002".to_string(),
        _ => s.trim().to_string(),
    }
}

fn print_help_run() {
    eprintln!("Run a .gs entry file.");
    eprintln!();
    eprintln!("USAGE:");
    eprintln!("  gs run [OPTIONS] <file.gs>");
    eprintln!();
    eprintln!("ARGS:");
    eprintln!("  <file.gs>    Entry file (.gs)");
    eprintln!();
    eprintln!("OPTIONS:");
    eprintln!("  -O, -O0..-O3      LLVM optimization level [default: O3] (-O == -O1)");
    eprintln!("  -o0..-o3          Lowercase aliases for optimization level");
    eprintln!("  --opt <0..3>      Set optimization level");
    eprintln!("  --output <file>   LLVM IR output path (default: <file>.ll)");
    eprintln!("  -o <file>         Same as --output");
    eprintln!("  -mod <mode>        Module mode: mod | readonly");
    eprintln!("                     [default: mod]");
    eprintln!("                     - mod:     may update gost.lock if needed");
    eprintln!("                     - readonly: errors if gost.lock is missing or mismatched");
    eprintln!();
    eprintln!("GLOBAL OPTIONS:");
    eprintln!("  --offline           Disallow any network access (git fetch/clone).");
    eprintln!();
    eprintln!("NOTES:");
    eprintln!("  - In readonly mode, gost.mod/gost.lock must be consistent.");
    eprintln!(
        "  - If no gost.mod is found, legacy import resolution is used (no module fetching)."
    );
    eprintln!();
    eprintln!("EXAMPLES:");
    eprintln!("  gs run main.gs");
    eprintln!("  gs run main.gs -mod=readonly");
    eprintln!("  gs run main.gs -mod=readonly --offline");
}

fn print_help_build() {
    eprintln!("Build a .gs entry file.");
    eprintln!();
    eprintln!("USAGE:");
    eprintln!("  gs build [OPTIONS] <file.gs>");
    eprintln!();
    eprintln!("ARGS:");
    eprintln!("  <file.gs>    Entry file (.gs)");
    eprintln!();
    eprintln!("OPTIONS:");
    eprintln!("  -O, -O0..-O3      LLVM optimization level [default: O3] (-O == -O1)");
    eprintln!("  -o0..-o3          Lowercase aliases for optimization level");
    eprintln!("  --opt <0..3>      Set optimization level");
    eprintln!("  --output <file>   LLVM IR output path (default: <file>.ll)");
    eprintln!("  -o <file>         Same as --output");
    eprintln!("  -mod <mode>        Module mode: mod | readonly");
    eprintln!("                     [default: readonly]");
    eprintln!("                     - mod:     may update gost.lock if needed");
    eprintln!("                     - readonly: errors if gost.lock is missing or mismatched");
    eprintln!();
    eprintln!("GLOBAL OPTIONS:");
    eprintln!("  --offline           Disallow any network access (git fetch/clone).");
    eprintln!();
    eprintln!("EXAMPLES:");
    eprintln!("  gs build main.gs");
    eprintln!("  gs build main.gs --offline");
    eprintln!("  gs build main.gs -mod=mod");
}

fn print_help_mod() {
    eprintln!("Module/dependency management.");
    eprintln!();
    eprintln!("USAGE:");
    eprintln!("  gs mod <SUBCOMMAND> [OPTIONS]");
    eprintln!();
    eprintln!("SUBCOMMANDS:");
    eprintln!("  init        Create gost.mod (and initial gost.lock)");
    eprintln!("  tidy        Scan project, update require list, optionally refresh lock");
    eprintln!("  verify      Verify mod/lock consistency + resolve check (default offline)");
    eprintln!("  download    Prefetch module cache (default cache-only)");
    eprintln!("  graph       Print module graph from gost.mod + gost.lock");
    eprintln!("  help        Print this message or the help of the given subcommand(s)");
    eprintln!();
    eprintln!("GLOBAL OPTIONS:");
    eprintln!("  --offline    Disallow any network access (git fetch/clone). Highest priority.");
    eprintln!("  -h, --help   Print help information");
    eprintln!();
    eprintln!("EXAMPLES:");
    eprintln!("  gs mod init github.com/you/project");
    eprintln!("  gs mod tidy");
    eprintln!("  gs mod tidy --readonly");
    eprintln!("  gs mod verify");
    eprintln!("  gs mod verify --online");
    eprintln!("  gs mod download");
    eprintln!("  gs mod download --online");
    eprintln!("  gs mod graph");
}

fn print_help_mod_init() {
    eprintln!("Create gost.mod (and initial gost.lock).");
    eprintln!();
    eprintln!("USAGE:");
    eprintln!("  gs mod init [module]");
    eprintln!();
    eprintln!("ARGS:");
    eprintln!("  [module]    Module path (e.g. github.com/you/project).");
    eprintln!(
        "              If omitted, gs tries to infer from git origin, otherwise uses a placeholder."
    );
    eprintln!();
    eprintln!("GLOBAL OPTIONS:");
    eprintln!(
        "  --offline    Disallow any network access (not used by init, but accepted globally)."
    );
    eprintln!();
    eprintln!("EXAMPLES:");
    eprintln!("  gs mod init");
    eprintln!("  gs mod init github.com/you/project");
}

fn print_help_mod_tidy() {
    eprintln!("Scan project .gs files, infer required modules, and update gost.mod.");
    eprintln!("Optionally refresh gost.lock in mod mode.");
    eprintln!();
    eprintln!("USAGE:");
    eprintln!("  gs mod tidy [OPTIONS]");
    eprintln!();
    eprintln!("OPTIONS:");
    eprintln!(
        "  --readonly     Do not modify files; only verify that requirements/lock are consistent."
    );
    eprintln!("  --offline      Disallow any network access (git fetch/clone).");
    eprintln!();
    eprintln!("GLOBAL OPTIONS:");
    eprintln!("  --offline      (same as above; global)");
    eprintln!();
    eprintln!("BEHAVIOR:");
    eprintln!(
        "  - mod mode:    may update gost.mod and refresh gost.lock (network may be needed unless offline)"
    );
    eprintln!(
        "  - readonly:    never mutates; fails if required modules are missing or lock mismatched"
    );
    eprintln!();
    eprintln!("EXAMPLES:");
    eprintln!("  gs mod tidy");
    eprintln!("  gs mod tidy --readonly");
    eprintln!("  gs mod tidy --offline");
    eprintln!("  gs mod tidy --readonly --offline");
}

fn print_help_mod_verify() {
    eprintln!("Verify module state (default offline).");
    eprintln!();
    eprintln!("USAGE:");
    eprintln!("  gs mod verify [OPTIONS]");
    eprintln!();
    eprintln!("OPTIONS:");
    eprintln!(
        "  --online      Allow network access for resolve check (git fetch/clone) if cache is missing."
    );
    eprintln!("  --offline     Force offline (explicit; also the default).");
    eprintln!();
    eprintln!("GLOBAL OPTIONS:");
    eprintln!("  --offline     Disallow any network access. Overrides --online.");
    eprintln!();
    eprintln!("BEHAVIOR (2-phase):");
    eprintln!("  1) strict readonly tidy verification (no edits)");
    eprintln!("  2) resolve check:");
    eprintln!("     - default: offline (cache-only)");
    eprintln!(
        "     - --online: allows fetch/clone if needed (still readonly; does not write lock)"
    );
    eprintln!();
    eprintln!("EXAMPLES:");
    eprintln!("  gs mod verify");
    eprintln!("  gs mod verify --offline");
    eprintln!("  gs mod verify --online");
}

fn print_help_mod_download() {
    eprintln!("Prefetch module cache.");
    eprintln!();
    eprintln!("USAGE:");
    eprintln!("  gs mod download [OPTIONS]");
    eprintln!();
    eprintln!("OPTIONS:");
    eprintln!(
        "  --online      Allow network access (git fetch/clone) if cache/mirrors are missing."
    );
    eprintln!("                Without --online, download is cache-only (offline) by default.");
    eprintln!();
    eprintln!("GLOBAL OPTIONS:");
    eprintln!("  --offline     Disallow any network access. Overrides --online.");
    eprintln!();
    eprintln!("NOTES:");
    eprintln!("  - download never modifies gost.lock.");
    eprintln!("  - cache-only mode errors if required mirrors are missing.");
    eprintln!();
    eprintln!("EXAMPLES:");
    eprintln!("  gs mod download              # cache-only");
    eprintln!("  gs mod download --online     # allow fetch/clone");
    eprintln!("  gs mod download --offline    # same as cache-only");
}

fn print_help_mod_graph() {
    eprintln!("Print module graph using gost.mod + gost.lock.");
    eprintln!();
    eprintln!("USAGE:");
    eprintln!("  gs mod graph [OPTIONS]");
    eprintln!();
    eprintln!("OPTIONS:");
    eprintln!("  --offline     Default. Prints graph without any network access.");
    eprintln!(
        "  --online      Optional: may additionally show local cache status (if implemented),"
    );
    eprintln!("                but must not fetch/clone unless explicitly allowed by your policy.");
    eprintln!();
    eprintln!("GLOBAL OPTIONS:");
    eprintln!("  --offline     Disallow any network access. Overrides --online.");
    eprintln!();
    eprintln!("EXAMPLES:");
    eprintln!("  gs mod graph");
    eprintln!("  gs mod graph --offline");
    eprintln!("  gs mod graph --online");
}

// Precondition: `ll_path` points to valid LLVM IR produced by compile pipeline.
// Postcondition: Produces executable at input-derived path and runs it when linking succeeds.
// Side effects: Builds runtime artifacts, writes object/executable files, and executes subprocesses.
fn link_and_run(input_path: &Path, ll_path: &Path, opt_level: OptLevel) -> Result<(), String> {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let runtime_dir = manifest_dir.join("runtime");
    let cc = resolve_cc();
    ensure_native_compiler(&cc)?;
    let rt_lib = build_runtime(input_path, &runtime_dir, &cc, opt_level)?;
    let obj_path = input_path.with_extension("o");
    let exe_path = input_path.with_extension(if cfg!(windows) { "exe" } else { "" });
    if exe_path.exists() {
        let _ = std::fs::remove_file(&exe_path);
    }
    compile_ll_to_obj(input_path, &cc, ll_path, &obj_path, opt_level)?;
    let native = std::env::var("GOST_NATIVE")
        .map(|v| v != "0")
        .unwrap_or(true);
    let mut link_args = vec![obj_path.display().to_string(), rt_lib.display().to_string()];
    link_args.push(opt_level.clang_flag());
    if let Some(target) = macos_deployment_target() {
        link_args.push(format!("-mmacosx-version-min={}", target));
    }
    if opt_level.is_optimized() && native {
        link_args.push("-march=native".into());
        link_args.push("-mtune=native".into());
    }
    if is_gcc(&cc) {
        link_args.push("-pthread".into());
    } else {
        link_args.push("-lpthread".into());
    }
    if cfg!(windows) {
        // Rust std on windows-gnu expects these system libs at final link.
        link_args.push("-lntdll".into());
        link_args.push("-lws2_32".into());
        link_args.push("-lwsock32".into());
        link_args.push("-luserenv".into());
        link_args.push("-lbcrypt".into());
        link_args.push("-ladvapi32".into());
        link_args.push("-luser32".into());
        link_args.push("-lshell32".into());
        link_args.push("-lole32".into());
        link_args.push("-loleaut32".into());
        link_args.push("-lcomdlg32".into());
        link_args.push("-lgdi32".into());
        link_args.push("-lwinmm".into());
        link_args.push("-lsecur32".into());
    }
    if cfg!(target_os = "macos") {
        // Why: Runtime TLS stack pulls security-framework symbols that require explicit framework linkage.
        link_args.push("-framework".into());
        link_args.push("CoreFoundation".into());
        link_args.push("-framework".into());
        link_args.push("Security".into());
    }
    if let Ok(ldflags) = std::env::var("GOST_LDFLAGS") {
        for tok in ldflags.split_whitespace() {
            if !tok.is_empty() {
                link_args.push(tok.to_string());
            }
        }
    }
    link_args.push("-o".into());
    link_args.push(exe_path.display().to_string());
    let mut link_key_material = String::new();
    link_key_material.push_str("gost-link-cache-v1\0");
    link_key_material.push_str(&cc);
    link_key_material.push('\0');
    link_key_material.push_str(&tool_identity(&cc, &["--version"]));
    link_key_material.push('\0');
    link_key_material
        .push_str(&incremental::hash_file_sha256(&obj_path).map_err(|e| e.to_string())?);
    link_key_material.push('\0');
    link_key_material.push_str(&incremental::hash_file_sha256(&rt_lib).map_err(|e| e.to_string())?);
    link_key_material.push('\0');
    for arg in &link_args {
        link_key_material.push_str(arg);
        link_key_material.push('\0');
    }
    let link_key = incremental::hash_bytes_sha256(link_key_material.as_bytes());
    let bin_cache =
        incremental::ensure_cache_namespace(input_path, "bin").map_err(|e| e.to_string())?;
    let cached_exe = cache_path_with_template_ext(&bin_cache, &link_key, &exe_path);
    let cached_exe_warnings = warning_sidecar_path(&cached_exe);
    if cached_exe.exists() {
        std::fs::copy(&cached_exe, &exe_path).map_err(|e| {
            format!(
                "failed to restore linked executable from cache {} -> {}: {}",
                cached_exe.display(),
                exe_path.display(),
                e
            )
        })?;
        replay_cached_warnings_from_path(&cached_exe_warnings);
        inc_trace(&format!("link cache hit: {}", cached_exe.display()));
    } else {
        let output = run_cmd_capture(&cc, &link_args)?;
        let warnings = collect_warning_lines(&output);
        persist_warning_lines(&cached_exe_warnings, &warnings);
        if let Some(parent) = cached_exe.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        let _ = std::fs::copy(&exe_path, &cached_exe);
        inc_trace(&format!("link cache store: {}", cached_exe.display()));
    }
    let exe_cmd = exe_path.canonicalize().unwrap_or_else(|_| exe_path.clone());
    run_cmd(exe_cmd.to_string_lossy().as_ref(), &[])?;
    Ok(())
}

fn build_runtime(
    entry_path: &Path,
    runtime_dir: &Path,
    cc: &str,
    opt_level: OptLevel,
) -> Result<PathBuf, String> {
    let cargo_toml = runtime_dir.join("Cargo.toml");
    if !cargo_toml.exists() {
        return Err("runtime/Cargo.toml not found; Rust runtime required".to_string());
    }
    build_runtime_rust(entry_path, runtime_dir, cc, opt_level)
}

fn build_runtime_rust(
    entry_path: &Path,
    runtime_dir: &Path,
    cc: &str,
    opt_level: OptLevel,
) -> Result<PathBuf, String> {
    let release = opt_level.is_optimized();
    let native = std::env::var("GOST_NATIVE")
        .map(|v| v != "0")
        .unwrap_or(true);
    let target = if let Ok(t) = std::env::var("GOST_RUST_TARGET") {
        Some(t)
    } else if let Some(triple) = compiler_triple(cc) {
        rust_target_from_compiler_triple(&triple).or_else(rustc_host_triple)
    } else {
        rustc_host_triple()
    };
    let lib_name = if target.as_deref().unwrap_or("").contains("msvc") {
        "gostrt.lib"
    } else {
        "libgostrt.a"
    };
    let profile = if release { "release" } else { "debug" };
    let runtime_cache_key =
        runtime_cache_key(runtime_dir, cc, opt_level, target.as_deref(), native)?;
    let runtime_cache_root =
        incremental::ensure_cache_namespace(entry_path, "runtime").map_err(|e| e.to_string())?;
    let runtime_cached = runtime_cache_root.join(&runtime_cache_key).join(lib_name);
    let runtime_warning_sidecar = warning_sidecar_path(&runtime_cached);
    if runtime_cached.exists() {
        replay_cached_warnings_from_path(&runtime_warning_sidecar);
        inc_trace(&format!("runtime cache hit: {}", runtime_cached.display()));
        return Ok(runtime_cached);
    }

    let mut cmd = Command::new("cargo");
    cmd.arg("build")
        .arg("--manifest-path")
        .arg(runtime_dir.join("Cargo.toml"));
    if release {
        cmd.arg("--release");
    }
    // Isolate runtime build artifacts from the main crate to avoid locking gs.exe on Windows.
    cmd.env("CARGO_TARGET_DIR", runtime_dir.join("target"));
    if let Some(target) = &target {
        cmd.arg("--target").arg(target);
    }
    if std::env::var("GOST_STATS").ok().as_deref() == Some("1") {
        cmd.arg("--features").arg("stats");
    }
    let mut rustflags = std::env::var("RUSTFLAGS").unwrap_or_default();
    if !rustflags.contains("opt-level=") {
        if !rustflags.is_empty() {
            rustflags.push(' ');
        }
        rustflags.push_str(&format!("-C opt-level={}", opt_level.as_digit()));
    }
    if native && opt_level.is_optimized() && !rustflags.contains("target-cpu") {
        if !rustflags.is_empty() {
            rustflags.push(' ');
        }
        rustflags.push_str("-C target-cpu=native");
    }
    if !rustflags.is_empty() {
        cmd.env("RUSTFLAGS", rustflags);
    }
    if let Some(target) = macos_deployment_target() {
        cmd.env("MACOSX_DEPLOYMENT_TARGET", target);
    }
    cmd.env("CC", cc);
    let output = cmd
        .output()
        .map_err(|e| format!("failed to run cargo: {}", e))?;
    if !output.stdout.is_empty() {
        print!("{}", String::from_utf8_lossy(&output.stdout));
    }
    if !output.stderr.is_empty() {
        eprint!("{}", String::from_utf8_lossy(&output.stderr));
    }
    if !output.status.success() {
        return Err("failed to build Rust runtime (is target installed?)".to_string());
    }
    let warnings = collect_warning_lines(&output);

    let lib_path = if let Some(target) = target {
        runtime_dir
            .join("target")
            .join(target)
            .join(profile)
            .join(lib_name)
    } else {
        runtime_dir.join("target").join(profile).join(lib_name)
    };
    if !lib_path.exists() {
        return Err(format!(
            "Rust runtime output not found: {}",
            lib_path.display()
        ));
    }
    if let Some(parent) = runtime_cached.parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            format!(
                "failed to create runtime cache directory {}: {}",
                parent.display(),
                e
            )
        })?;
    }
    std::fs::copy(&lib_path, &runtime_cached).map_err(|e| {
        format!(
            "failed to write runtime cache {}: {}",
            runtime_cached.display(),
            e
        )
    })?;
    persist_warning_lines(&runtime_warning_sidecar, &warnings);
    inc_trace(&format!(
        "runtime cache store: {}",
        runtime_cached.display()
    ));
    Ok(runtime_cached)
}

fn resolve_cc() -> String {
    if let Ok(cc) = std::env::var("GOST_CC") {
        return cc;
    }
    #[cfg(target_os = "macos")]
    if command_exists("clang") {
        return "clang".to_string();
    }
    if command_exists("gcc") {
        return "gcc".to_string();
    }
    "clang".to_string()
}

fn ensure_native_compiler(cc: &str) -> Result<(), String> {
    if is_clang(cc)
        && let Some(target) = compiler_target(cc)
        && target.contains("wasm32")
    {
        return Err(format!(
            "compiler target is {}; install a native clang and set GOST_CC to its path",
            target
        ));
    }
    Ok(())
}

fn compiler_target(cc: &str) -> Option<String> {
    let output = Command::new(cc).arg("--version").output().ok()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    for line in stdout.lines() {
        if let Some(rest) = line.strip_prefix("Target: ") {
            return Some(rest.trim().to_string());
        }
    }
    None
}

fn compile_ll_to_obj(
    entry_path: &Path,
    cc: &str,
    ll_path: &Path,
    obj_path: &Path,
    opt_level: OptLevel,
) -> Result<(), String> {
    let native = std::env::var("GOST_NATIVE")
        .map(|v| v != "0")
        .unwrap_or(true);
    let mut key_material = String::new();
    key_material.push_str("gost-obj-cache-v1\0");
    key_material.push_str(&incremental::hash_file_sha256(ll_path).map_err(|e| e.to_string())?);
    key_material.push('\0');
    key_material.push_str(cc);
    key_material.push('\0');
    key_material.push_str(&tool_identity(cc, &["--version"]));
    key_material.push('\0');
    key_material.push_str(&opt_level.as_digit().to_string());
    key_material.push('\0');
    key_material.push_str(if native { "native=1" } else { "native=0" });
    key_material.push('\0');
    if !is_clang(cc) {
        let llc = resolve_llc();
        key_material.push_str(&llc);
        key_material.push('\0');
        key_material.push_str(&tool_identity(&llc, &["--version"]));
        key_material.push('\0');
        if let Some(triple) = compiler_triple(cc) {
            key_material.push_str(&triple);
            key_material.push('\0');
        }
    }
    let key = incremental::hash_bytes_sha256(key_material.as_bytes());
    let obj_cache =
        incremental::ensure_cache_namespace(entry_path, "obj").map_err(|e| e.to_string())?;
    let cached_obj = obj_cache.join(format!("{}.o", key));
    let cached_obj_warnings = warning_sidecar_path(&cached_obj);
    if cached_obj.exists() {
        std::fs::copy(&cached_obj, obj_path).map_err(|e| {
            format!(
                "failed to restore object from cache {} -> {}: {}",
                cached_obj.display(),
                obj_path.display(),
                e
            )
        })?;
        replay_cached_warnings_from_path(&cached_obj_warnings);
        inc_trace(&format!("obj cache hit: {}", cached_obj.display()));
        return Ok(());
    }
    let tool_output = if is_clang(cc) {
        let mut args = vec![
            "-c".into(),
            ll_path.display().to_string(),
            "-o".into(),
            obj_path.display().to_string(),
        ];
        args.push(opt_level.clang_flag());
        if let Some(target) = macos_deployment_target() {
            args.push(format!("-mmacosx-version-min={}", target));
            args.push("-Wno-override-module".into());
        }
        if opt_level.is_optimized() && native {
            args.push("-march=native".into());
        }
        run_cmd_capture(cc, &args)?
    } else {
        let llc = resolve_llc();
        if !command_exists(&llc) {
            return Err("llc not found; install LLVM or set GOST_LLC".to_string());
        }
        let mut args = vec![
            "-filetype=obj".into(),
            "-o".into(),
            obj_path.display().to_string(),
        ];
        args.push(opt_level.llc_flag());
        if opt_level.is_optimized() && native {
            args.push("-mcpu=native".into());
        }
        if let Some(triple) = compiler_triple(cc) {
            args.push("-mtriple".into());
            args.push(triple);
        }
        args.push(ll_path.display().to_string());
        run_cmd_capture(&llc, &args)?
    };
    let warnings = collect_warning_lines(&tool_output);
    persist_warning_lines(&cached_obj_warnings, &warnings);
    let _ = std::fs::copy(obj_path, &cached_obj);
    inc_trace(&format!("obj cache store: {}", cached_obj.display()));
    Ok(())
}

fn cache_path_with_template_ext(cache_dir: &Path, key: &str, template: &Path) -> PathBuf {
    if let Some(ext) = template.extension().and_then(|s| s.to_str())
        && !ext.is_empty()
    {
        return cache_dir.join(format!("{}.{}", key, ext));
    }
    cache_dir.join(key)
}

fn runtime_cache_key(
    runtime_dir: &Path,
    cc: &str,
    opt_level: OptLevel,
    target: Option<&str>,
    native: bool,
) -> Result<String, String> {
    let mut files = collect_runtime_input_files(runtime_dir)?;
    files.sort();
    let mut material = String::new();
    material.push_str("gost-runtime-cache-v1\0");
    material.push_str(cc);
    material.push('\0');
    material.push_str(&tool_identity(cc, &["--version"]));
    material.push('\0');
    material.push_str(&opt_level.as_digit().to_string());
    material.push('\0');
    material.push_str(target.unwrap_or(""));
    material.push('\0');
    material.push_str(if native { "native=1" } else { "native=0" });
    material.push('\0');
    material.push_str(
        if std::env::var("GOST_STATS").ok().as_deref() == Some("1") {
            "stats=1"
        } else {
            "stats=0"
        },
    );
    material.push('\0');
    for f in files {
        material.push_str(&f);
        material.push('\0');
        let hash = incremental::hash_file_sha256(Path::new(&f)).map_err(|e| e.to_string())?;
        material.push_str(&hash);
        material.push('\0');
    }
    Ok(incremental::hash_bytes_sha256(material.as_bytes()))
}

fn collect_runtime_input_files(runtime_dir: &Path) -> Result<Vec<String>, String> {
    fn walk(root: &Path, dir: &Path, out: &mut Vec<String>) -> Result<(), String> {
        let entries =
            std::fs::read_dir(dir).map_err(|e| format!("read_dir {}: {}", dir.display(), e))?;
        for ent in entries {
            let ent = ent.map_err(|e| e.to_string())?;
            let path = ent.path();
            if path.is_dir() {
                if let Some(name) = path.file_name().and_then(|s| s.to_str())
                    && matches!(name, "target" | ".git")
                {
                    continue;
                }
                walk(root, &path, out)?;
                continue;
            }
            if path.is_file() {
                let rel = path
                    .strip_prefix(root)
                    .unwrap_or(&path)
                    .to_string_lossy()
                    .replace('\\', "/");
                out.push(root.join(rel.clone()).to_string_lossy().replace('\\', "/"));
            }
        }
        Ok(())
    }

    let mut out = Vec::new();
    for fixed in ["Cargo.toml", "Cargo.lock", "build.rs"] {
        let p = runtime_dir.join(fixed);
        if p.exists() && p.is_file() {
            out.push(p.to_string_lossy().replace('\\', "/"));
        }
    }
    let src = runtime_dir.join("src");
    if src.exists() {
        walk(runtime_dir, &src, &mut out)?;
    }
    Ok(out)
}

fn tool_identity(tool: &str, args: &[&str]) -> String {
    let mut cmd = Command::new(tool);
    cmd.args(args);
    match cmd.output() {
        Ok(out) => {
            let mut sig = String::new();
            sig.push_str(&format!("status={};", out.status));
            sig.push_str(&String::from_utf8_lossy(&out.stdout));
            sig.push_str(&String::from_utf8_lossy(&out.stderr));
            sig
        }
        Err(_) => format!("{}:<unavailable>", tool),
    }
}

fn resolve_llc() -> String {
    if let Ok(llc) = std::env::var("GOST_LLC") {
        return llc;
    }
    if let Ok(cc) = std::env::var("GOST_CC")
        && let Some(path) = tool_in_cc_dir(&cc, "llc")
    {
        return path;
    }
    "llc".to_string()
}

fn compiler_triple(cc: &str) -> Option<String> {
    let output = Command::new(cc).arg("-dumpmachine").output().ok()?;
    if !output.status.success() {
        return None;
    }
    let triple = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if triple.is_empty() {
        None
    } else {
        Some(triple)
    }
}

fn rustc_host_triple() -> Option<String> {
    let output = Command::new("rustc").arg("-vV").output().ok()?;
    if !output.status.success() {
        return None;
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    for line in stdout.lines() {
        if let Some(rest) = line.strip_prefix("host:") {
            let host = rest.trim();
            if !host.is_empty() {
                return Some(host.to_string());
            }
        }
    }
    None
}

fn rust_target_from_compiler_triple(raw: &str) -> Option<String> {
    let triple = raw.trim().to_ascii_lowercase();
    if triple.is_empty() {
        return None;
    }
    let arch_raw = triple.split('-').next()?;
    let arch = normalize_rust_arch(arch_raw);
    if triple.contains("darwin") || triple.contains("macos") {
        return Some(format!("{}-apple-darwin", arch));
    }
    if triple.contains("windows") || triple.contains("mingw") {
        let abi = if triple.contains("msvc") {
            "msvc"
        } else {
            "gnu"
        };
        return Some(format!("{}-pc-windows-{}", arch, abi));
    }
    if triple.contains("linux") {
        let libc = if triple.contains("musl") {
            "musl"
        } else {
            "gnu"
        };
        return Some(format!("{}-unknown-linux-{}", arch, libc));
    }
    if triple.contains("freebsd") {
        return Some(format!("{}-unknown-freebsd", arch));
    }
    if triple.contains("openbsd") {
        return Some(format!("{}-unknown-openbsd", arch));
    }
    if triple.contains("netbsd") {
        return Some(format!("{}-unknown-netbsd", arch));
    }
    if triple.contains("dragonfly") {
        return Some(format!("{}-unknown-dragonfly", arch));
    }
    let mut parts = triple.split('-');
    let _ = parts.next();
    let rest: Vec<&str> = parts.collect();
    if rest.is_empty() {
        Some(arch.into_owned())
    } else {
        Some(format!("{}-{}", arch, rest.join("-")))
    }
}

fn normalize_rust_arch(arch: &str) -> Cow<'_, str> {
    match arch {
        "arm64" => Cow::Borrowed("aarch64"),
        "amd64" => Cow::Borrowed("x86_64"),
        "x86" | "i386" | "i486" | "i586" => Cow::Borrowed("i686"),
        _ => Cow::Borrowed(arch),
    }
}

fn macos_deployment_target() -> Option<String> {
    if !cfg!(target_os = "macos") {
        return None;
    }
    if let Ok(raw) = std::env::var("MACOSX_DEPLOYMENT_TARGET") {
        let v = raw.trim();
        if !v.is_empty() {
            return Some(v.to_string());
        }
    }
    let output = Command::new("sw_vers")
        .arg("-productVersion")
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let raw = String::from_utf8_lossy(&output.stdout);
    let mut parts = raw.trim().split('.');
    let major = parts.next()?.trim();
    if major.is_empty() || !major.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    let minor = parts
        .next()
        .map(str::trim)
        .filter(|s| !s.is_empty() && s.chars().all(|c| c.is_ascii_digit()))
        .unwrap_or("0");
    Some(format!("{}.{}", major, minor))
}

fn is_clang(cc: &str) -> bool {
    if cc.to_ascii_lowercase().contains("clang") {
        return true;
    }
    if let Ok(output) = Command::new(cc).arg("--version").output()
        && output.status.success()
    {
        let stdout = String::from_utf8_lossy(&output.stdout).to_ascii_lowercase();
        if stdout.contains("clang") {
            return true;
        }
    }
    false
}

fn is_gcc(cc: &str) -> bool {
    cc.to_ascii_lowercase().contains("gcc")
}

fn command_exists(cmd: &str) -> bool {
    Command::new(cmd).arg("--version").output().is_ok()
}

fn sanitize_module_name(raw: &str) -> String {
    let mut out = String::new();
    for (i, ch) in raw.chars().enumerate() {
        let valid = ch.is_ascii_alphanumeric() || ch == '_';
        let c = if valid { ch } else { '_' };
        if i == 0 {
            if c.is_ascii_alphabetic() || c == '_' {
                out.push(c);
            } else {
                out.push('_');
                out.push(c);
            }
        } else {
            out.push(c);
        }
    }
    if out.is_empty() {
        "bindings".to_string()
    } else {
        out
    }
}

fn run_bindgen(header: &Path, out: &Path, module: &str, abi: &str) -> Result<(), String> {
    let source = std::fs::read_to_string(header)
        .map_err(|e| format!("failed to read {}: {}", header.display(), e))?;
    let stripped = strip_c_comments(&source);
    let stmts = split_c_statements(&stripped);

    #[derive(Clone)]
    struct FnSig {
        name: String,
        ret: String,
        params: Vec<(String, String)>,
        variadic: bool,
    }
    #[derive(Clone)]
    struct GlobalSig {
        name: String,
        ty: String,
    }

    let mut fns: Vec<FnSig> = Vec::new();
    let mut globals: Vec<GlobalSig> = Vec::new();
    for stmt in stmts {
        let s = stmt.trim();
        if s.is_empty()
            || s.starts_with('#')
            || s.contains('{')
            || s.contains('}')
            || s.starts_with("typedef")
        {
            continue;
        }
        if let Some(sig) = parse_c_function_decl(s) {
            fns.push(FnSig {
                name: sig.0,
                ret: sig.1,
                params: sig.2,
                variadic: sig.3,
            });
            continue;
        }
        if let Some(g) = parse_c_global_decl(s) {
            globals.push(GlobalSig { name: g.0, ty: g.1 });
        }
    }

    let mut generated = String::new();
    generated.push_str(&format!("module {}\n\n", sanitize_module_name(module)));
    for g in &globals {
        generated.push_str(&format!("extern \"{}\" let {}: {};\n", abi, g.name, g.ty));
    }
    if !globals.is_empty() && !fns.is_empty() {
        generated.push('\n');
    }
    for f in &fns {
        let mut params = Vec::new();
        for (name, ty) in &f.params {
            params.push(format!("{}: {}", name, ty));
        }
        if f.variadic {
            params.push("...".to_string());
        }
        generated.push_str(&format!(
            "extern \"{}\" fn {}({}) -> {};\n",
            abi,
            f.name,
            params.join(", "),
            f.ret
        ));
    }
    std::fs::write(out, generated)
        .map_err(|e| format!("failed to write {}: {}", out.display(), e))?;
    Ok(())
}

fn strip_c_comments(input: &str) -> String {
    let bytes = input.as_bytes();
    let mut out = String::new();
    let mut i = 0usize;
    while i < bytes.len() {
        if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'/' {
            i += 2;
            while i < bytes.len() && bytes[i] != b'\n' {
                i += 1;
            }
            continue;
        }
        if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'*' {
            i += 2;
            while i + 1 < bytes.len() && !(bytes[i] == b'*' && bytes[i + 1] == b'/') {
                i += 1;
            }
            i = (i + 2).min(bytes.len());
            continue;
        }
        out.push(bytes[i] as char);
        i += 1;
    }
    out
}

fn split_c_statements(input: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut depth = 0i32;
    for ch in input.chars() {
        match ch {
            '(' => {
                depth += 1;
                cur.push(ch);
            }
            ')' => {
                depth -= 1;
                cur.push(ch);
            }
            ';' if depth == 0 => {
                out.push(cur.trim().to_string());
                cur.clear();
            }
            _ => cur.push(ch),
        }
    }
    if !cur.trim().is_empty() {
        out.push(cur.trim().to_string());
    }
    out
}

type CParamDecl = (String, String);
type CFunctionDecl = (String, String, Vec<CParamDecl>, bool);

fn parse_c_function_decl(stmt: &str) -> Option<CFunctionDecl> {
    let open = stmt.find('(')?;
    let close = stmt.rfind(')')?;
    if close <= open {
        return None;
    }
    let head = stmt[..open].trim();
    if head.is_empty() {
        return None;
    }
    let (name, head_start) = extract_trailing_ident(head)?;
    let ret_c = head[..head_start].trim();
    if ret_c.is_empty() {
        return None;
    }
    let ret = map_c_type_to_gs(ret_c);
    let params_raw = stmt[open + 1..close].trim();
    let mut params = Vec::new();
    let mut variadic = false;
    if !params_raw.is_empty() && params_raw != "void" {
        for (idx, p) in params_raw.split(',').enumerate() {
            let p = p.trim();
            if p.is_empty() {
                continue;
            }
            if p == "..." {
                variadic = true;
                continue;
            }
            let (pname, pty) = parse_c_param_decl(p, idx);
            params.push((pname, pty));
        }
    }
    Some((name, ret, params, variadic))
}

fn parse_c_param_decl(param: &str, idx: usize) -> (String, String) {
    if let Some((name, start)) = extract_trailing_ident(param) {
        let mut ty = param[..start].trim().to_string();
        if ty.is_empty() {
            ty = param.trim().to_string();
            return (format!("arg{}", idx), map_c_type_to_gs(&ty));
        }
        if ty.ends_with('*') {
            ty.push(' ');
        }
        (name, map_c_type_to_gs(&ty))
    } else {
        (format!("arg{}", idx), map_c_type_to_gs(param))
    }
}

fn parse_c_global_decl(stmt: &str) -> Option<(String, String)> {
    let s = stmt.trim();
    if !s.starts_with("extern ") || s.contains('(') {
        return None;
    }
    let body = s.trim_start_matches("extern").trim();
    let (name, start) = extract_trailing_ident(body)?;
    let ty = body[..start].trim();
    if ty.is_empty() {
        return None;
    }
    Some((name, map_c_type_to_gs(ty)))
}

fn extract_trailing_ident(s: &str) -> Option<(String, usize)> {
    let bytes = s.as_bytes();
    let mut end = bytes.len();
    while end > 0 && bytes[end - 1].is_ascii_whitespace() {
        end -= 1;
    }
    let mut start = end;
    while start > 0 {
        let c = bytes[start - 1] as char;
        if c.is_ascii_alphanumeric() || c == '_' {
            start -= 1;
        } else {
            break;
        }
    }
    if start == end {
        return None;
    }
    Some((s[start..end].to_string(), start))
}

fn map_c_type_to_gs(c_ty: &str) -> String {
    let mut t = c_ty.replace('\t', " ");
    for q in ["const", "volatile", "restrict", "register"] {
        t = t.replace(q, " ");
    }
    let lower = t
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
        .to_ascii_lowercase();
    let is_ptr = lower.contains('*') || lower.contains('[');
    if is_ptr {
        if lower.contains("char") {
            return "string".to_string();
        }
        return "i64".to_string();
    }
    match lower.as_str() {
        "void" => "unit".to_string(),
        "bool" | "_bool" => "bool".to_string(),
        "char" | "signed char" | "short" | "short int" | "int" | "int32_t" => "i32".to_string(),
        "unsigned char" | "unsigned short" | "unsigned short int" | "unsigned int" | "uint32_t" => {
            "u32".to_string()
        }
        "long" | "long int" | "long long" | "long long int" | "int64_t" | "ssize_t" => {
            "i64".to_string()
        }
        "unsigned long"
        | "unsigned long int"
        | "unsigned long long"
        | "unsigned long long int"
        | "uint64_t"
        | "size_t" => "u64".to_string(),
        "float" => "f32".to_string(),
        "double" | "long double" => "f64".to_string(),
        _ => "i64".to_string(),
    }
}

fn run_cmd(cmd: &str, args: &[String]) -> Result<(), String> {
    let status = Command::new(cmd)
        .args(args)
        .status()
        .map_err(|e| format!("failed to run {}: {}", cmd, e))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("command failed: {} {}", cmd, args.join(" ")))
    }
}

fn run_cmd_capture(cmd: &str, args: &[String]) -> Result<std::process::Output, String> {
    let output = Command::new(cmd)
        .args(args)
        .output()
        .map_err(|e| format!("failed to run {}: {}", cmd, e))?;
    if !output.stdout.is_empty() {
        print!("{}", String::from_utf8_lossy(&output.stdout));
    }
    if !output.stderr.is_empty() {
        eprint!("{}", String::from_utf8_lossy(&output.stderr));
    }
    if output.status.success() {
        Ok(output)
    } else {
        Err(format!("command failed: {} {}", cmd, args.join(" ")))
    }
}

fn collect_warning_lines(output: &std::process::Output) -> Vec<String> {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    collect_warning_lines_from_text(&stdout, &stderr)
}

fn collect_warning_lines_from_text(stdout: &str, stderr: &str) -> Vec<String> {
    let mut lines = Vec::<String>::new();
    let mut seen = BTreeSet::<String>::new();
    for line in stdout.lines().chain(stderr.lines()) {
        if !line.to_ascii_lowercase().contains("warning") {
            continue;
        }
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        if seen.insert(trimmed.to_string()) {
            lines.push(trimmed.to_string());
        }
    }
    lines
}

fn warning_sidecar_path(cache_path: &Path) -> PathBuf {
    let mut text = cache_path.to_string_lossy().to_string();
    text.push_str(".warnings");
    PathBuf::from(text)
}

fn persist_warning_lines(path: &Path, warnings: &[String]) {
    if warnings.is_empty() {
        let _ = std::fs::remove_file(path);
        return;
    }
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    let payload = warnings.join("\n");
    let _ = std::fs::write(path, payload);
}

fn replay_cached_warnings_from_path(path: &Path) {
    if !path.exists() {
        return;
    }
    if let Ok(raw) = std::fs::read_to_string(path) {
        for line in raw.lines() {
            let text = line.trim();
            if text.is_empty() {
                continue;
            }
            eprintln!("{}", text);
        }
    }
}

fn tool_in_cc_dir(cc: &str, tool: &str) -> Option<String> {
    let cc_path = Path::new(cc);
    let dir = cc_path.parent()?;
    let exe = if cfg!(windows) {
        format!("{}.exe", tool)
    } else {
        tool.to_string()
    };
    let candidate = dir.join(exe);
    if candidate.exists() {
        return Some(candidate.to_string_lossy().to_string());
    }
    if cfg!(windows) {
        let candidate = dir.join(tool);
        if candidate.exists() {
            return Some(candidate.to_string_lossy().to_string());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::{
        collect_warning_lines_from_text, normalize_explain_key, persist_warning_lines,
        rust_target_from_compiler_triple, warning_sidecar_path,
    };
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn rust_target_normalizes_macos_arm64_dumpmachine() {
        let target = rust_target_from_compiler_triple("arm64-apple-darwin25.2.0")
            .expect("target should be inferred");
        assert_eq!(target, "aarch64-apple-darwin");
    }

    #[test]
    fn rust_target_normalizes_linux_gnu_dumpmachine() {
        let target = rust_target_from_compiler_triple("x86_64-linux-gnu")
            .expect("target should be inferred");
        assert_eq!(target, "x86_64-unknown-linux-gnu");
    }

    #[test]
    fn rust_target_normalizes_mingw_dumpmachine() {
        let target = rust_target_from_compiler_triple("x86_64-w64-mingw32")
            .expect("target should be inferred");
        assert_eq!(target, "x86_64-pc-windows-gnu");
    }

    #[test]
    fn explain_shorthand_maps_monomorphization_depth_code() {
        assert_eq!(normalize_explain_key("mono-depth"), "E1201");
        assert_eq!(normalize_explain_key("generic-depth"), "E1201");
        assert_eq!(normalize_explain_key("monomorphization-depth"), "E1201");
    }

    #[test]
    fn explain_key_passthrough_keeps_unknown_literal() {
        assert_eq!(normalize_explain_key("E9999"), "E9999");
    }

    #[test]
    fn warning_parser_collects_and_deduplicates_warning_lines() {
        let stdout = "note: hi\nwarning: alpha\nwarning: alpha\n";
        let stderr = "WARNING: beta\nerror: nope\n";
        let got = collect_warning_lines_from_text(stdout, stderr);
        assert_eq!(
            got,
            vec!["warning: alpha".to_string(), "WARNING: beta".to_string()]
        );
    }

    #[test]
    fn warning_sidecar_is_written_as_plain_lines() {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock drift")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "gost-cli-warning-sidecar-{}-{}",
            std::process::id(),
            nonce
        ));
        fs::create_dir_all(&root).expect("mkdir");
        let cache_path = root.join("artifact.o");
        let sidecar = warning_sidecar_path(&cache_path);
        let warnings = vec!["warning: first".to_string(), "warning: second".to_string()];
        persist_warning_lines(&sidecar, &warnings);
        let raw = fs::read_to_string(&sidecar).expect("read sidecar");
        assert!(raw.contains("warning: first"));
        assert!(raw.contains("warning: second"));
        let _ = fs::remove_dir_all(PathBuf::from(root));
    }
}
