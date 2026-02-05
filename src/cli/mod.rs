use std::path::{Path, PathBuf};
use std::process::Command;

use crate::pkg::resolve::ModMode;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CliMode {
    Compile,
    Run,
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

pub fn run_cli_with_mode<I>(args: I, default_mode: CliMode) -> i32
where
    I: IntoIterator<Item = String>,
{
    let mut args = args.into_iter();
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
        while let Some(arg) = args.next() {
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
            eprintln!("error: conflicting flags: --offline disables network, so --online can't be used (remove --offline).");
            return 1;
        }
        if global_offline && online {
            eprintln!("error: --offline is set: network is disabled (remove --offline to use --online).");
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
            "init" => {
                crate::pkg::modcmd::cmd_init(cwd, module_arg)
            }
            "tidy" => {
                crate::pkg::modcmd::cmd_tidy(cwd, mod_mode, offline)
            }
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
    let mut mod_mode = if mode == CliMode::Run {
        ModMode::Mod
    } else {
        ModMode::Readonly
    };
    let mut offline = false;
    while let Some(arg) = args.next() {
        if arg == "-o" {
            match args.next() {
                Some(path) => output = Some(PathBuf::from(path)),
                None => {
                    eprintln!("expected output after -o");
                    return 1;
                }
            }
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
    let ll_path = output
        .map(PathBuf::from)
        .unwrap_or_else(|| input_path.with_extension("ll"));
    if let Err(err) = std::fs::write(&ll_path, llvm) {
        eprintln!("failed to write {}: {}", ll_path.display(), err);
        return 1;
    }
    if mode == CliMode::Run {
        if let Err(err) = link_and_run(&input_path, &ll_path) {
            eprintln!("{}", err);
            return 1;
        }
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
    eprintln!("  mod       Module/dependency management");
    eprintln!("  explain   Show detailed explanation for a diagnostic code");
    eprintln!("  help      Print this message or the help of the given subcommand(s)");
    eprintln!();
    eprintln!("GLOBAL OPTIONS:");
    eprintln!("  --offline         Disallow any network access (git fetch/clone). Highest priority.");
    eprintln!("  -V, --version     Print version information");
    eprintln!("  -h, --help        Print help information");
    eprintln!();
    eprintln!("POLICY:");
    eprintln!("  - --offline is global and overrides subcommand-specific online behavior.");
    eprintln!("  - Using --offline together with --online is an error.");
    eprintln!("  - Network access (fetch/clone) is only allowed when explicitly enabled by subcommands that support it.");
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
    eprintln!("  gs explain E1101");
    eprintln!("  gs --explain E1101");
}

fn explain_code(code: &str) -> i32 {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("docs").join("diagnostics.md");
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
        if l.starts_with("- ") && l.to_ascii_lowercase().contains(&target.to_ascii_lowercase()) {
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
    eprintln!("  - If no gost.mod is found, legacy import resolution is used (no module fetching).");
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
    eprintln!("              If omitted, gs tries to infer from git origin, otherwise uses a placeholder.");
    eprintln!();
    eprintln!("GLOBAL OPTIONS:");
    eprintln!("  --offline    Disallow any network access (not used by init, but accepted globally).");
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
    eprintln!("  --readonly     Do not modify files; only verify that requirements/lock are consistent.");
    eprintln!("  --offline      Disallow any network access (git fetch/clone).");
    eprintln!();
    eprintln!("GLOBAL OPTIONS:");
    eprintln!("  --offline      (same as above; global)");
    eprintln!();
    eprintln!("BEHAVIOR:");
    eprintln!("  - mod mode:    may update gost.mod and refresh gost.lock (network may be needed unless offline)");
    eprintln!("  - readonly:    never mutates; fails if required modules are missing or lock mismatched");
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
    eprintln!("  --online      Allow network access for resolve check (git fetch/clone) if cache is missing.");
    eprintln!("  --offline     Force offline (explicit; also the default).");
    eprintln!();
    eprintln!("GLOBAL OPTIONS:");
    eprintln!("  --offline     Disallow any network access. Overrides --online.");
    eprintln!();
    eprintln!("BEHAVIOR (2-phase):");
    eprintln!("  1) strict readonly tidy verification (no edits)");
    eprintln!("  2) resolve check:");
    eprintln!("     - default: offline (cache-only)");
    eprintln!("     - --online: allows fetch/clone if needed (still readonly; does not write lock)");
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
    eprintln!("  --online      Allow network access (git fetch/clone) if cache/mirrors are missing.");
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
    eprintln!("  --online      Optional: may additionally show local cache status (if implemented),");
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

fn link_and_run(input_path: &Path, ll_path: &Path) -> Result<(), String> {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let runtime_dir = manifest_dir.join("runtime");
    let cc = resolve_cc();
    ensure_native_compiler(&cc)?;
    let rt_lib = build_runtime(&runtime_dir, &cc)?;
    let obj_path = input_path.with_extension("o");
    let exe_path = input_path.with_extension(if cfg!(windows) { "exe" } else { "" });
    compile_ll_to_obj(&cc, ll_path, &obj_path)?;
    let mut link_args = vec![
        obj_path.display().to_string(),
        rt_lib.display().to_string(),
    ];
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
    link_args.push("-o".into());
    link_args.push(exe_path.display().to_string());
    run_cmd(&cc, &link_args)?;
    let exe_cmd = exe_path
        .canonicalize()
        .unwrap_or_else(|_| exe_path.clone());
    run_cmd(exe_cmd.to_string_lossy().as_ref(), &[])?;
    Ok(())
}

fn build_runtime(runtime_dir: &Path, cc: &str) -> Result<PathBuf, String> {
    let cargo_toml = runtime_dir.join("Cargo.toml");
    if !cargo_toml.exists() {
        return Err("runtime/Cargo.toml not found; Rust runtime required".to_string());
    }
    build_runtime_rust(runtime_dir, cc)
}

fn build_runtime_rust(runtime_dir: &Path, cc: &str) -> Result<PathBuf, String> {
    let target = if let Ok(t) = std::env::var("GOST_RUST_TARGET") {
        Some(t)
    } else if let Some(triple) = compiler_triple(cc) {
        if triple.contains("msvc") {
            Some("x86_64-pc-windows-msvc".to_string())
        } else if triple.contains("mingw") || triple.contains("gnu") {
            Some("x86_64-pc-windows-gnu".to_string())
        } else {
            Some(triple)
        }
    } else {
        None
    };
    let mut cmd = Command::new("cargo");
    cmd.arg("build")
        .arg("--manifest-path")
        .arg(runtime_dir.join("Cargo.toml"));
    if let Some(target) = &target {
        cmd.arg("--target").arg(target);
    }
    if std::env::var("GOST_STATS").ok().as_deref() == Some("1") {
        cmd.arg("--features").arg("stats");
    }
    cmd.env("CC", cc);
    let status = cmd.status().map_err(|e| format!("failed to run cargo: {}", e))?;
    if !status.success() {
        return Err("failed to build Rust runtime (is target installed?)".to_string());
    }
    let lib_name = if target.as_deref().unwrap_or("").contains("msvc") {
        "gostrt.lib"
    } else {
        "libgostrt.a"
    };
    let lib_path = if let Some(target) = target {
        runtime_dir.join("target").join(target).join("debug").join(lib_name)
    } else {
        runtime_dir.join("target").join("debug").join(lib_name)
    };
    if !lib_path.exists() {
        return Err(format!("Rust runtime output not found: {}", lib_path.display()));
    }
    Ok(lib_path)
}

fn resolve_cc() -> String {
    if let Ok(cc) = std::env::var("GOST_CC") {
        return cc;
    }
    if command_exists("gcc") {
        return "gcc".to_string();
    }
    "clang".to_string()
}

fn ensure_native_compiler(cc: &str) -> Result<(), String> {
    if is_clang(cc) {
        if let Some(target) = compiler_target(cc) {
            if target.contains("wasm32") {
                return Err(format!(
                    "compiler target is {}; install a native clang and set GOST_CC to its path",
                    target
                ));
            }
        }
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

fn compile_ll_to_obj(cc: &str, ll_path: &Path, obj_path: &Path) -> Result<(), String> {
    if is_clang(cc) {
        return run_cmd(
            cc,
            &[
                "-c".into(),
                ll_path.display().to_string(),
                "-o".into(),
                obj_path.display().to_string(),
            ],
        );
    }
    let llc = resolve_llc();
    if !command_exists(&llc) {
        return Err("llc not found; install LLVM or set GOST_LLC".to_string());
    }
    let mut args = vec![
        "-filetype=obj".into(),
        "-o".into(),
        obj_path.display().to_string(),
    ];
    if let Some(triple) = compiler_triple(cc) {
        args.push("-mtriple".into());
        args.push(triple);
    }
    args.push(ll_path.display().to_string());
    run_cmd(&llc, &args)
}

fn resolve_llc() -> String {
    if let Ok(llc) = std::env::var("GOST_LLC") {
        return llc;
    }
    if let Ok(cc) = std::env::var("GOST_CC") {
        if let Some(path) = tool_in_cc_dir(&cc, "llc") {
            return path;
        }
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

fn is_clang(cc: &str) -> bool {
    cc.to_ascii_lowercase().contains("clang")
}

fn is_gcc(cc: &str) -> bool {
    cc.to_ascii_lowercase().contains("gcc")
}

fn command_exists(cmd: &str) -> bool {
    Command::new(cmd).arg("--version").output().is_ok()
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
