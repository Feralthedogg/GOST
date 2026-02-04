use std::path::{Path, PathBuf};
use std::process::Command;

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
    let first = match args.next() {
        Some(arg) => arg,
        None => {
            print_usage();
            return 1;
        }
    };
    let (mode, input) = if first == "run" {
        let input = match args.next() {
            Some(arg) => arg,
            None => {
                print_usage();
                return 1;
            }
        };
        (CliMode::Run, input)
    } else {
        (default_mode, first)
    };
    let mut output = None;
    while let Some(arg) = args.next() {
        if arg == "-o" {
            match args.next() {
                Some(path) => output = Some(PathBuf::from(path)),
                None => {
                    eprintln!("expected output after -o");
                    return 1;
                }
            }
        } else {
            eprintln!("unknown argument: {}", arg);
            return 1;
        }
    }
    let input_path = PathBuf::from(&input);
    if input_path.extension().and_then(|s| s.to_str()) != Some("gs") {
        eprintln!("expected .gs source file");
        return 1;
    }
    let llvm = match crate::compile::compile_to_llvm(&input_path) {
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

fn print_usage() {
    eprintln!("usage: gs run <input.gs> [-o output.ll]");
    eprintln!("   or: gost <input.gs> [-o output.ll]");
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
