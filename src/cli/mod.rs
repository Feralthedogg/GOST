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
    let rt_src = runtime_dir.join("gostrt.c");
    let rt_obj = runtime_dir.join("gostrt.o");
    let rt_lib = runtime_dir.join("libgostrt.a");
    let stamp_path = runtime_dir.join("gostrt.toolchain");
    let stamp = toolchain_stamp(cc).unwrap_or_else(|_| cc.to_string());
    let mut needs_rebuild = !rt_lib.exists();
    if !needs_rebuild {
        if is_newer(&rt_src, &rt_lib)? {
            needs_rebuild = true;
        } else if let Ok(existing) = std::fs::read_to_string(&stamp_path) {
            if existing != stamp {
                needs_rebuild = true;
            }
        } else {
            needs_rebuild = true;
        }
    }
    if needs_rebuild {
        let _ = std::fs::remove_file(&rt_obj);
        let _ = std::fs::remove_file(&rt_lib);
        run_cmd(
            cc,
            &[
                "-c".into(),
                rt_src.display().to_string(),
                "-o".into(),
                rt_obj.display().to_string(),
            ],
        )?;
        let ar = find_ar().ok_or_else(|| "llvm-ar or ar not found in PATH".to_string())?;
        run_cmd(
            &ar,
            &[
                "rcs".into(),
                rt_lib.display().to_string(),
                rt_obj.display().to_string(),
            ],
        )?;
        std::fs::write(&stamp_path, stamp).map_err(|e| e.to_string())?;
    }
    Ok(rt_lib)
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

fn toolchain_stamp(cc: &str) -> Result<String, String> {
    let version = Command::new(cc)
        .arg("--version")
        .output()
        .map_err(|e| e.to_string())?;
    let mut stamp = String::new();
    stamp.push_str("cc=");
    stamp.push_str(cc);
    stamp.push('\n');
    if let Ok(text) = String::from_utf8(version.stdout) {
        if let Some(line) = text.lines().next() {
            stamp.push_str("version=");
            stamp.push_str(line.trim());
            stamp.push('\n');
        }
    }
    if let Some(triple) = compiler_triple(cc) {
        stamp.push_str("triple=");
        stamp.push_str(triple.trim());
        stamp.push('\n');
    }
    Ok(stamp)
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

fn is_newer(src: &Path, out: &Path) -> Result<bool, String> {
    let src_meta = std::fs::metadata(src).map_err(|e| e.to_string())?;
    let out_meta = std::fs::metadata(out).map_err(|e| e.to_string())?;
    let src_time = src_meta.modified().map_err(|e| e.to_string())?;
    let out_time = out_meta.modified().map_err(|e| e.to_string())?;
    Ok(src_time > out_time)
}

fn find_ar() -> Option<String> {
    if let Ok(ar) = std::env::var("GOST_AR") {
        if command_exists(&ar) {
            return Some(ar);
        }
    }
    if let Ok(cc) = std::env::var("GOST_CC") {
        if let Some(path) = tool_in_cc_dir(&cc, "llvm-ar") {
            if command_exists(&path) {
                return Some(path);
            }
        }
        if let Some(path) = tool_in_cc_dir(&cc, "ar") {
            if command_exists(&path) {
                return Some(path);
            }
        }
    }
    for name in ["llvm-ar", "ar"] {
        let ok = Command::new(name)
            .arg("--version")
            .output()
            .is_ok();
        if ok {
            return Some(name.to_string());
        }
    }
    None
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
