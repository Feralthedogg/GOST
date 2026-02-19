use std::env;

fn main() {
    let target = env::var("TARGET").unwrap_or_default();
    if target.contains("windows-msvc") {
        panic!("gostrt runtime requires gnu target to assemble .asm (install x86_64-pc-windows-gnu and set GOST_RUST_TARGET)");
    }
    let mut build = cc::Build::new();
    if target.contains("apple") {
        if target.contains("aarch64") || target.contains("arm64") {
            build.file("gostctx_arm64_macos.asm");
        } else {
            build.file("gostctx_x86_64_macos.asm");
        }
    } else if target.contains("aarch64") || target.contains("arm64") {
        build.file("gostctx_arm64.asm");
    } else if target.contains("windows") {
        build.file("gostctx_x86_64_win.asm");
    } else {
        build.file("gostctx_x86_64_sysv.asm");
    }
    build.flag("-x").flag("assembler");
    build.flag_if_supported("-Qunused-arguments");
    build.compile("gostctx");
}
