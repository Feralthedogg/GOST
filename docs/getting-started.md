# Getting Started

## Prerequisites

- Rust toolchain (`cargo`, `rustc`)
- Native C toolchain for linking:
  - `clang` (preferred path), or
  - `gcc` + `llc`

Runtime is built from `runtime/` as a Rust static library during `gs run`.

## Build the CLI

```powershell
cargo build --bin gs
```

On Windows, you can also use `gs.cmd` from repo root. It bootstraps local toolchain paths under `tools/winlibs` if present.

## First Program

Create `hello.gs`:

```gs
module main

import "std/fmt" as fmt { println }

fn main() -> i32 {
    fmt.println("Hello, Gost")
    return 0
}
```

Run:

```powershell
cargo run --bin gs -- run hello.gs
```

Or (Windows):

```powershell
.\gs.cmd run hello.gs
```

## Build LLVM IR Only

```powershell
cargo run --bin gs -- build hello.gs
```

Output defaults to `hello.ll`. Override with `--output` or `-o <file>`.

## Optimization Levels

Supported optimization flags:

- `-O` (same as `-O1`)
- `-O0`, `-O1`, `-O2`, `-O3`
- lowercase aliases: `-o0`, `-o1`, `-o2`, `-o3`
- `--opt 0|1|2|3`

Default optimization level is `O3`.

## Useful Environment Variables

- `GOST_CC`: compiler command/path (e.g. `clang`, `gcc`)
- `GOST_LLC`: `llc` path (used in gcc/llc path)
- `GOST_LDFLAGS`: extra linker flags
- `GOST_NATIVE`: `1`/`0` for native CPU tuning
- `GOST_RUST_TARGET`: explicit Rust target for runtime build
- `GOST_OPT_LEVEL` / `GOST_OPT`: default optimization level
- `GOST_RELEASE`: if `0`, defaults to `O0`; otherwise release-default behavior
- `GOST_COLOR`: enables ANSI diagnostic coloring
- `GOST_HOME`: fallback location containing `std/`
- `GOST_CACHE_DIR`: module cache root override

## Windows Toolchain Helper

If you need a GCC+LLVM bundle on Windows:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/setup-toolchain.ps1
```

## Next Steps

- Read [Language Guide](language-guide.md)
- Check [CLI Reference](cli-reference.md)
- For project dependency workflow, read [Modules and Packages](modules-and-packages.md)
