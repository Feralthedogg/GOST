# CLI Reference

Gost CLI entrypoint is `gs` (`src/bin/gs.rs`, `src/cli/mod.rs`).

## Global

```text
gs [--offline] <command> ...
```

- `--offline`: disallow any network access (highest priority)
- `-h`, `--help`: help
- `-V`, `--version`: version
- `--explain <CODE>` / `--explain=CODE`: show diagnostics from `docs/diagnostics.md`

`--online` is only valid for `gs mod verify`, `gs mod download`, and `gs mod graph`.

## run

```text
gs run [OPTIONS] <file.gs>
```

Options:

- `-O`, `-O0..-O3`, `-o0..-o3`, `--opt <0..3>`
- `--output <file>`, `-o <file>`: write LLVM IR path (default `<file>.ll`)
- `-mod=mod|readonly` or `--mod=mod|readonly`
  - default for `run`: `mod`

Behavior:

- Compiles to LLVM IR
- Builds runtime static lib
- Links executable
- Runs executable

## build

```text
gs build [OPTIONS] <file.gs>
```

Same options as `run`, but default `-mod=readonly`.

Behavior:

- Compiles to LLVM IR and writes `.ll`
- Does not run linked binary

## mod

```text
gs mod <subcommand> [OPTIONS]
```

Subcommands:

- `init [module]`
- `tidy [--readonly] [--offline]`
- `verify [--online|--offline]`
- `download [--online|--offline]`
- `graph [--online|--offline]`

See [Modules and Packages](modules-and-packages.md) for details.

## bindgen

```text
gs bindgen [OPTIONS] <header.h>
```

Options:

- `-o <file.gs>`
- `--module <name>`
- `--abi <name>` (default `C`)

Generates Gost extern declarations from simple C declarations.

## explain

```text
gs explain <CODE>
```

Alias:

```text
gs --explain <CODE>
```

Also accepts short keys mapped internally, for example:

- `field` -> `E1101`
- `method` -> `E1102`
- `variant` -> `E1103`
- `name` -> `E1002`
- `type` -> `E1003`
- `import` -> `E2001`

## Optimization and Build Environment

Default optimization selection order:

1. `GOST_OPT_LEVEL`
2. `GOST_OPT`
3. `GOST_RELEASE` (`0` => `O0`, otherwise release default)

Compiler path selection:

1. `GOST_CC`
2. `gcc` if available
3. `clang`

`llc` path selection:

1. `GOST_LLC`
2. sibling of `GOST_CC` if found
3. `llc`

Additional knobs:

- `GOST_NATIVE=0|1`
- `GOST_LDFLAGS`
- `GOST_RUST_TARGET`
- `GOST_STATS=1` (runtime feature)

## Exit Codes

- `0`: success
- non-zero (`1` in most paths): failure
