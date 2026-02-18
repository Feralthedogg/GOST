# Gost Documentation

This directory is the canonical documentation for the Gost language, toolchain, and standard library.
The goal is to keep docs aligned with the current compiler/runtime behavior in this repository.

## Start Here

- [Getting Started](getting-started.md)
- [CLI Reference](cli-reference.md)
- [Language Guide](language-guide.md)
- [Language Reference](language-reference.md)
- [Intrinsics Reference](intrinsics-reference.md)
- [Ownership Cheat Sheet](ownership-cheatsheet.md)
- [Modules and Packages](modules-and-packages.md)
- [Standard Library Reference](stdlib-reference.md)
- [Testing Guide](testing-guide.md)
- [Diagnostics](diagnostics.md)
- [Compiler Architecture](compiler-architecture.md)
- [Owner/Alias Model](owner_alias_model.md)

## Scope and Stability

- Documentation reflects the implementation under `src/`, `runtime/`, and `std/`.
- If behavior conflicts between docs and code, code is source of truth until docs are updated.
- `owner_alias_model.md` is a normative accepted spec for ownership/aliasing semantics.
- Current closure model is first-class `closure(...) -> ...` with sema-level view-capture rejection and specialized support for captured-closure flow through `fn(...)` paths.

## Conventions

- Code examples use `.gs` syntax.
- `Result[T, error]` and `(T, error)` both appear in codebase; prefer `Result[T, error]` in new docs and examples.
- Statements end by newline (automatic semicolon insertion). Explicit `;` in source is rejected.
