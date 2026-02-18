# Testing Guide

This project uses layered tests:

1. Rust unit/regression tests (`cargo test --lib`)
2. `.gs` fixture programs under `test/`
3. CI guardrails for codegen responsibility boundaries

## 1. Fast Path Commands

Run library tests (includes compiler pipeline regressions in `src/compile.rs`):

```powershell
cargo test --lib
```

Build tests without execution:

```powershell
cargo test --lib --no-run
```

Run a specific test by name:

```powershell
cargo test --lib generic_inference_rejects_conflicting_argument_types
```

Run closure-focused regressions quickly:

```powershell
cargo test --lib closure_
```

## 2. CI Baseline

CI workflow: `.github/workflows/codegen-guardrails.yml`

Current CI checks:

1. `cargo test --lib`
2. `scripts/check_codegen_reject_strings.ps1`
3. `scripts/check_string_cmp_codegen.ps1`

Guardrail script validates:

- no stale AST fallback markers in compile path
- no legacy reject helpers in codegen
- no new user-facing reject strings in `src/codegen` unless baseline is intentionally updated
- no invalid LLVM string comparisons (`icmp ... %string`) in selected regression fixtures

## 3. Regression Tests in `src/compile.rs`

Main regression suite location:

- `src/compile.rs` (`#[cfg(test)]` module)

Pattern:

- temporary `.gs` source emitted in OS temp dir
- compile through `compile_to_llvm(...)`
- assert success/failure and message snippets
- optionally assert LLVM text fragments

Use this when adding compiler behavior/regression tests.

## 4. Fixture Programs in `test/`

`test/*.gs` serves as scenario corpus:

- parser/lexer cases
- sema/type-system cases
- MIR/codegen regressions
- std module integration samples

Examples:

- `test/mir_match_or_try_test.gs`
- `test/trait_generic_method_test.gs`
- `test/map_struct_key_runtime_test.gs`
- `test/closure_maturity_test.gs`
- `test/std_*_test.gs`

When adding a new feature, include at least one focused fixture.

## 5. Choosing Test Location

Add to `src/compile.rs` tests if:

- behavior should be enforced in CI with explicit assertions
- you need direct access to compiler internals (`compile_to_llvm` output)

Add to `test/*.gs` if:

- scenario is broader language sample/integration case
- you want reusable fixture source for manual runs and future harnesses

Use both for non-trivial language/runtime changes.

## 6. Failure Test Guidelines

For negative tests:

- assert key diagnostic substring(s), not entire formatted output
- include code (`E100x`, etc.) when the path emits it
- avoid brittle dependence on exact whitespace or colorized formatting

Example strategy (existing style):

- `expect_err(...)`
- `assert!(err.contains("...") ...)`

## 7. Codegen Boundary Rule

Important project invariant:

- user-code reject diagnostics should terminate in sema/front-end
- codegen should mostly assert internal invariants on already-valid MIR

When adding tests, include at least one assertion that invalid user source does not surface backend-internal-style failures unexpectedly.

## 8. Docs/Diagnostics Sync

If you add a new stable diagnostic code:

1. define/use code in diagnostics path
2. update `docs/diagnostics.md`
3. verify `gs explain <code>` resolves correctly

## 9. Local Workflow Recommendation

1. Write/adjust `.gs` fixture in `test/`
2. Add/adjust focused Rust regression in `src/compile.rs` if needed
3. Run `cargo test --lib`
4. Run `./scripts/check_codegen_reject_strings.ps1` (PowerShell)
5. Run `./scripts/check_string_cmp_codegen.ps1` (PowerShell)

## 10. Common Pitfalls

- Asserting full diagnostic text (too brittle)
- Adding user-facing reject strings in codegen instead of sema
- Forgetting to update diagnostics docs for new code families
- Introducing feature behavior without both positive and negative coverage
