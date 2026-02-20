// Purpose: Define crate-level module surface for compiler and tooling components.
// Inputs/Outputs: Re-exports internal modules for binaries/tests and integration entry points.
// Invariants: Public module boundaries should remain stable for internal callers.
// Gotchas: Keep module wiring consistent with src/main.rs and src/bin/gs.rs entry paths.

pub mod abi;
pub mod cli;
pub mod codegen;
pub mod compile;
pub mod frontend;
pub mod incremental;
pub mod intrinsics;
pub mod mir;
pub mod pkg;
pub mod sema;
