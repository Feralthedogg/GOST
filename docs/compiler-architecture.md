# Compiler Architecture

This document describes the implemented pipeline in this repository.

## 1. High-Level Pipeline

Entry point: `compile_to_llvm` in `src/compile.rs`.

Stages:

1. Parse source (`frontend/lexer.rs`, `frontend/parser.rs`)
2. Load/merge imports (module-aware resolution when `gost.mod` exists)
3. Front-end lowering/rewriting of selected high-level constructs (`lower_high_level_features`)
4. Semantic analysis (`sema/mod.rs`)
5. MIR lowering (`mir/lower.rs`)
6. MIR verification passes (`mir/passes.rs`)
7. LLVM emission from MIR (`codegen/mod.rs`, `codegen/emitter.rs`)

## 2. Frontend

### 2.1 Lexer

- tokenization with newline-driven semicolon insertion
- nested block comment support
- numeric literal normalization for prefixed ints

### 2.2 Parser

- builds AST from `frontend/ast.rs`
- handles imports, items, statements, expressions, patterns, and types
- parses layout modifiers (`repr`, `pack`, `bitfield`)

## 3. Semantic Analysis (SSOT)

Core type model: `src/sema/types.rs`.

Sema responsibilities:

- name/type resolution
- trait/impl consistency checks
- type checking and conversions
- borrow/linear-state checks
- ownership handle rules (`own`/`alias`)
- map key validation against runtime capabilities
- user-facing diagnostics

Design rule:

- user-code rejection happens in sema/front-end, not codegen.

## 4. MIR and Invariants

MIR is lowered from sema-validated program and then validated by passes:

- cleanup chain construction
- linear checks
- strict MIR verification
- backend-readiness verification

Backend assumes MIR is already valid and lowered.

Closure-specific lowering rules:

- Remaining closure literals are rewritten before sema/codegen boundary.
- Capturing closures are materialized as runtime closure objects (interface-compatible payload form).
- Placeholder fallback codepaths are removed; failed closure lowering is treated as internal error.

## 5. Codegen Contract

Codegen API: `emit_llvm_from_mir(program, mir)`.

Error split (`src/codegen/mod.rs`):

- `FrontendDiagnostic(String)`
- `InternalCompilerError(String)`

Current compile entry maps internal codegen errors to generic internal-compiler messages.

MIR-only rule:

- backend receives MIR, not high-level AST.
- high-level constructs that reach backend are invariant violations.
- backend handles closure runtime objects as interface-like values, not as AST-level closure syntax.

## 6. Runtime Boundary

LLVM output references runtime symbols declared in `src/codegen/mod.rs` prelude.

Runtime implementation:

- Rust runtime crate: `runtime/`
- architecture-specific assembly context switches in `runtime/*.asm`

Key runtime domains:

- strings/slices/shared handles
- map/chan/select primitives
- panic/recover
- OS/network intrinsics
- synchronization primitives

## 7. Modules and Dependency Resolution

Package resolver lives in `src/pkg/`.

- With `gost.mod`: dependency graph + lock/cache resolution
- Without `gost.mod`: legacy local/std import loading path

## 8. Testing Strategy

Main regression tests are embedded in `src/compile.rs` and under `test/*.gs`.

Coverage includes:

- diagnostics categories
- generics inference
- repr/layout behavior
- ownership/alias model
- map key kinds
- for/match lowering behavior
- concurrency/select/FFI paths

## 9. Guardrails

Script: `scripts/check_codegen_reject_strings.ps1`

Checks:

- blocks new user-facing reject strings in `src/codegen`
- rejects stale AST fallback hooks
- rejects legacy reject helper patterns

Purpose: enforce sema/codegen responsibility boundary.

## 10. Relevant Source Map

- CLI: `src/cli/mod.rs`
- Compile orchestration: `src/compile.rs`
- Frontend: `src/frontend/*`
- Sema: `src/sema/*`
- MIR: `src/mir/*`
- Codegen: `src/codegen/*`
- Runtime: `runtime/*`
- Std library: `std/*`
