# Commenting Guidelines

This document defines the standard comment style for the compiler/runtime codebase.

## 1) Write comments for `why`, not `what`

- Code and names should explain what happens.
- Comments should explain:
  - why this design/ordering is required
  - why alternatives are unsafe/incorrect/too expensive
  - which invariant or assumption this code depends on

Good:

```rust
// Why: This pass must run before SSA rewrite because dominance would be invalidated later.
```

Bad:

```rust
// Iterate blocks.
```

## 2) File-level contract header (max ~10 lines)

At the top of each module/file, keep this fixed header:

```text
// Purpose: ...
// Inputs/Outputs: ...
// Invariants: ...
// Gotchas: ...
```

## 3) Function comments use only contract fields

Use only the needed fields from:

- `Precondition`
- `Postcondition`
- `Side effects`

Example:

```rust
// Precondition: cfg has no unreachable blocks.
// Postcondition: returns dominator tree for all remaining blocks.
// Side effects: none.
```

## 4) Put structural invariants near types

For IR/type/CFG structs:

- document field meaning at declaration site
- group critical invariants above the struct definition

## 5) Mandatory tags for risky code paths

For unsafe blocks, non-obvious optimizations, and cache invalidation logic:

- `SAFETY: ...`
- `PERF: ...`
- `INVALIDATION: ...`

Template:

```rust
// SAFETY: ...
// PERF: ...
// INVALIDATION: ...
```

## 6) TODO format

Use condition + owner + acceptance criteria:

```text
// TODO(owner): switch to sparse set when blocks > 10k (measured 2x faster).
```

Do not use bare TODOs like `// TODO: fix`.

## 7) Avoid duplicating long specs in code comments

Keep long explanations in `docs/`, and reference them in code:

```text
// See docs/commenting-guidelines.md#5-mandatory-tags-for-risky-code-paths
```

## 8) Diagnostic sites should include user-facing rationale

Near user-facing diagnostics, explain:

- why this is the best reporting point
- what corrective action the user should take

## 9) Style and language

- All source comments must be written in English.
- Keep wording concise and consistent.
- Prefer wrapping around 80-100 columns.

## 10) Practical examples (copy/paste patterns)

### Example A: pass ordering

Good:

```rust
// Why: Cleanup-chain synthesis must run before linear checks because it rewrites terminators.
```

Bad:

```rust
// Run cleanup pass.
```

### Example B: unsafe pointer use

Good:

```rust
// SAFETY: `pd` was loaded from a handle table entry validated by (id, generation).
// SAFETY: Acquire failure means descriptor retirement; caller must invalidate cache.
```

Bad:

```rust
// SAFETY: This is safe.
```

### Example C: performance trade-off

Good:

```rust
// PERF: Keep per-thread weak-handle cache to avoid shard-lock traffic on hot net paths.
```

Bad:

```rust
// PERF: faster.
```

### Example D: cache invalidation

Good:

```rust
// INVALIDATION: Any close/remove path must clear id+generation keyed cache entries.
```

Bad:

```rust
// clear cache.
```

### Example E: function contract

Good:

```rust
// Precondition: MIR strict verifier already succeeded for this function.
// Postcondition: Returned body contains only backend-supported statements.
```

Bad:

```rust
// Validates MIR.
```
