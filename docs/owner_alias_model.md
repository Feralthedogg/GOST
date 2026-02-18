# Owner/Alias Handle Model

## 1. Purpose

This model replaces the need for user-visible lifetime parameters.
It keeps the language simple while preserving strong safety guarantees for escaped data.

The model is based on two persistent handle types:

- `own[T]`: unique, move-only owner handle
- `alias[T]`: shared, read-only handle

Existing `ref[T]` / `mutref[T]` remain temporary view types only.

## 2. Type Layer

The language has four access layers:

1. `mutref[T]`: temporary mutable view (non-escaping)
2. `ref[T]`: temporary shared view (non-escaping)
3. `own[T]`: persistent unique handle (escaping allowed)
4. `alias[T]`: persistent shared handle (escaping allowed)

No lifetime syntax is introduced.
No region inference is introduced.

## 3. Class Mapping (SSOT)

`TypeClass` mapping is fixed as:

- `own[T]` -> `Linear`
- `alias[T]` -> `Copy`
- `ref[T]`, `mutref[T]` -> `View`

This mapping is the single source of truth used by sema, MIR, and codegen.

## 4. Escape Rules

`ref[T]` and `mutref[T]` MUST NOT escape expression/block scope.
They are forbidden in:

- struct fields
- enum payload fields
- function return types
- global variable types
- channel element types
- map key/value types
- array/slice element types (persistent storage positions)

`own[T]` and `alias[T]` MAY be used in all escaping positions above.

## 5. Conversion and Capability Rules

The capability lattice is intentionally one-way:

- `own[T]` -> `alias[T]` is allowed via `freeze`
- `alias[T]` -> `own[T]` is forbidden

No implicit two-way upgrade/downgrade is allowed.
No hidden mutability escalation is allowed.

### 5.1 Core operations

The model defines these primitive operations (surface syntax or intrinsic form):

- `own_new[T](value: T) -> own[T]`
- `own_borrow[T](x: ref[own[T]]) -> ref[T]`
- `own_borrow_mut[T](x: mutref[own[T]]) -> mutref[T]`
- `freeze[T](x: own[T]) -> alias[T]`
- `alias_borrow[T](x: ref[alias[T]]) -> ref[T]`
- `own_into_value[T](x: own[T]) -> T` (move out)

`freeze` consumes `own[T]`. After freezing, the original `own` value is moved.

## 6. Borrowing Rules

Borrow checking remains lexical and local, with no lifetime annotations.

For `own[T]`:

- shared borrows (`own_borrow`) can coexist
- mutable borrow (`own_borrow_mut`) requires no active borrows
- while mutable borrow is active, no other borrows are allowed

For `alias[T]`:

- only shared borrow (`alias_borrow`) is allowed
- mutable borrow from `alias[T]` is forbidden

## 7. Safety Contract

This model guarantees:

- no escaped dangling views
- no mutable aliasing through persistent shared handles
- no hidden ownership resurrection

It intentionally does not provide:

- lifetime polymorphism
- user-visible region algebra
- alias-to-owner recovery

## 8. Runtime Representation

`own[T]` and `alias[T]` share the same runtime object layout as current shared payload objects.
The distinction is static (type/checker-level), not ABI-level.

Required runtime primitives:

- allocate payload object
- retain/release shared object
- query uniqueness for mutable access
- get payload pointer

Current runtime functions already match this contract:

- `__gost_shared_new`
- `__gost_shared_inc`
- `__gost_shared_dec`
- `__gost_shared_is_unique`
- `__gost_shared_get_ptr`

## 9. MIR Invariants (Normative)

MIR MUST satisfy:

1. Any value with `TypeClass::View` does not escape its defining scope.
2. `freeze` consumes its `own` input local (linear move).
3. `own_borrow_mut` is only emitted from mutable access to `own`.
4. No MIR edge may re-materialize `own` from `alias`.
5. Drop scheduling treats `own` as linear and `alias` as copy-like refcounted handle.

Backend assumes valid MIR and MUST NOT emit user-facing reject diagnostics for these invariants.

## 10. Codegen Contract

Codegen lowering rules:

- `own_new` -> `__gost_shared_new`
- `freeze` move -> no runtime conversion; ownership state changes in MIR/sema only
- copy of `alias` -> retain (`__gost_shared_inc`)
- drop of `own` or `alias` -> release (`__gost_shared_dec`)
- `own_borrow_mut` -> uniqueness guard (`__gost_shared_is_unique == 1`) + payload pointer
- read borrows -> payload pointer only

Codegen may assert invariants but must not be the first rejection point.

## 11. Sema Diagnostics (Normative)

Sema is the only rejection boundary for user code.
Minimum required diagnostics:

- `view value cannot escape; use own[T] or alias[T]`
- `cannot mutably borrow alias[T]`
- `cannot convert alias[T] to own[T]`
- `use after move of own[T]`
- `freeze consumes owner handle`

## 12. Interaction With Existing Features

### 12.1 Generics

`own[T]` and `alias[T]` are normal type constructors.
No lifetime-generic parameters are added.

### 12.2 Collections

- map keys: unchanged runtime key-kind policy
- map values: `own` and `alias` allowed
- slice/array elements: `own` and `alias` allowed

### 12.3 Concurrency (`go`, `chan`)

- moving `own[T]` across goroutines is allowed (single owner transfer)
- sending `alias[T]` is allowed (shared read handle)
- mutable access still requires `own_borrow_mut`

## 13. Surface Syntax Policy

The accepted model is syntax-agnostic at parser level.
Implementation MAY start with intrinsic forms and later add syntax sugar.

If sugar is added, it MUST remain equivalent to the primitive operations in Section 5.1.

## 14. Non-Goals

The following are explicitly out of scope for v1:

- borrow checker based on inferred region lifetimes
- higher-ranked lifetime constructs
- mutable shared handles
- automatic alias-to-owner recovery

## 15. Migration Policy

Existing rules that previously forced blanket rejection of escaped view usage are replaced by:

1. reject escaped `ref/mutref`
2. suggest `own/alias` conversion path

This is a hard policy change for new code and the reference behavior for compiler development.

## 16. Acceptance Criteria

The model is considered implemented when all are true:

1. `own/alias` types are recognized in sema and class-mapped as specified.
2. escaped view rejection occurs only in sema with mandated diagnostics.
3. MIR validates invariants in Section 9.
4. codegen consumes MIR without introducing new user-facing reject paths.
5. regression tests cover:
   - escaped view rejection
   - `own` move semantics
   - `freeze` one-way conversion
   - alias read-only behavior
   - mutable borrow uniqueness guard

