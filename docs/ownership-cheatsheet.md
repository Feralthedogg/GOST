# Ownership Cheat Sheet

This is a practical companion to the normative spec in `docs/owner_alias_model.md`.

## 1. Mental Model

- `ref[T]`, `mutref[T]`: temporary views (non-escaping)
- `own[T]`: unique owner handle (linear)
- `alias[T]`: shared read handle (copy-like)

If a value may escape scope (field/return/global/etc.), use `own[T]` or `alias[T]`, not raw views.

## 2. Core Intrinsics

```gs
own_new[T](value: T) -> own[T]
own_borrow[T](x: ref[own[T]]) -> ref[T]
own_borrow_mut[T](x: mutref[own[T]]) -> mutref[T]
freeze[T](x: own[T]) -> alias[T]
alias_borrow[T](x: ref[alias[T]]) -> ref[T]
own_into_value[T](x: own[T]) -> T
```

## 3. Allowed and Forbidden Conversions

Allowed:

- `own[T] -> alias[T]` via `freeze`
- borrow from `own` / borrow from `alias`

Forbidden:

- `alias[T] -> own[T]`
- mutable borrow from `alias[T]`

## 4. Typical Patterns

## 4.1 Build mutable, then freeze

```gs
let o = own_new[i32](1)
{
    let p = own_borrow_mut[i32](&mut o)
    *p = 7
}
let a = freeze[i32](o)
let r = alias_borrow[i32](&a)
```

## 4.2 Move out from owner

```gs
let o = own_new[i32](9)
let v = own_into_value[i32](o)
```

## 4.3 Escaping fields/returns

```gs
struct S {
    owner: own[i32]
    ro: alias[i32]
}
```

This is valid. `ref`/`mutref` in such escaping positions are rejected.

## 5. Frequent Errors and Fixes

`view value cannot escape; use own[T] or alias[T]`

- Cause: storing/returning `ref` or `mutref`
- Fix: materialize as `own` or `alias`

`cannot mutably borrow alias[T]`

- Cause: `own_borrow_mut`-style mutation on alias path
- Fix: keep mutable phase in `own` before freezing

`cannot convert alias[T] to own[T]`

- Cause: trying to regain unique owner from shared alias
- Fix: redesign API to keep ownership transfer explicit

`freeze consumes owner handle`

- Cause: using non-owner input for `freeze` or reusing moved owner
- Fix: pass `own[T]` and treat it as moved after freeze

`use after move of own[T]`

- Cause: reusing owner after move-consume operations
- Fix: clone value before move (if copy type), or restructure flow

## 6. Design Tips

- Keep mutation boundaries short and explicit.
- Freeze late: perform all writes before `freeze`.
- Public APIs returning read-shared data should prefer `alias[T]`.
- Use `own[T]` for transfer-of-ownership semantics across functions/goroutines.

## 7. Quick Decision Table

- Need shared read access only across scopes? -> `alias[T]`
- Need unique mutable ownership across scopes? -> `own[T]`
- Need temporary borrow in expression/block? -> `ref[T]` / `mutref[T]`

## 8. Anti-Patterns

- Returning `ref[T]` from function boundaries
- Embedding `mutref[T]` in structs/enums/globals
- Freezing too early then attempting mutation
- Treating alias as recoverable owner

## 9. See Also

- Spec: `docs/owner_alias_model.md`
- Intrinsics details: `docs/intrinsics-reference.md`
- Full language constraints: `docs/language-reference.md`
