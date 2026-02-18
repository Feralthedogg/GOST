# Intrinsics Reference

Single-source declaration: `src/intrinsics.rs` (`intrinsic_signature`).
Type/semantic checks are enforced in sema (`src/sema/mod.rs`, `check_intrinsic_call`).

## Conventions

- `TArgs`: type argument count requirement
- `Args`: value argument count requirement
- `Std-only`: callable only from standard library modules

## Core Language Intrinsics

| Name | TArgs | Args | Return | Std-only | Notes |
|---|---:|---:|---|---|---|
| `panic` | 0 | 1 | `unit` | no | argument must be `string` |
| `recover` | 0 | 0 | `error` | no | panic recovery value |
| `string_len` | 0 | 1 | `i64` | no | arg: `string` |
| `string_get` | 0 | 2 | `i32` | no | `(string, i64/i32)` |
| `string_slice` | 0 | 3 | `string` | no | `(string, start, len)` |
| `string_concat` | 0 | 2 | `string` | no | both args `string` |
| `string_from_byte` | 0 | 1 | `string` | no | arg: byte-ish int |

## Inline ASM Intrinsics

| Name | TArgs | Args | Return | Notes |
|---|---:|---:|---|---|
| `asm` | 0..1 | >=1 | inferred or explicit | unsafe context required |
| `asm_pure` | 0..1 | >=1 | inferred or explicit | unsafe context required |
| `asm_volatile` | 0..1 | >=1 | inferred or explicit | unsafe context required |
| `asm_label` | 0 | 1 | `unit` | literal string label |
| `asm_goto` | 0..1 | >=3 | inferred/explicit | unsafe context required |

`asm`/`asm_goto` validate template/constraints/label literals and operand count based on parsed constraint spec.

## Iterator Intrinsics

| Name | TArgs | Args | Return | Notes |
|---|---:|---:|---|---|
| `iter` | 0 | 1 | `iter[ref[T]]` | expects `&slice` (`&bytes` -> `u32`) |
| `iter_mut` | 0 | 1 | `iter[mutref[T]]` | expects `&mut slice` |
| `filter` | 0 | 2 | `iter[T]` | second arg must be function symbol `fn(T)->bool` |
| `map` | 0 | 2 | `iter[U]` | second arg must be function symbol `fn(T)->U` |

Iterator chain values are restricted to for-loop lowering path; escaping iterator values is rejected.

## Channel Intrinsics

| Name | TArgs | Args | Return | Notes |
|---|---:|---:|---|---|
| `make_chan` | 1 | 1 | `chan[T]` | cap: `i64`/`i32` |
| `__gost_chan_can_send` | 0 | 1 | `i32` | expects `chan[T]` |
| `__gost_chan_can_recv` | 0 | 1 | `i32` | expects `chan[T]` |
| `__gost_select_wait` | 0 | any | `i32` | std-internal select helper |

## Slice Intrinsics

| Name | TArgs | Args | Return | Notes |
|---|---:|---:|---|---|
| `make_slice` | 1 | 2 | `[]T` | `(len, cap)` as int |
| `slice_len` | 1 | 1 | `i64` | arg: `ref []T` |
| `slice_get_copy` | 1 | 2 | `T` | requires `T: Copy` |
| `slice_set` | 1 | 3 | `unit` | arg0: `mutref []T` |
| `slice_ref` | 1 | 2 | `ref[T]` | element borrow |
| `slice_mutref` | 1 | 2 | `mutref[T]` | mutable element borrow |
| `slice_push` | 1 | 2 | `unit` | arg0: `mutref []T` |
| `slice_pop` | 1 | 1 | `(T, bool)` | arg0: `mutref []T` |

## Shared/Owner/Alias Intrinsics

| Name | TArgs | Args | Return | Notes |
|---|---:|---:|---|---|
| `shared_new` | 1 | 1 | `shared[T]` | arg type must be `T` |
| `shared_get` | 1 | 1 | `ref[T]` | arg: `ref shared[T]` |
| `shared_get_mut` | 1 | 1 | `mutref[T]` | arg: `ref/mutref shared[T]` |
| `own_new` | 1 | 1 | `own[T]` | owner construction |
| `own_borrow` | 1 | 1 | `ref[T]` | arg: `ref/mutref own[T]` |
| `own_borrow_mut` | 1 | 1 | `mutref[T]` | arg: `mutref own[T]`; alias mutation rejected |
| `freeze` | 1 | 1 | `alias[T]` | consumes `own[T]` |
| `alias_borrow` | 1 | 1 | `ref[T]` | arg: `ref/mutref alias[T]` |
| `own_into_value` | 1 | 1 | `T` | consumes `own[T]`; alias->own rejected |

Related diagnostics:

- `view value cannot escape; use own[T] or alias[T]`
- `cannot mutably borrow alias[T]`
- `cannot convert alias[T] to own[T]`
- `freeze consumes owner handle`

See: `docs/owner_alias_model.md` and `docs/ownership-cheatsheet.md`.

## Map Intrinsics

| Name | TArgs | Args | Return | Notes |
|---|---:|---:|---|---|
| `make_map` | 2 | 1 | `map[K,V]` | cap argument; key must be runtime-supported |
| `map_get` | 2 | 2 | `(V, bool)` | arg0: `ref map[K,V]` |
| `map_set` | 2 | 3 | `unit` | arg0: `mutref map[K,V]` |
| `map_del` | 2 | 2 | `bool` | arg0: `mutref map[K,V]` |
| `map_len` | 2 | 1 | `i64` | arg0: `ref map[K,V]` |

## Standard Library Internal Intrinsics

These are intentionally gated to std modules (`self.is_std` checks in sema):

- `__gost_println`
- `__gost_error_new`, `__gost_error_message`
- `__gost_singleton_acquire`
- `__gost_now_ms`, `__gost_process_exit`
- `__gost_sync_*`
- `__gost_os_*`
- `__gost_net_*`
- `__gost_select_wait`

## OS/Net/Sync Internal Families (Arity Summary)

- `__gost_sync_*`: mutex/waitgroup/once handles (`i64`-like)
- `__gost_os_*`: process/fs/env helpers, mostly string-based args
- `__gost_net_*`: socket/http/ws helpers with string payload/address and handle args

Use the std wrappers in `std/os`, `std/net`, `std/sync`, etc. instead of calling these directly.

## Validation Order

1. Name must be a known intrinsic (`is_intrinsic_name`)
2. Type-arg arity check
3. Value-arg arity check
4. Std-only gate check
5. Per-intrinsic semantic/type checks

If arity fails, diagnostics come from `src/intrinsics.rs` error strings.
If semantic typing fails, diagnostics come from `src/sema/mod.rs`.
