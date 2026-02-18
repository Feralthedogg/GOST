# Language Reference

This document describes the implemented language behavior in `src/frontend`, `src/sema`, and `src/mir`.

## 1. Lexical Structure

### 1.1 Identifiers

- Start: ASCII letter or `_`
- Continue: ASCII letter, digit, or `_`

### 1.2 Keywords

`module`, `import`, `pub`, `private`, `const`, `type`, `extern`, `unsafe`, `fn`, `struct`, `enum`, `copy`, `let`, `if`, `else`, `match`, `while`, `loop`, `for`, `in`, `mut`, `return`, `break`, `continue`, `select`, `case`, `default`, `go`, `defer`, `send`, `recv`, `close`, `after`, `true`, `false`, `nil`, `interface`, `trait`, `impl`, `as`

### 1.3 Comments

- Line comment: `// ...`
- Block comment: `/* ... */`
- Block comments are nested.

### 1.4 Semicolon Insertion

- Newline may insert statement terminators automatically (Go-like).
- Explicit `;` tokens in source are rejected by parser diagnostics.

### 1.5 Literals

- Integer: decimal, `0x`, `0o`, `0b` (underscore separators allowed)
- Float: decimal with optional exponent
- String: `"..."` with escapes (`\n`, `\r`, `\t`, `\\`, `\"`, `\'`)
- Char: `'x'` and escaped char forms
- Bool: `true`, `false`
- Nil: `nil`

## 2. Source File Structure

A file must start with:

```gs
module <name>
```

Then optional imports and item declarations.

## 3. Imports

```gs
import "path/to/pkg"
import "path/to/pkg" as alias
import "path/to/pkg" as alias { name1, name2 }
```

- `as alias` enables namespaced access (`alias.func(...)`).
- `{ ... }` restricts imported symbols to the listed names.

## 4. Top-Level Items

### 4.1 Visibility

- `pub`
- `private` (or `priv`)
- default: public

### 4.2 Functions

```gs
fn name[T](a: A, b: B, ...) -> R { ... }
extern "C" fn name(a: A) -> R
unsafe fn name(...) -> R { ... }
```

- Variadic marker `...` supported for user functions and function pointers.
- `extern fn` uses declaration form (no body, terminated declaration).

### 4.3 Extern Globals

```gs
extern "C" let errno: i32
```

### 4.4 Type Alias

```gs
type Name = SomeType
```

### 4.5 Trait and Impl

```gs
trait MyTrait {
    fn f[T](x: T) -> T
    fn g(x: i32) -> i32 { ... }
}

impl MyType {
    fn method() -> i32 { ... }
}

impl MyTrait for MyType {
    fn f[U](x: U) -> U { ... }
}
```

- Trait methods can be generic.
- Default trait method bodies are supported.
- In `impl` methods, receiver-style behavior is provided; method calls use `obj.method(...)`.

### 4.6 Struct / Enum

```gs
copy struct S {
    x: i32
    y: i32
}

enum E {
    A
    B(i32, string)
}
```

- `copy` marks copy-like ADTs (validated by sema).
- Struct fields and enum variants are line-terminated declarations (semicolon insertion model).

### 4.7 Global Let / Const

```gs
let G = init_expr
const C: i32 = 3
```

- Global `let` may use inferred type and runtime initializer.
- Global constants must be compile-time evaluable.

## 5. Layout Modifiers

Applicable before `struct`/`enum` (and also accepted around `copy struct|enum` forms):

- `repr(C)`
- `repr(transparent)`
- `repr(i8|i16|i32|i64|isize|u8|u16|u32|u64|usize)`
- `pack(N)`
- `bitfield`

Sema constraints:

- conflicting `repr(...)` combinations are rejected
- `pack(N)` and `bitfield` require `repr(C)`
- `repr(transparent)` is struct-only and requires exactly one field
- integer `repr(...)` is enum-only
- unknown `repr(...)` names are currently rejected as unsupported

## 6. Types

### 6.1 Builtins

`bool`, `i8`, `i16`, `i32`, `i64`, `isize`, `u8`, `u16`, `u32`, `u64`, `usize`, `f32`, `f64`, `char`, `unit`, `string`, `error`, `bytes`

### 6.2 Composite and Special Types

- `ref[T]`, `mutref[T]`
- `own[T]`, `alias[T]`, `shared[T]`
- `[]T` (slice), `[N]T` (array)
- `map[K, V]`
- `result[T, E]` / `Result[T, E]`
- `chan[T]`
- `interface`
- trait object syntax: `dyn TraitName`
- tuple: `(A, B, ...)`
- function pointer: `fn(A, B, ...)->R`
- closure object: `closure(A, B, ...)->R`

`(T, error)` is normalized in sema to result-like behavior where applicable.

## 7. Statements

- `let name [: Type] = expr`
- `const name [: Type] = expr`
- assignment and compound assignment (`=`, `+=`, `-=`, `*=`, `/=`, `%=` and bitwise/shift variants)
- expression statement
- `return [expr]`
- `break [label]`
- `continue [label]`
- `while cond { ... }`
- `loop { ... }`
- `for v in iterable { ... }`
- `for idx, v in iterable { ... }`
- `for v in start..end { ... }` / `start..=end`
- `for idx, v in start..end { ... }`
- `select { case ... => ..., default => ... }`
- `go call_expr`
- `defer call_expr`

Labels:

```gs
name: for ... { ... }
```

## 8. Expressions

### 8.1 Primary

- literals, identifier, tuple
- struct literal: `Type { field = expr, ... }`
- array literal: `[expr, ...]`
- typed slice literal: `[]T{expr, ...}`
- block expression: `{ stmts; tail_expr }`
- `unsafe { ... }`
- `if` expression
- `match` expression
- closure: `|x: T, y| expr_or_block`
- channel ops: `send(ch, v)`, `recv(ch)`, `close(ch)`, `after(ms)`

### 8.2 Postfix

- call: `f(a, b)`
- generic call: `f[T](a)`
- field: `obj.field`
- index: `obj[i]`

### 8.3 Unary

- `!`, `-`, `~`
- borrow: `&expr`, `&mut expr`
- deref: `*expr`
- cast: `expr as T`
- try: `expr?`

### 8.4 Binary Operators (low -> high)

1. `||`
2. `&&`
3. `|`
4. `^`
5. `&` and pipeline `|>`
6. `==`, `!=`
7. `<`, `<=`, `>`, `>=`
8. `<<`, `>>`
9. `+`, `-`
10. `*`, `/`, `%`

Pipeline rewriting:

- `x |> f` -> `f(x)`
- `x |> f(a, b)` -> `f(x, a, b)`

### 8.5 Pattern Syntax

- `_`
- literal `true/false/int`
- identifier binding
- enum variant pattern: `Enum.Variant(a, b)`
- or-pattern: `p1 | p2 | ...`
- optional guard: `pattern if cond => ...`

### 8.6 String Interpolation

Inside string literals:

```gs
"hello ${expr}"
```

Lowered to concatenation calls.

### 8.7 Inline Assembly

Supported parser-level forms include intrinsic-style `asm` and colon-style `asm volatile/goto (...)` with operand sections.

### 8.8 Closure Value Semantics

- Closure literals are first-class values.
- Captures are frozen at closure creation time (capture-by-value semantics in lowered form).
- Closure values can be stored in locals, struct fields, arrays, returned from functions, and passed as arguments.
- `closure(...) -> ...` is the stable type form for captured closures.
- `fn(...) -> ...` and `closure(...) -> ...` are call-compatible in sema call checking.
- For `fn(...)`-typed paths, compiler may use specialization/lowering to support captured closures where a plain symbol conversion is not possible.
- View captures (`ref`/`mutref`) are rejected in sema as escape violations.

## 9. Semantic Rules

### 9.1 Type Classes

Compiler classifies types as:

- `Copy`
- `Linear`
- `View`

`ref`/`mutref` are view types.

### 9.2 View Escape

View values (`ref`, `mutref`) cannot escape to persistent positions (e.g., struct fields, enum payloads, return types, extern globals). Use `own[T]` / `alias[T]` for escaping handles.
Captured closure environments are persistent values, so capturing a view into a closure is rejected by the same rule.

### 9.3 Ownership Handles

`own[T]`/`alias[T]` follow the accepted owner/alias model in [owner_alias_model.md](owner_alias_model.md).

### 9.4 Map Key Support

Map keys are accepted when runtime map can represent them. Supported families include:

- integer-like keys (including fieldless enum tags)
- `bool`, `char`, `string`, `error`
- function pointers and selected handle-like keys
- bytewise-comparable composite keys (struct/tuple/array/result/enum payload forms) via key callbacks

If unsupported, sema reports map-key diagnostics.

### 9.5 Recursive Types

- Direct recursive struct value cycles are rejected.
- Recursion through indirection/container forms is allowed.
- Enums are modeled with out-of-line payload representation in backend path.

### 9.6 For-In Iterable Forms

Accepted iterables include slice/array/map/bytes/string/iter and ref/mutref variants of supported containers.

### 9.7 `go` / `defer` Restrictions

- `go` expects a direct call expression and rejects explicit type args on the `go` call site.
- `defer` expects a direct call expression.

### 9.8 Diagnostics Boundary

User-code rejection should happen in sema/front-end diagnostics. Codegen assumes valid MIR and treats violations as internal compiler errors/invariants.

### 9.9 Trait Objects and Dynamic Dispatch

- `dyn TraitName` is accepted in type positions and treated as trait-object intent.
- Cast checks validate that source type satisfies required trait method signatures.
- Method calls through trait objects dispatch dynamically via interface-based method resolution.

## 10. Supported Extern ABIs

Case-insensitive names:

- `C`, `system`, `stdcall`, `fastcall`, `vectorcall`, `thiscall`, `win64`, `sysv64`, `aapcs`

## 11. Compilation Pipeline Contract

The production path is MIR-only:

1. parse (`frontend`)
2. semantic analysis (`sema`)
3. MIR lowering + MIR verification passes
4. LLVM emission from MIR (`codegen`)

Codegen does not own high-level language rejection logic.
