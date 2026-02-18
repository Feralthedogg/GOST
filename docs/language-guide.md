# Language Guide

This guide is example-first. For exact syntax and constraints, see [Language Reference](language-reference.md).

## 1) File Shape

Each file must start with `module <name>`.

```gs
module main

fn main() -> i32 {
    return 0
}
```

Imports support aliasing and selective import:

```gs
module main

import "std/fmt" as fmt { println }

fn main() -> i32 {
    fmt.println("hello")
    return 0
}
```

## 2) Variables and Constants

- `let` is mutable local/global binding.
- `const` is immutable compile-time constant.
- Type annotation is optional when inferrable.

```gs
module main

const LIMIT: i32 = 10
let global_counter = 0

fn main() -> i32 {
    let x = 1
    let y: i64 = 2
    global_counter = global_counter + x
    return y as i32
}
```

## 3) Functions and Methods

Basic function:

```gs
fn add(a: i32, b: i32) -> i32 {
    return a + b
}
```

Methods are defined in `impl` blocks. Receiver-style calls (`obj.method()`) are supported.

```gs
struct User {
    name: string
}

impl User {
    fn greet() -> string {
        return "hello ${self.name}"
    }
}
```

## 4) Structs, Enums, Match

```gs
struct Point {
    x: i32
    y: i32
}

enum Shape {
    Dot(Point)
    Empty
}

fn score(s: Shape) -> i32 {
    return match s {
        Shape.Dot(p) => p.x + p.y,
        Shape.Empty => 0,
    }
}
```

Pattern features include `_`, literal patterns, enum variant patterns, and or-patterns:

```gs
match v {
    0 | 1 => 10,
    _ => 20,
}
```

Guards are supported:

```gs
match x {
    Value.Ok(n) if n > 0 => n,
    _ => 0,
}
```

## 5) Control Flow

### if / else

`if` is an expression.

```gs
let sign = if x >= 0 { 1 } else { -1 }
```

### while / loop

```gs
let i = 0
while i < 3 {
    i += 1
}

loop {
    break
}
```

### for-in and range for

```gs
for v in xs {
    // value only
}

for idx, v in xs {
    // idx: i64, v: element
}

for v in 1..=3 {
    // inclusive range
}

for i, v in 1..=3 {
    // i: 0-based index, v: current range value
}
```

### Labeled loops

```gs
outer: for i in 0..10 {
    for j in 0..10 {
        if j == 3 {
            continue outer
        }
    }
}
```

## 6) Collections and Builtins

Slices and maps are created via intrinsics:

```gs
let xs = make_slice[i32](0 as i64, 0 as i64)
slice_push[i32](&mut xs, 10)
let n = slice_len[i32](&xs)

let m = make_map[string, i32](8 as i64)
map_set[string, i32](&mut m, "a", 1)
let got = map_get[string, i32](&m, "a")
```

Channels:

```gs
let ch = make_chan[i32](1)
send(ch, 42)
let v = recv(ch)
close(ch)
```

## 7) Errors and `?`

`Result[T, error]` and `?` are supported.

```gs
fn one() -> Result[i32, error] {
    return Result.Ok[i32, error](1)
}

fn use_it() -> Result[i32, error] {
    let v = one()?
    return Result.Ok[i32, error](v + 1)
}
```

## 8) Generics, Traits, Interfaces

Generic functions:

```gs
fn id[T](x: T) -> T {
    return x
}
```

Trait and impl:

```gs
trait Identity {
    fn id[T](x: T) -> T
}

copy struct User {
    id: i32
}

impl Identity for User {
    fn id[U](x: U) -> U {
        return x
    }
}
```

Trait object style is also available with `dyn Trait`:

```gs
fn run(v: dyn Identity, x: i32) -> i32 {
    return v.id(x)
}
```

## 9) Closures as First-Class Values

Closures are first-class and capture values at creation time.
For captured closures, `closure(...) -> ...` is the canonical type.

```gs
fn make_adder(base: i32) -> closure(i32)->i32 {
    let add: closure(i32)->i32 = |x: i32| x + base
    return add
}

fn main() -> i32 {
    let add = make_adder(2)
    return add(3) // 5
}
```

Captured closures can also flow through `fn(...)`-typed call paths; compiler uses lowering/specialization when needed.
This is supported, but for stored/returned captured closures, prefer explicit `closure(...)` type in user code:

```gs
fn apply(f: fn(i32)->i32, x: i32) -> i32 {
    return f(x)
}

fn main() -> i32 {
    let base: i32 = 1
    let add: fn(i32)->i32 = |x: i32| x + base
    return apply(add, 2) // 3
}
```

View captures are rejected by sema:

```gs
fn bad(v: ref[i32]) -> closure()->i32 {
    // error: view value cannot escape (captured by closure)
    return || *v
}
```

## 10) Ownership Without Lifetimes (`own`/`alias`)

Persistent ownership handles:

- `own[T]`: unique owner (move-like)
- `alias[T]`: shared read handle

Core intrinsics:

```gs
let o = own_new[i32](1)
{
    let p = own_borrow_mut[i32](&mut o)
    *p = 7
}
let a = freeze[i32](o)
let r = alias_borrow[i32](&a)
```

Full model: [owner_alias_model.md](owner_alias_model.md)

## 11) Concurrency: `go`, `defer`, `select`

```gs
go worker(1)
defer cleanup()

select {
    case recv(ch) => |v, ok| handle(v, ok),
    case send(ch, 3) => done(),
    case after(1000) => timeout(),
    default => idle(),
}
```

## 12) FFI and `extern`

```gs
extern "C" fn puts(msg: string) -> i32
extern "C" let errno: i32
```

Supported ABI names include `C`, `system`, `stdcall`, `fastcall`, `vectorcall`, `thiscall`, `win64`, `sysv64`, `aapcs`.

## 13) Useful Syntax Extras

- String interpolation: `"hello ${name}"`
- Pipeline call: `x |> f` (rewritten as `f(x)`)
- Cast: `expr as Type`
- Try: `expr?`

## 14) Layout Attributes

You can attach layout modifiers before `struct`/`enum`:

- `repr(C)`
- `repr(transparent)` (struct only, exactly one field)
- `repr(i8|i16|i32|i64|isize|u8|u16|u32|u64|usize)` (enum)
- `pack(N)` (requires `repr(C)`, struct)
- `bitfield` (requires `repr(C)`, struct)

Example:

```gs
repr(C) pack(1) struct Packed {
    a: u8
    b: u16
}
```
