module main

import "std/fmt" as fmt { println }

pub const PUBLIC_CONST: i32 = 7
private const PRIVATE_CONST: i32 = 9
let GLOBAL_I64: i64 = 123

extern "C" fn printf(fmt: string, ...) -> i32
extern "C" let errno: i32
extern "system" fn c_system(x: i64) -> i64
extern "win64" fn c_win64(x: i64) -> i64
extern "sysv64" fn c_sysv64(x: i64) -> i64

type ID = i64
type Mapper = fn(i32) -> i32
trait AnyBox {}

repr(C) pack(1) struct PackedPair {
    a: u8
    b: u16
}

repr(C) bitfield struct FlagsBits {
    a: u32
    b: u32
}

repr(C) enum CTag {
    A
    B
}

struct User {
    name: string
    age: i32
}

copy struct Pair {
    a: i32
    b: i32
}

enum Paint {
    Red
    Rgb(i32, i32, i32)
}

copy enum Tiny {
    One
    Two
}

impl User {
    pub fn greet() -> string {
        return "hello ${self.name}"
    }

    private fn age_plus(delta: i32) -> i32 {
        return self.age + delta
    }
}

pub fn exported_helper(v: i32) -> i32 {
    return v + 1
}

fn add(x: i32, d: i32) -> i32 {
    return x + d
}

fn plus1(x: i32) -> i32 {
    return x + 1
}

fn inc_i64(x: i64) -> i64 {
    return x + 1 as i64
}

fn dbl_i64(x: i64) -> i64 {
    return x * 2 as i64
}

fn id[T](v: T) -> T {
    return v
}

fn pick_left[T, U](a: T, _b: U) -> T {
    return a
}

fn apply_map(m: Mapper, x: i32) -> i32 {
    return m(x)
}

fn pair_ok() -> (i32, error) {
    return (3, nil)
}

fn res_ok() -> Result[i32, error] {
    return Result.Ok(4)
}

fn try_combo() -> error {
    let a = pair_ok()?
    let b = res_ok()?
    let s = a + b
    if s > 0 {
        return nil
    }
    return nil
}

fn async_send(ch: chan[i32]) {
    send(ch, 77)
}

fn defer_mark() {
    fmt.println("defer: end")
}

unsafe fn asm_probe(x: i64) -> i64 {
    asm volatile("test %0, %0" : : "r"(x) : "cc")
    let y: i64 = asm("nop")
    let z: i64 = asm[i64]("mov $1, $0", "=r,r", x)
    let _u: unit = asm[unit]("nop", "")
    return y + z - z
}

fn check_enum_match(p: Paint) -> i32 {
    return match p {
        Paint.Red => 1,
        Paint.Rgb(r, g, b) if r > 0 => r + g + b,
        Paint.Rgb(_, _, _) => 0,
        _ => -1,
    }
}

fn check_guard_or(n: i64) -> i64 {
    return match n {
        1 | 2 if n == 0 as i64 => 99 as i64,
        1 | 2 => 10 as i64,
        3 if n > 2 as i64 => 20 as i64,
        3 | 4 => 30 as i64,
        _ => 0 as i64,
    }
}

fn main() -> i32 {
    fmt.println("syntax smoke start")
    defer defer_mark()

    if PUBLIC_CONST != 7 || PRIVATE_CONST != 9 {
        return 1
    }
    if GLOBAL_I64 != 123 as i64 {
        return 2
    }

    let logic_ok = (true && !false) || false
    if !logic_ok {
        return 3
    }
    let chv: char = 'x'
    if chv != 'x' {
        return 4
    }
    let f32v: f32 = 1.25
    let f64v: f64 = 1.25
    if f32v <= 1.0 as f32 || f64v <= 1.0 {
        return 5
    }

    let i8v: i8 = 1
    let i16v: i16 = 2
    let u8v: u8 = 3
    let u16v: u16 = 4
    let isz: isize = 5
    let usz: usize = 6
    let _int_sum: i64 =
        (i8v as i64) + (i16v as i64) + (u8v as i64) + (u16v as i64) + (isz as i64) + (usz as i64)

    let bits: u32 = 1
    bits <<= 1 as u32
    bits |= 3 as u32
    bits &= 7 as u32
    bits ^= 1 as u32
    bits += 2 as u32
    bits -= 1 as u32
    bits *= 3 as u32
    bits /= 2 as u32
    bits %= 3 as u32
    let inv = ~bits
    let _shifted = (inv as i64) >> (1 as i64)

    let block_v = { let k = 1
 k + 2 }
    if block_v != 3 {
        return 6
    }
    let tag = if block_v == 3 { "ok" } else { "bad" }
    if string_len(tag) != 2 as i64 {
        return 7
    }

    let _tp = (1, 2)

    let p = Pair { a = 1, b = 2 }
    let ra = &p.a
    let rb = &p.b
    if *ra + *rb != 3 {
        return 8
    }

    let _packed = PackedPair { a = 1 as u8, b = 2 as u16 }
    let _flags = FlagsBits { a = 1 as u32, b = 2 as u32 }
    let _ctag = CTag.A
    let _tiny = Tiny.One

    let u = User { name = "gost", age = 10 }
    let greeting = u.greet()
    if string_len(greeting) == 0 as i64 {
        return 9
    }
    if u.age_plus(2) != 12 {
        return 10
    }

    let idv = id(42 as i64)
    if idv != 42 as i64 {
        return 11
    }
    let left = pick_left(42 as i64, 7 as u32)
    if left != 42 as i64 {
        return 111
    }
    let mapped = apply_map(plus1, 9)
    if mapped != 10 {
        return 12
    }
    let alias_id: ID = 5 as ID
    if alias_id != 5 as i64 {
        return 13
    }

    let base: i64 = 40
    let addc = |x: i64| x + base
    let out1 = addc(2 as i64)
    let nested = |v: i64| (|w: i64| w + base)(v)
    let out2 = nested(3 as i64)
    if out1 != 42 as i64 || out2 != 43 as i64 {
        return 14
    }

    let piped = 3 as i64 |> inc_i64 |> dbl_i64
    if piped != 8 as i64 {
        return 15
    }

    let sum: i64 = 0
    for i in 1 as i64..=3 as i64 {
        sum += i
    }
    for j in 0 as i64..3 as i64 {
        sum += j
    }
    if sum != 9 as i64 {
        return 16
    }

    let xs_index = make_slice[i32](0 as i64, 0 as i64)
    slice_push[i32](&mut xs_index, 10)
    slice_push[i32](&mut xs_index, 20)
    slice_push[i32](&mut xs_index, 30)

    let acc = 0
    for idx, v in xs_index {
        acc += idx + v
    }
    if acc != 63 {
        return 17
    }

    let xs_refs = make_slice[i32](0 as i64, 0 as i64)
    slice_push[i32](&mut xs_refs, 10)
    slice_push[i32](&mut xs_refs, 20)
    slice_push[i32](&mut xs_refs, 30)
    for rv in &xs_refs {
        let _x = *rv
    }

    let xs_mut = make_slice[i32](0 as i64, 0 as i64)
    slice_push[i32](&mut xs_mut, 10)
    slice_push[i32](&mut xs_mut, 20)
    slice_push[i32](&mut xs_mut, 30)
    for mv in &mut xs_mut {
        *mv += 1
    }
    let _popv = slice_pop[i32](&mut xs_mut)

    let mp = make_map[string, i32](4 as i64)
    map_set[string, i32](&mut mp, "a", 10)
    let _got = map_get[string, i32](&mp, "a")
    if map_len[string, i32](&mp) != 1 as i64 {
        return 18
    }
    if !map_del[string, i32](&mut mp, "a") {
        return 19
    }

    let sh = own_new[i32](1)
    let shr = own_borrow_mut[i32](&mut sh)
    *shr = *shr + 2
    if *shr != 3 {
        return 20
    }

    let ch = make_chan[i32](1)
    send(ch, 99)
    let _first_recv = recv(ch)

    select {
        case send(ch, 123) => { },
        default => { return 21
 },
    }
    select {
        case recv(ch) => |v, ok| {
            if !ok || v != 123 {
                return 22
            }
        },
        case after(20) => { return 23
 },
    }
    close(ch)
    select {
        case recv(ch) => |_v, ok| {
            if ok {
                return 24
            }
        },
        default => { return 25
 },
    }

    let ch2 = make_chan[i32](0)
    go async_send(ch2)
    select {
        case recv(ch2) => |v, ok| {
            if !ok || v != 77 {
                return 26
            }
        },
        case after(200) => { return 27
 },
    }

    let outer_seen: i64 = 0
    let inner_seen: i64 = 0
    let i: i64 = 0
    outer: while i < 5 as i64 {
        i = i + 1 as i64
        outer_seen = outer_seen + 1 as i64

        let j: i64 = 0
        inner: while j < 3 as i64 {
            j = j + 1 as i64
            if j == 2 as i64 {
                continue outer
            }
            inner_seen = inner_seen + 1 as i64
            if j > 10 as i64 {
                break inner
            }
        }
    }

    let k: i64 = 0
    done: loop {
        k = k + 1 as i64
        let t: i64 = 0
        while t < 5 as i64 {
            t = t + 1 as i64
            if t == 3 as i64 {
                break done
            }
        }
    }
    if !(outer_seen == 5 as i64 && inner_seen == 5 as i64 && k == 1 as i64) {
        return 28
    }

    if check_enum_match(Paint.Rgb(1, 2, 3)) != 6 {
        return 29
    }
    if check_guard_or(3 as i64) != 20 as i64 {
        return 30
    }
    let mr = Result.Ok(7)
    let bound = match mr {
        Result.Ok(x) if x > 5 => x,
        Result.Ok(_) => 0,
        Result.Err(_) => -1,
        _ => -2,
    }
    if bound != 7 {
        return 31
    }

    let any = 41 as interface
    let dyn_got = any.add(1)
    if dyn_got != 42 {
        return 32
    }
    let back = any as i32
    if back != 41 {
        return 33
    }

    let ta = 12 as AnyBox
    let tb = ta as i32
    if tb != 12 {
        return 34
    }

    const LOCAL_CONST: i32 = 5
    if LOCAL_CONST != 5 {
        return 35
    }

    let e0 = recover()
    if e0 != nil {
        return 36
    }
    panic("boom")
    let e1 = recover()
    if e1 == nil {
        return 37
    }
    let e2 = recover()
    if e2 != nil {
        return 38
    }

    let terr = try_combo()
    if terr != nil {
        return 39
    }

    let _asmv: i64 = unsafe { asm_probe(1 as i64) }

    fmt.println("all syntax smoke test: ok")
    return 0
}
