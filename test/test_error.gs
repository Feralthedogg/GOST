module main

import "std/fmt"
import "std/error"

struct Point {
    x: i32;
    y: i32;
}

copy struct Pair {
    a: i32;
    b: i32;
}

enum Color {
    Red;
    Rgb(i32, i32, i32);
}

copy enum Flag {
    On;
    Off;
}

fn takes_bool(b: bool) -> unit { }
fn takes_i32(x: i32) -> i32 { return x; }

fn field_test(p: Point) -> unit {
    let _u = p.xo;
}

fn len(p: Point) -> i32 { return p.x; }
fn bump_mut(p: mutref[Point]) -> unit { }
fn bump_ref(p: ref[Point]) -> unit { }
fn id_point(p: Point) -> Point { return p; }

fn method_test(p: Point) -> i32 {
    return p.lenn();
}

fn method_autoadj_tests(p: Point) -> unit {
    // E1105: need &mut but receiver is not mutable
    p.bump_mut();

    // E1104: temporary receiver is not addressable
    id_point(p).bump_ref();
}

fn main() -> i32 {
    // let/type mismatch
    let n: i32 = true;

    // if/else condition type error
    if 1 { } else { }

    // match pattern type mismatch
    let _m = match true { 0 => 1, _ => 2, };

    // for/in expects slice
    for v in 123 { }

    // break/continue outside loop
    break;
    continue;

    // return type mismatch (expects i32)
    return true;

    // select/case/default with recv/send misuse
    select {
        case recv(1) => { },
        case send(1, 2) => { },
        default => { },
    }

    // go/defer with non-call/block
    go 1;
    defer 2;

    // send/recv/close/after wrong args/types
    send(1, 2);
    recv(1);
    close(1);
    after(true);

    // true/false/nil type mismatch
    let _b: i32 = false;
    let _c: bool = 123;
    let _d: i32 = nil;

    // interface type misuse (if restricted)
    let _it: interface = 1;

    // unknown identifier (did-you-mean)
    pritn("hi");

    // unknown type (did-you-mean)
    let _s: strnig = "x";

    // unknown enum variant (did-you-mean)
    let _e = Color.Rdd;

    0
}
