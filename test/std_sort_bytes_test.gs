module main

import "std/sort"
import "std/bytes"
import "std/fmt"

fn eq_str(a: string, b: string) -> bool {
    let na: i64 = string_len(a)
    let nb: i64 = string_len(b)
    if na != nb { return false
 }
    let i: i64 = 0 as i64
    while i < na {
        if string_get(a, i) != string_get(b, i) { return false
 }
        i = i + 1 as i64
    }
    return true
}

fn main() -> i32 {
    let xs = make_slice[i32](0 as i64, 0 as i64)
    slice_push[i32](&mut xs, 3)
    slice_push[i32](&mut xs, 1)
    slice_push[i32](&mut xs, 2)
    sort_i32(&mut xs)
    if !is_sorted_i32(&xs) { return 1
 }

    let ys = make_slice[i32](0 as i64, 0 as i64)
    slice_push[i32](&mut ys, 1)
    slice_push[i32](&mut ys, 2)
    slice_push[i32](&mut ys, 3)
    if search_i32(&ys, 2) != 1 as i64 { return 2
 }

    let gs = make_slice[i64](0, 0)
    slice_push[i64](&mut gs, 9 as i64)
    slice_push[i64](&mut gs, 1 as i64)
    slice_push[i64](&mut gs, 5 as i64)
    sort_i64(&mut gs)
    if !is_sorted_i64(&gs) { return 21 }
    if search_i64(&gs, 5 as i64) != 1 { return 22 }

    let b0 = from_string("hi")
    if !eq_str(to_string(b0), "hi") { return 3
 }
    let b1 = from_string("hi")
    let b2 = from_string("hi")
    if !equal(&b1, &b2) { return 4
 }
    let pfx = from_string("h")
    let sfx = from_string("i")
    if !has_prefix(&b1, &pfx) { return 5 }
    if !has_suffix(&b1, &sfx) { return 6 }
    let src = from_string("a--b--c")
    let old = from_string("--")
    let newb = from_string("/")
    let rep = replace(&src, &old, &newb)
    if !eq_str(to_string(rep), "a/b/c") { return 7 }
    let txt = from_string("abC")
    let upper = to_upper_ascii(&txt)
    if !eq_str(to_string(upper), "ABC") { return 8 }

    fmt.println("std_sort_bytes_test ok")
    return 0
}
