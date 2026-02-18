module main

import "std/fmt"

fn eq_str(a: string, b: string) -> bool {
    let na: i64 = string_len(a)
    let nb: i64 = string_len(b)
    if na != nb { return false
 }
    let i: i64 = 0
    while i < na {
        if string_get(a, i) != string_get(b, i) { return false
 }
        i = i + 1
    }
    return true
}

fn main() -> i32 {
    let args = make_slice[string](0, 0)
    slice_push[string](&mut args, "gost")
    slice_push[string](&mut args, arg_i32(42))
    slice_push[string](&mut args, arg_bool(true))

    let s = sprintf("hi %s %d %v %%", args)
    if !eq_str(s, "hi gost 42 true %") { return 1
 }

    let args2 = make_slice[string](0, 0)
    slice_push[string](&mut args2, "x")
    slice_push[string](&mut args2, "7")
    printf("fmt:%s/%d\n", args2)

    let parts = make_slice[string](0, 0)
    slice_push[string](&mut parts, "a")
    slice_push[string](&mut parts, "b")
    slice_push[string](&mut parts, "c")

    if !eq_str(sprint(parts), "a b c") { return 2
 }
    let parts2 = make_slice[string](0, 0)
    slice_push[string](&mut parts2, "a")
    slice_push[string](&mut parts2, "b")
    slice_push[string](&mut parts2, "c")
    printv(parts2)
    println("")
    let parts3 = make_slice[string](0, 0)
    slice_push[string](&mut parts3, "a")
    slice_push[string](&mut parts3, "b")
    slice_push[string](&mut parts3, "c")
    printlnv(parts3)

    if !eq_str(arg_f64(3.14), "3.14") { return 3
 }
    if !eq_str(arg_f32(2.5), "2.5") { return 4
 }

    fmt.println("std_fmt_test ok")
    return 0
}
