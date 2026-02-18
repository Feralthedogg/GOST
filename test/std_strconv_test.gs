module main

import "std/strconv"
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
    if !eq_str(itoa(-12), "-12") { return 1
 }
    if !eq_str(format_bool(true), "true") { return 2
 }

    let av: i32 = atoi_loose("42", -1)
    if av == -1 { return 3
 }
    if av != 42 { return 4
 }

    let iv: i64 = parse_i64_loose("-9001", 1 as i64)
    if iv == 1 as i64 { return 5
 }
    if iv != -9001 as i64 { return 6
 }

    let bv: bool = parse_bool_loose("true", false)
    if !bv { return 7
 }
    if !bv { return 8
 }

    let fv: f64 = 0.0
    match parse_f64("3.125") {
        Result.Ok(v) => { fv = v },
        Result.Err(_) => { return 9 },
        _ => { return 10 },
    }
    if fv < 3.124 || fv > 3.126 { return 11 }
    if !eq_str(format_f64(3.125, 3), "3.125") { return 12 }

    fmt.println("std_strconv_test ok")
    return 0
}
