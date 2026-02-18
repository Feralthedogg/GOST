module main

import "std/flag"

private fn flag_test_eq(a: string, b: string) -> bool {
    if string_len(a) != string_len(b) {
        return false
    }
    let i: i64 = 0
    let n: i64 = string_len(a)
    while i < n {
        if string_get(a, i) != string_get(b, i) {
            return false
        }
        i = i + 1
    }
    return true
}

fn main() -> i32 {
    let argv = make_slice[string](0, 0)
    slice_push[string](&mut argv, "--name=gost")
    slice_push[string](&mut argv, "-v")
    slice_push[string](&mut argv, "main.gs")
    let fs = parse_args(argv)
    if !flag_test_eq(string_value(&fs, "name", ""), "gost") { return 1 }
    if !bool_value(&fs, "v", false) { return 2 }
    return 0
}

