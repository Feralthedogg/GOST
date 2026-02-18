module main

import "std/filepath"

private fn filepath_test_eq(a: string, b: string) -> bool {
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
    let parts = make_slice[string](0, 0)
    slice_push[string](&mut parts, "a")
    slice_push[string](&mut parts, "b")
    slice_push[string](&mut parts, "c.txt")
    let p = join(parts)
    if !filepath_test_eq(ext(p), ".txt") { return 1 }
    if filepath_test_eq(base(p), "") { return 2 }
    return 0
}

