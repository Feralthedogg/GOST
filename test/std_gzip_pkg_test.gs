module main

import "std/compress/gzip"

private fn gzip_test_eq(a: string, b: string) -> bool {
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
    let packed = compress("aaabbbbcc")
    match packed {
        Result.Ok(v) => {
            match decompress(v) {
                Result.Ok(s) => {
                    if !gzip_test_eq(s, "aaabbbbcc") { return 1 }
                },
                Result.Err(_) => { return 2 },
                _ => { return 3 },
            }
        },
        Result.Err(_) => { return 4 },
        _ => { return 5 },
    }
    return 0
}

