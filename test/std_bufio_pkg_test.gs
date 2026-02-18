module main

import "std/bufio"

private fn bufio_test_eq(a: string, b: string) -> bool {
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
    let r = new_reader("one\ntwo\n")
    match read_line(&mut r) {
        Result.Ok(v) => {
            if !bufio_test_eq(v, "one") { return 1 }
        },
        Result.Err(_) => { return 2 },
        _ => { return 3 },
    }
    return 0
}

