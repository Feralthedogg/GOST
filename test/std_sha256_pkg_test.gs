module main

import "std/crypto/sha256"

private fn sha256_test_eq(a: string, b: string) -> bool {
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
    if !sha256_test_eq(sum("abc"), "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad") {
        return 1
    }
    return 0
}

