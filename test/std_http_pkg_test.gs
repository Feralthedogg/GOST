module main

import "std/net/http"

private fn http_test_eq(a: string, b: string) -> bool {
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
    let req = new_request("GET", "http://127.0.0.1", "")
    if !http_test_eq(req.method, "GET") { return 1 }
    return 0
}

