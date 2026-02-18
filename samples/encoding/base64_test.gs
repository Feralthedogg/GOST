module main

import "std/encoding/base64"
import "std/fmt"

fn str_eq(a: string, b: string) -> bool {
    let na: i64 = string_len(a)
    let nb: i64 = string_len(b)
    if na != nb { return false
 }
    let i: i64 = string_len("")
    while i < na {
        if string_get(a, i) != string_get(b, i) { return false
 }
        i = i + string_len("a")
    }
    return true
}

fn main() -> i32 {
    let plain: string = "hello world"

    let enc: string = encode_to_string(plain)
    if !str_eq(enc, "aGVsbG8gd29ybGQ=") {
        println("encoding/base64 encode mismatch")
        return 1
    }

    match decode_string(enc) {
        Result.Ok(v) => {
            if !str_eq(v, plain) {
                println("encoding/base64 decode mismatch")
                return 2
            }
        },
        Result.Err(_) => {
            println("encoding/base64 decode error")
            return 3
        },
        _ => { return 4
 },
    }

    let raw: string = raw_std_encode_to_string(plain)
    if !str_eq(raw, "aGVsbG8gd29ybGQ") {
        println("encoding/base64 raw encode mismatch")
        return 5
    }

    match raw_std_decode_string(raw) {
        Result.Ok(v2) => {
            if !str_eq(v2, plain) {
                println("encoding/base64 raw decode mismatch")
                return 6
            }
        },
        Result.Err(_) => {
            println("encoding/base64 raw decode error")
            return 7
        },
        _ => { return 8
 },
    }

    let url_in: string = "hello/world?x=1"
    let url_enc: string = url_encode_to_string(url_in)
    match url_decode_string(url_enc) {
        Result.Ok(v3) => {
            if !str_eq(v3, url_in) {
                println("encoding/base64 url decode mismatch")
                return 9
            }
        },
        Result.Err(_) => {
            println("encoding/base64 url decode error")
            return 10
        },
        _ => { return 11
 },
    }

    match decode_string("a===") {
        Result.Ok(_) => {
            println("encoding/base64 expected invalid input error")
            return 12
        },
        Result.Err(_) => {},
        _ => { return 13
 },
    }

    println("encoding/base64 ok")
    return 0
}
