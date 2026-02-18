module main

import "std/encoding/hex"
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
    let plain: string = "hello"
    let enc: string = encode_to_string(plain)
    if !str_eq(enc, "68656c6c6f") {
        println("encoding/hex encode mismatch")
        return 1
    }

    match decode_string(enc) {
        Result.Ok(v) => {
            if !str_eq(v, plain) {
                println("encoding/hex decode mismatch")
                return 2
            }
        },
        Result.Err(_) => {
            println("encoding/hex decode error")
            return 3
        },
        _ => { return 4
 },
    }

    if encoded_len(string_len(plain)) != string_len(enc) {
        println("encoding/hex encoded_len mismatch")
        return 5
    }

    if decoded_len(string_len(enc)) != string_len(plain) {
        println("encoding/hex decoded_len mismatch")
        return 6
    }

    match decode_string("6") {
        Result.Ok(_) => {
            println("encoding/hex expected invalid length error")
            return 7
        },
        Result.Err(_) => {},
        _ => { return 8
 },
    }

    match decode_string("zz") {
        Result.Ok(_) => {
            println("encoding/hex expected invalid character error")
            return 9
        },
        Result.Err(_) => {},
        _ => { return 10
 },
    }

    println("encoding/hex ok")
    return 0
}
