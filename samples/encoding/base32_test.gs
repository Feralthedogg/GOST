module main

import "std/encoding/base32"
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
    let plain: string = "foobar"

    let enc: string = encode_to_string(plain)
    if !str_eq(enc, "MZXW6YTBOI======") {
        println("encoding/base32 encode mismatch")
        return 1
    }

    match decode_string(enc) {
        Result.Ok(v) => {
            if !str_eq(v, plain) {
                println("encoding/base32 decode mismatch")
                return 2
            }
        },
        Result.Err(_) => {
            println("encoding/base32 decode error")
            return 3
        },
        _ => { return 4
 },
    }

    let raw: string = raw_std_encode_to_string(plain)
    if !str_eq(raw, "MZXW6YTBOI") {
        println("encoding/base32 raw encode mismatch")
        return 5
    }

    match raw_std_decode_string(raw) {
        Result.Ok(v2) => {
            if !str_eq(v2, plain) {
                println("encoding/base32 raw decode mismatch")
                return 6
            }
        },
        Result.Err(_) => {
            println("encoding/base32 raw decode error")
            return 7
        },
        _ => { return 8
 },
    }

    let hex_enc: string = hex_encode_to_string(plain)
    if !str_eq(hex_enc, "CPNMUOJ1E8======") {
        println("encoding/base32 hex encode mismatch")
        return 9
    }

    match hex_decode_string(hex_enc) {
        Result.Ok(v3) => {
            if !str_eq(v3, plain) {
                println("encoding/base32 hex decode mismatch")
                return 10
            }
        },
        Result.Err(_) => {
            println("encoding/base32 hex decode error")
            return 11
        },
        _ => { return 12
 },
    }

    let raw_hex: string = raw_hex_encode_to_string(plain)
    if !str_eq(raw_hex, "CPNMUOJ1E8") {
        println("encoding/base32 raw hex encode mismatch")
        return 13
    }

    match raw_hex_decode_string(raw_hex) {
        Result.Ok(v4) => {
            if !str_eq(v4, plain) {
                println("encoding/base32 raw hex decode mismatch")
                return 14
            }
        },
        Result.Err(_) => {
            println("encoding/base32 raw hex decode error")
            return 15
        },
        _ => { return 16
 },
    }

    match decode_string("MZXW6YTB0I======") {
        Result.Ok(_) => {
            println("encoding/base32 expected invalid alphabet error")
            return 17
        },
        Result.Err(_) => {},
        _ => { return 18
 },
    }

    match raw_std_decode_string("ABC") {
        Result.Ok(_) => {
            println("encoding/base32 expected invalid raw length error")
            return 19
        },
        Result.Err(_) => {},
        _ => { return 20
 },
    }

    println("encoding/base32 ok")
    return 0
}
