module main

import "std/encoding/toml"
import "std/fmt"

fn has_newline(s: string) -> bool {
    let i: i64 = string_len("")
    let n: i64 = string_len(s)
    while i < n {
        if string_get(s, i) == string_get("\n", string_len("")) {
            return true
        }
        i = i + string_len("a")
    }
    return false
}

fn ends_with_newline(s: string) -> bool {
    let n: i64 = string_len(s)
    if n == string_len("") { return false
 }
    return string_get(s, n - string_len("a")) == string_get("\n", string_len(""))
}

fn main() -> i32 {
    let src = "title = \"TOML\"\n[owner]\nname = \"Tom\"\nage = 30\n"

    if !valid(src) {
        println("toml_api: valid failed")
        return 1
    }

    let normalized = ""
    match format(src) {
        Result.Ok(v) => { normalized = v
 },
        Result.Err(_) => {
            println("toml_api: format err")
            return 2
        },
        _ => { return 3
 },
    }

    if !valid(normalized) {
        println("toml_api: format invalid")
        return 4
    }

    let dec = new_decoder(src)

    let out = ""
    match decode(dec) {
        Result.Ok(v) => {
            let enc = encoder_set_newline(new_encoder(), true)
            match encode(enc, v) {
                Result.Ok(t) => { out = t
 },
                Result.Err(_) => {
                    println("toml_api: encode err")
                    return 5
                },
                _ => { return 6
 },
            }
        },
        Result.Err(_) => {
            println("toml_api: decode err")
            return 7
        },
        _ => { return 8
 },
    }

    if !has_newline(out) {
        println("toml_api: encode no newline")
        return 9
    }

    let out2 = ""
    let dec2 = new_decoder(src)
    match decode(dec2) {
        Result.Ok(v2) => {
            let enc2 = encoder_set_newline(new_encoder(), false)
            match encode(enc2, v2) {
                Result.Ok(t2) => { out2 = t2
 },
                Result.Err(_) => {
                    println("toml_api: encode(no newline) err")
                    return 10
                },
                _ => { return 11
 },
            }
        },
        Result.Err(_) => {
            println("toml_api: decode2 err")
            return 12
        },
        _ => { return 13
 },
    }

    if ends_with_newline(out2) {
        println("toml_api: encode(no newline) still ends with newline")
        return 14
    }

    println("toml_api_test ok")
    return 0
}
