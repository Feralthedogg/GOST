module main

import "std/encoding/yaml"
import "std/fmt"

fn has_prefix(s: string, p: string) -> bool {
    let ns: i64 = string_len(s);
    let np: i64 = string_len(p);
    let i: i64 = string_len("");
    if np > ns { return false; }
    while i < np {
        if string_get(s, i) != string_get(p, i) { return false; }
        i = i + string_len("a");
    }
    return true;
}

fn has_newline(s: string) -> bool {
    let i: i64 = string_len("");
    let n: i64 = string_len(s);
    while i < n {
        if string_get(s, i) == string_get("\n", string_len("")) {
            return true;
        }
        i = i + string_len("a");
    }
    return false;
}

fn main() -> i32 {
    let src = "a: 1\nb:\n  - true\n  - null\n";

    if !valid(src) {
        println("yaml_api: valid failed");
        return 1;
    }

    let normalized = "";
    match format(src) {
        Result.Ok(v) => { normalized = v; },
        Result.Err(_) => {
            println("yaml_api: format err");
            return 2;
        },
        _ => { return 3; },
    };

    if !valid(normalized) {
        println("yaml_api: format invalid");
        return 4;
    }

    let dec = new_decoder(src);

    let out = "";
    match decode(dec) {
        Result.Ok(v) => {
            let enc = encoder_set_indent(new_encoder(), ">> ", "");
            match encode(enc, v) {
                Result.Ok(t) => { out = t; },
                Result.Err(_) => {
                    println("yaml_api: encode err");
                    return 5;
                },
                _ => { return 6; },
            };
        },
        Result.Err(_) => {
            println("yaml_api: decode err");
            return 7;
        },
        _ => { return 8; },
    };

    if !has_newline(out) {
        println("yaml_api: encode no newline");
        return 9;
    }
    if !has_prefix(out, ">> ") {
        println("yaml_api: encode prefix missing");
        return 10;
    }

    println("yaml_api_test ok");
    return 0;
}
