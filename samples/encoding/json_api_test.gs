module main

import "std/encoding/json"
import "std/fmt"

fn str_eq(a: string, b: string) -> bool {
    let i: i64 = string_len("");
    let na: i64 = string_len(a);
    let nb: i64 = string_len(b);
    if na != nb { return false; }
    while i < na {
        if string_get(a, i) != string_get(b, i) { return false; }
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
    let src = "{ \"a\" : 1 , \"b\" : [ true , null ] }";

    if !valid(src) {
        println("json_api: valid failed");
        return 1;
    }

    let compacted = "";
    match compact(src) {
        Result.Ok(v) => { compacted = v; },
        Result.Err(_) => {
            println("json_api: compact err");
            return 2;
        },
        _ => { return 3; },
    };

    if !valid(compacted) {
        println("json_api: compact invalid");
        return 4;
    }

    let indented = "";
    match indent(src, "", "  ") {
        Result.Ok(v) => { indented = v; },
        Result.Err(_) => {
            println("json_api: indent err");
            return 5;
        },
        _ => { return 6; },
    };

    if !has_newline(indented) {
        println("json_api: indent no newline");
        return 7;
    }

    let escaped = html_escape("<a&b>");
    if !str_eq(escaped, "\\u003ca\\u0026b\\u003e") {
        println("json_api: html_escape mismatch");
        return 8;
    }

    let dec = decoder_disallow_unknown_fields(decoder_use_number(new_decoder(src)));
    let out = "";
    match decode(dec) {
        Result.Ok(v) => {
            let enc = encoder_set_escape_html(encoder_set_indent(new_encoder(), "", "  "), false);
            match encode(enc, v) {
                Result.Ok(t) => { out = t; },
                Result.Err(_) => {
                    println("json_api: encode err");
                    return 11;
                },
                _ => { return 12; },
            };
        },
        Result.Err(_) => {
            println("json_api: decode err");
            return 9;
        },
        _ => { return 10; },
    };

    if !has_newline(out) {
        println("json_api: encode no newline");
        return 13;
    }

    println("json_api_test ok");
    return 0;
}
