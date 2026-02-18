module main

import "std/unicode"

fn main() -> i32 {
    if !is_letter(string_get("A", 0)) { return 1 }
    if !is_digit(string_get("9", 0)) { return 2 }
    if to_upper(string_get("b", 0)) != string_get("B", 0) { return 3 }
    return 0
}

