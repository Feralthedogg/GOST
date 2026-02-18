module main

import "std/strings"
import "std/fmt"

fn eq_str(a: string, b: string) -> bool {
    let na: i64 = string_len(a)
    let nb: i64 = string_len(b)
    if na != nb { return false
 }
    let i: i64 = 0 as i64
    while i < na {
        if string_get(a, i) != string_get(b, i) { return false
 }
        i = i + 1 as i64
    }
    return true
}

fn main() -> i32 {
    if !contains("hello world", "world") { return 1
 }
    if !has_prefix("hello world", "he") { return 2
 }
    if !has_suffix("hello world", "ld") { return 3
 }
    if index("abcabc", "cab") != 2 as i64 { return 4
 }
    if count("aaaa", "aa") != 2 { return 5
 }
    if !eq_str(replace("a-b-c", "-", ":"), "a:b:c") { return 6
 }
    if !eq_str(to_upper("abZ"), "ABZ") { return 7
 }
    if !eq_str(to_lower("ABz"), "abz") { return 8
 }
    if !eq_str(trim_space(" \t x \n"), "x") { return 9
 }
    if !eq_str(repeat("go", 3), "gogogo") { return 10
 }
    fmt.println("std_strings_test ok")
    return 0
}
