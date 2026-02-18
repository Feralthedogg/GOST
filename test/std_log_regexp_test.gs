module main

import "std/log"
import "std/regexp"
import "std/fmt"

fn eq_str(a: string, b: string) -> bool {
    let na: i64 = string_len(a)
    let nb: i64 = string_len(b)
    if na != nb {
        return false
    }
    let i: i64 = 0
    while i < na {
        if string_get(a, i) != string_get(b, i) {
            return false
        }
        i = i + 1
    }
    return true
}

fn main() -> i32 {
    let lg = with_level("test", level_debug())
    debug(lg, "debug-line")
    info(lg, "info-line")
    warn(lg, "warn-line")
    error(lg, "error-line")

    if !match_string("a.*c", "abbbc") { return 1 }
    if !match_string("ab?c", "ac") { return 2 }
    if match_string("*abc", "abc") { return 3 }

    let r = must_compile("ab*c")
    if !is_match(r, "abbbc") { return 4 }
    if is_match(r, "abbbd") { return 5 }

    let found = find_string(r, "zzabbbczz")
    if !eq_str(found, "abbbc") { return 6 }

    let replaced = replace_all_string(r, "xxabbbcyyabc", "_")
    if !eq_str(replaced, "xx_yy_") { return 7 }

    let found2 = find("h.llo", "--hello--")
    if !eq_str(found2, "hello") { return 8 }

    let rep2 = replace_all("colou?r", "color colour", "X")
    if !eq_str(rep2, "X X") { return 9 }

    fmt.println("std_log_regexp_test ok")
    return 0
}
