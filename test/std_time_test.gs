module main

import "std/time"
import "std/fmt"

private fn eq_str(a: string, b: string) -> bool {
    let na: i64 = string_len(a)
    if na != string_len(b) {
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
    let t0: i64 = now_ms()
    sleep_ms(20)
    let elapsed: i64 = now_ms() - t0
    if elapsed < 1 { return 1
 }

    let s: Instant = now()
    sleep_ms(5)
    let dt: i64 = since_ms(s)
    if dt < 1 { return 2
 }

    let d: Duration = seconds(1)
    if duration_ms(d) != 1000 { return 3
 }

    let s2: Instant = now()
    sleep(milliseconds(2))
    let dd: Duration = since(s2)
    if duration_ms(dd) < 1 { return 4
 }

    let dm = add(minutes(1), seconds(2))
    if duration_ms(dm) != 62000 { return 5
 }
    if !eq_str(format_duration(dm), "1m2s") { return 6
 }
    match parse_duration("2h3m4s5ms") {
        Result.Ok(v) => {
            if duration_ms(v) != 7384005 { return 7
 }
        },
        Result.Err(_) => { return 8
 },
        _ => { return 9
 },
    }

    fmt.println("std_time_test ok")
    return 0
}
