module main

import "std/error" as error { is_nil, new }

fn may_fail(v: i32) -> error {
    if v == 3 {
        return error.new("boom")
    }
    return nil
}

fn check(v: i32) -> error {
    let _ = match v {
        0 | 1 => {},
        2 | 3 => may_fail(v)?,
        _ => {},
    }
    return nil
}

fn main() -> i32 {
    let ok = check(2)
    if !error.is_nil(ok) {
        return 1
    }
    let bad = check(3)
    if error.is_nil(bad) {
        return 2
    }
    return 0
}
