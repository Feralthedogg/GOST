module main

import "std/error" as err

fn main() -> i32 {
    let e0 = nil
    if err.is_nil(e0) == false {
        return 1
    }

    let e = err.new("alpha")
    if e == nil {
        return 3
    }
    if err.is_nil(e) == true {
        return 4
    }

    let m1 = err.message(e)
    if string_len(m1) != 5 as i64 {
        return 5
    }
    if string_get(m1, 0 as i64) != 97 {
        return 6
    }

    let m2 = e.message()
    if string_len(m2) != 5 as i64 {
        return 7
    }

    let any = e as interface
    let m3 = any.message()
    if string_len(m3) != 5 as i64 {
        return 8
    }
    if string_get(m3, 4 as i64) != 97 {
        return 9
    }

    panic("boom")
    let pe = recover()
    if pe == nil {
        return 10
    }
    let pm = err.message(pe)
    if string_len(pm) != 4 as i64 {
        return 11
    }
    if string_get(pm, 0 as i64) != 98 {
        return 12
    }

    if recover() != nil {
        return 13
    }

    return 0
}