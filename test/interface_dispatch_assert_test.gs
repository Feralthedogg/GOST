module main

fn inc(x: i32, d: i32) -> i32 {
    return x + d
}

fn main() -> i32 {
    let v = 41
    let any = v as interface
    let got = any.inc(1)
    if got != 42 {
        return 1
    }

    let back = any as i32
    if back != 41 {
        return 2
    }

    return 0
}
