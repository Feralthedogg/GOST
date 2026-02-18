module main

trait Any {}

fn plus(x: i32, d: i32) -> i32 {
    return x + d
}

fn main() -> i32 {
    let v = 41
    let any = v as Any
    let got = any.plus(1)
    if got != 42 {
        return 1
    }

    let e = recover()
    let _ = e
    return 0
}
