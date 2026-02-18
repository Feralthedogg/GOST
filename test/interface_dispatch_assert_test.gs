module main

trait Incr {
    fn inc(d: i32) -> i32
}

impl Incr for i32 {
    fn inc(d: i32) -> i32 {
        return self + d
    }
}

fn main() -> i32 {
    let v = 41
    let any = v as dyn Incr
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
