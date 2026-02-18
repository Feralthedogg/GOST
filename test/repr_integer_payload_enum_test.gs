module main

repr(u8) enum E {
    A(i32)
    B
}

fn main() -> i32 {
    let e = E.A(7 as i32)
    let out = match e {
        E.A(v) => v,
        E.B => 0 as i32,
    }
    if out != 7 as i32 {
        return 1
    }
    return 0
}
