module main

enum Token {
    Num(i32)
}

fn add1(x: i32) -> i32 {
    return x + 1
}

fn inc(x: i32, d: i32) -> i32 {
    return x + d
}

fn main() -> i32 {
    let t = Token.Num[i32](7)
    let v = match t {
        Token.Num(x) => x,
    }
    if v != 7 {
        return 1
    }

    let fp = add1
    let fp_val = fp[i32](4)
    if fp_val != 5 {
        return 2
    }

    let any = 41 as interface
    let got = any.inc[i32](1)
    if got != 42 {
        return 3
    }

    return 0
}
