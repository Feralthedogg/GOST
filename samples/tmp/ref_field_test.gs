module main

struct S {
    x: i32;
}

fn setx(p: mutref[S]) {
    p.x = 1;
}

fn getx(p: ref[S]) -> i32 {
    return p.x;
}

fn main() -> i32 {
    let s = S { x = 0 };
    setx(&mut s);
    return getx(&s);
}
