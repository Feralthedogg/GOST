module main

fn idv(self: i32) -> i32 {
    return self
}

fn main() -> i32 {
    let x: i32 = 7
    let r = &x
    let y = r.idv()
    if y != 7 { return 1
 }
    return 0
}
