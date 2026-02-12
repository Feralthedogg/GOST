module main

extern "C" fn c_log(tag: i64, ...) -> i64;

unsafe fn run() -> i64 {
    let tag: i64 = 1;
    let a: i64 = 2;
    let b: i64 = 3;
    let c: i64 = 4;
    let d: i64 = 5;
    return c_log(tag, a, b, c, d);
}

fn main() -> i32 {
    let _x: i64 = unsafe { run() };
    return 0;
}
