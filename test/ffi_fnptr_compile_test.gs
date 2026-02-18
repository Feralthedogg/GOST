module main

extern "C" fn c_call_cb(x: i64, cb: fn(i64) -> i64) -> i64

fn add1(x: i64) -> i64 {
    return x + 1
}

unsafe fn run() -> i64 {
    let x: i64 = 41
    return c_call_cb(x, add1)
}

fn main() -> i32 {
    let _x: i64 = unsafe { run() }
    return 0
}
