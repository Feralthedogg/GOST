module main

extern "C" let c_counter: i64;

unsafe fn read_counter() -> i64 {
    return c_counter;
}

unsafe fn write_counter(v: i64) {
    c_counter = v;
}

fn main() -> i32 {
    let v: i64 = 7;
    unsafe { write_counter(v) };
    let _x: i64 = unsafe { read_counter() };
    return 0;
}
