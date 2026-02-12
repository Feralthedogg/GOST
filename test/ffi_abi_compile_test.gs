module main

extern "system" fn c_system(x: i64) -> i64;
extern "win64" fn c_win64(x: i64) -> i64;
extern "sysv64" fn c_sysv64(x: i64) -> i64;

unsafe fn run(v: i64) -> i64 {
    let a: i64 = c_system(v);
    let b: i64 = c_win64(a);
    return c_sysv64(b);
}

fn main() -> i32 {
    let v: i64 = 1;
    let _x: i64 = unsafe { run(v) };
    return 0;
}
