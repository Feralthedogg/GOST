module main

fn main() -> i32 {
    let n: i64 = 3
    let out = match n {
        1 | 2 if n == 0 => 99 as i64,
        1 | 2 => 10 as i64,
        3 if n > 2 as i64 => 20 as i64,
        3 | 4 => 30 as i64,
        _ => 0 as i64,
    }

    if out == 20 as i64 {
        return 0
    }
    return 1
}
