module main

fn main() -> i32 {
    let s = "abc"
    let sum: i64 = 0
    for i, ch in s {
        sum += i + (ch as i64)
    }
    if sum != 297 as i64 {
        return 1
    }

    let sum2: i64 = 0
    for ch in &s {
        sum2 += ch as i64
    }
    if sum2 != 294 as i64 {
        return 2
    }
    return 0
}
