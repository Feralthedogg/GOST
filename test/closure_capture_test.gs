module main

fn main() -> i32 {
    let base: i64 = 40
    let add = |x: i64| x + base
    let out = add(2 as i64)

    let twice = |v: i64| (|w: i64| w + base)(v)
    let out2 = twice(3 as i64)

    if out == 42 as i64 && out2 == 43 as i64 {
        return 0
    }
    return 1
}
