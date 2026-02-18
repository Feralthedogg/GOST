module main

fn main() -> i32 {
    let m = make_map[i32, i32](8 as i64)
    map_set[i32, i32](&mut m, 1 as i32, 10 as i32)
    map_set[i32, i32](&mut m, 2 as i32, 20 as i32)
    map_set[i32, i32](&mut m, 3 as i32, 30 as i32)

    let sum_k = 0 as i64
    let sum_v = 0 as i64
    for k, v in &m {
        sum_k += k as i64
        sum_v += v as i64
    }
    if sum_k != 6 as i64 {
        return 1
    }
    if sum_v != 60 as i64 {
        return 2
    }

    let only_v = 0 as i64
    for v in &m {
        only_v += v as i64
    }
    if only_v != 60 as i64 {
        return 3
    }
    return 0
}
