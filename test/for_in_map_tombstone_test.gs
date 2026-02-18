module main

fn main() -> i32 {
    let m = make_map[i32, i32](2 as i64)

    let i: i32 = 0
    while i < 2000 {
        map_set[i32, i32](&mut m, i, i * 2 as i32)
        i = i + 1
    }

    i = 0
    while i < 2000 {
        if i % 3 as i32 == 0 as i32 {
            let _ = map_del[i32, i32](&mut m, i)
        }
        i = i + 1
    }

    let want_len = 0 as i64
    let want_sum_k = 0 as i64
    let want_sum_v = 0 as i64
    i = 0
    while i < 2000 {
        if i % 3 as i32 != 0 as i32 {
            want_len += 1 as i64
            want_sum_k += i as i64
            want_sum_v += (i * 2 as i32) as i64
        }
        i = i + 1
    }

    let got_len = 0 as i64
    let got_sum_k = 0 as i64
    let got_sum_v = 0 as i64
    for k, v in &m {
        got_len += 1 as i64
        got_sum_k += k as i64
        got_sum_v += v as i64
    }

    if got_len != want_len {
        return 1
    }
    if got_sum_k != want_sum_k {
        return 2
    }
    if got_sum_v != want_sum_v {
        return 3
    }
    return 0
}
