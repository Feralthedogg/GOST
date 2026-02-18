module main

fn main() -> i32 {
    let m = make_map[f64, i32](4 as i64)
    map_set[f64, i32](&mut m, 1.25 as f64, 7 as i32)
    map_set[f64, i32](&mut m, 3.5 as f64, 9 as i32)
    if map_len[f64, i32](&m) != 2 as i64 {
        return 1
    }
    if !map_del[f64, i32](&mut m, 1.25 as f64) {
        return 2
    }
    if map_del[f64, i32](&mut m, 1.25 as f64) {
        return 3
    }
    if map_len[f64, i32](&m) != 1 as i64 {
        return 4
    }
    return 0
}
