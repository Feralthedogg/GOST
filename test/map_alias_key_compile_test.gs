module main

fn main() -> i32 {
    let m = make_map[alias[i32], i32](2 as i64)
    let owner = own_new[i32](7 as i32)
    let key = freeze[i32](owner)
    map_set[alias[i32], i32](&mut m, key, 11 as i32)
    if map_len[alias[i32], i32](&m) != 1 as i64 {
        return 1
    }
    if !map_del[alias[i32], i32](&mut m, key) {
        return 2
    }
    if map_del[alias[i32], i32](&mut m, key) {
        return 3
    }
    return 0
}
