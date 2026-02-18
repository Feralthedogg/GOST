module main

copy struct K {
    a: alias[i32]
    tag: i32
}

fn main() -> i32 {
    let m = make_map[K, i32](2 as i64)
    let owner = own_new[i32](3 as i32)
    let key = K { a = freeze[i32](owner), tag = 9 as i32 }
    map_set[K, i32](&mut m, key, 1 as i32)
    if map_len[K, i32](&m) != 1 as i64 {
        return 1
    }
    if !map_del[K, i32](&mut m, key) {
        return 2
    }
    return 0
}
