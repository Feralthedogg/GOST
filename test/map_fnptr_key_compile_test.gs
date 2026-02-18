module main

fn add1(x: i32) -> i32 {
    return x + 1
}

fn main() -> i32 {
    let m = make_map[fn(i32)->i32, i32](2 as i64)
    map_set[fn(i32)->i32, i32](&mut m, add1, 1 as i32)
    if map_len[fn(i32)->i32, i32](&m) != 1 as i64 {
        return 1
    }
    if !map_del[fn(i32)->i32, i32](&mut m, add1) {
        return 2
    }
    if map_del[fn(i32)->i32, i32](&mut m, add1) {
        return 3
    }
    return 0
}
