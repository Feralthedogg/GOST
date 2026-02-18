module main

enum Color {
    Red
    Blue
}

fn main() -> i32 {
    let mm = make_map[Color, i32](4 as i64)
    map_set[Color, i32](&mut mm, Color.Red, 7)
    let _v = map_get[Color, i32](&mm, Color.Red)
    if map_len[Color, i32](&mm) != 1 as i64 {
        return 1
    }
    return 0
}
