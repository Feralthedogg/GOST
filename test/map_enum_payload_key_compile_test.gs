module main

copy enum Token {
    Int(i32)
    Word(string)
    End
}

fn main() -> i32 {
    let m = make_map[Token, i32](8 as i64)

    let a = Token.Int(7 as i32)
    let b = Token.Int(7 as i32)
    map_set[Token, i32](&mut m, a, 1 as i32)
    if !map_del[Token, i32](&mut m, b) {
        return 1
    }

    let s1 = string_concat("a", "bc")
    let s2 = string_concat("ab", "c")
    let w1 = Token.Word(s1)
    let w2 = Token.Word(s2)
    map_set[Token, i32](&mut m, w1, 2 as i32)
    if !map_del[Token, i32](&mut m, w2) {
        return 2
    }

    map_set[Token, i32](&mut m, Token.End, 3 as i32)
    if !map_del[Token, i32](&mut m, Token.End) {
        return 3
    }
    if map_len[Token, i32](&m) != 0 as i64 {
        return 4
    }
    return 0
}
