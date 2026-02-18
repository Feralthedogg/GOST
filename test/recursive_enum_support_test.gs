module main

enum List {
    Nil
    Cons(i64, List)
    Link(alias[i32], List)
}

fn len(xs: List) -> i64 {
    return match xs {
        List.Nil => 0,
        List.Cons(_head, tail) => 1 + len(tail),
        List.Link(_v, tail) => 1 + len(tail),
    }
}

fn main() -> i32 {
    let p = freeze[i32](own_new[i32](2))
    let xs = List.Cons(1, List.Link(p, List.Nil))
    if len(xs) != 2 as i64 {
        return 1
    }
    return 0
}
