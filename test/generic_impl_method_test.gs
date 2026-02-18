module main

copy struct Box {
    v: i32
}

impl Box {
    fn id[T](x: T) -> T {
        return x
    }
}

fn main() -> i32 {
    let b = Box { v = 1 }

    let a: i32 = b.id[i32](7)
    if a != 7 {
        return 1
    }

    let c: u32 = b.id[u32](9 as u32)
    if c != 9 as u32 {
        return 2
    }

    let d: i64 = b.id(5 as i64)
    if d != 5 as i64 {
        return 3
    }

    return 0
}
