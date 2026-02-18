module main

fn main() -> i32 {
    let e0 = recover()
    if e0 != nil {
        return 1
    }

    panic("boom")

    let e1 = recover()
    if e1 == nil {
        return 2
    }

    let e2 = recover()
    if e2 != nil {
        return 3
    }

    return 0
}
