module main

fn apply(f: fn(i32)->i32, x: i32) -> i32 {
    return f(x)
}

fn make_fnptr() -> fn(i32)->i32 {
    let add = |x| x + 1
    return add
}

fn make_closure(base: i32) -> closure(i32)->i32 {
    let add: closure(i32)->i32 = |x: i32| x + base
    return add
}

fn main() -> i32 {
    let add = |x: i32| x + 1
    let f = add
    if f(2) != 3 {
        return 1
    }

    if apply(add, 2) != 3 {
        return 2
    }

    let g: closure(i32)->i32 = |x| x + 3
    if g(1) != 4 {
        return 3
    }

    if apply(|x: i32| x + 4, 1) != 5 {
        return 4
    }

    let h = make_fnptr()
    if h(2) != 3 {
        return 5
    }

    let c = make_closure(7)
    if c(8) != 15 {
        return 6
    }

    if apply(c, 2) != 9 {
        return 7
    }

    return 0
}
