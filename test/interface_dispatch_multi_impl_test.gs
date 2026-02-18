module main

fn greet(x: i32) -> i32 {
    return x + 100
}

copy struct User {
    id: i32
}

copy struct Admin {
    id: i32
}

impl User {
    fn greet() -> i32 {
        return self.id + 10
    }
}

impl Admin {
    fn greet() -> i32 {
        return self.id + 20
    }
}

fn call_iface(v: interface) -> i32 {
    return v.greet()
}

fn main() -> i32 {
    let u = User { id = 1 }
    let a = Admin { id = 2 }

    if u.greet() != 11 {
        return 1
    }
    if a.greet() != 22 {
        return 2
    }

    let any_u = u as interface
    let any_a = a as interface
    if any_u.greet() != 11 {
        return 3
    }
    if any_a.greet() != 22 {
        return 4
    }

    let any_i32 = 5 as interface
    if any_i32.greet() != 105 {
        return 5
    }

    let mixed = call_iface(any_u) + call_iface(any_a) + call_iface(any_i32)
    if mixed != 138 {
        return 6
    }

    return 0
}