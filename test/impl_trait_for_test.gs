module main

trait Greeter {
    fn greet() -> i32
}

copy struct User {
    id: i32
}

impl Greeter for User {
    fn greet() -> i32 {
        return self.id + 1
    }
}

fn main() -> i32 {
    let u = User { id = 41 }
    if u.greet() != 42 {
        return 1
    }

    let any = u as Greeter
    if any.greet() != 42 {
        return 2
    }
    return 0
}
