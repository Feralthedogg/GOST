module main

trait Greeter {
    fn greet() -> i32
}

copy struct User {
    id: i32
}

impl Greeter for User {
    fn greet(delta: i32) -> i32 {
        return self.id + delta
    }
}

fn main() -> i32 {
    return 0
}
