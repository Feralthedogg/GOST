module main

trait Text {
    fn text() -> string
}

trait Greeter {
    fn greet() -> i32
}

copy struct User {
    id: i32
}

impl Text for User {
    fn text() -> string {
        return "ok"
    }
}

impl Greeter for User {
    fn greet() -> i32 {
        return self.id
    }
}

fn render[T: Text + Greeter](v: T) -> i32 {
    let msg = v.text()
    if string_len(msg) == 2 {
        return v.greet()
    }
    return 0
}

fn main() -> i32 {
    let u = User { id = 7 }
    if render[User](u) != 7 {
        return 1
    }
    return 0
}
