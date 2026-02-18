module main

trait Greeter {
    fn greet() -> i32
}

fn render[T: Greeter](v: T) -> i32 {
    return v.greet()
}

fn main() -> i32 {
    return render[i32](1)
}
