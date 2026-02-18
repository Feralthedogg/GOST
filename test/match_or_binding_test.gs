module main

enum Shape {
    Circle(i32)
    Square(i32)
    Unit
}

fn main() -> i32 {
    let a = Shape.Square(7)
    let out_a = match a {
        Shape.Circle(x) | Shape.Square(x) => x,
        Shape.Unit => 0,
    }
    if out_a != 7 {
        return 1
    }

    let b = Shape.Circle(3)
    let out_b = match b {
        Shape.Circle(x) | Shape.Square(x) => x,
        _ => 0,
    }
    if out_b != 3 {
        return 2
    }
    return 0
}
