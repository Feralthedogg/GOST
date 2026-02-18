module main

trait Show {
    fn value() -> i32
}

copy struct NumBox {
    v: i32
}

impl Show for NumBox {
    fn value() -> i32 {
        return self.v
    }
}

fn read[T: Show](x: T) -> i32 {
    return x.value()
}

fn main() -> i32 {
    let b = NumBox { v = 9 }
    if read[NumBox](b) != 9 {
        return 1
    }
    return 0
}
