module main

trait Identity {
    fn id[T](x: T) -> T

    fn twice[T](x: T) -> T {
        return self.id[T](x)
    }
}

copy struct User {
    id: i32
}

impl Identity for User {
    fn id[U](x: U) -> U {
        return x
    }
}

fn main() -> i32 {
    let u = User { id = 1 }

    if u.id[i32](7) != 7 {
        return 1
    }
    if u.twice[i32](9) != 9 {
        return 2
    }

    let any = u as Identity
    if any.id[i32](5) != 5 {
        return 3
    }
    if any.twice[i32](6) != 6 {
        return 4
    }

    return 0
}