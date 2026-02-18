module main

copy struct Box {
    v: i32
}

unsafe impl Box {
    fn val() -> i32 {
        return self.v
    }
}

fn main() -> i32 {
    let b = Box { v = 1 }
    return b.val() - 1
}
