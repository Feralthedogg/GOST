module main

copy struct Vault {
    priv secret: i32
    pub opened: i32
}

fn main() -> i32 {
    let v = Vault { secret = 1, opened = 2 }
    return v.secret
}
