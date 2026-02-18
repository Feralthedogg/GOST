module main

struct User {
    name: string
}

impl User {
    fn greet() -> string {
        return "hello ${self.name}"
    }
}

fn main() -> i32 {
    let u = User { name = "gost" }
    let g = u.greet()
    let want = "hello gost"
    if string_len(g) != string_len(want) {
        return 1
    }
    if string_get(g, 0 as i64) != string_get(want, 0 as i64) {
        return 1
    }
    if string_get(g, 6 as i64) != string_get(want, 6 as i64) {
        return 1
    }

    let sum = 0
    for i in 1..=3 {
        sum += i
    }
    if sum != 6 {
        return 2
    }

    let xs = make_slice[i32](0 as i64, 0 as i64)
    slice_push[i32](&mut xs, 10)
    slice_push[i32](&mut xs, 20)
    slice_push[i32](&mut xs, 30)

    let acc = 0
    for idx, v in xs {
        acc += idx + v
    }
    if acc != 63 {
        return 3
    }

    return 0
}
