module main

fn id[T](v: T) -> T {
    return v
}

fn src() -> string {
    return "ok"
}

fn use_ret() -> string {
    return id(src())
}

private fn eq_str(a: string, b: string) -> bool {
    let na: i64 = string_len(a)
    if na != string_len(b) {
        return false
    }
    let i: i64 = 0
    while i < na {
        if string_get(a, i) != string_get(b, i) {
            return false
        }
        i = i + 1
    }
    return true
}

fn main() -> i32 {
    let s: string = id(src())
    let t = use_ret()
    if eq_str(s, "ok") {
        if eq_str(t, "ok") {
            return 0
        }
    }
    return 1
}
