module main

fn main() -> i32 {
    let r = Result.Ok[i32, error](7)
    let out = match r {
        Result.Ok(x) if x > 5 => x,
        Result.Ok(_) => 0,
        Result.Err(_) => -1,
        _ => -2,
    }

    if out == 7 {
        return 0
    }
    return 1
}
