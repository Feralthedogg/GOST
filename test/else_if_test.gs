module main

fn main() -> i32 {
    let v = 2
    let out: i32 = if v == 1 { 10 as i32 } else if v == 2 { 20 as i32 } else { 30 as i32 }
    if out != 20 as i32 {
        return 1
    }
    return 0
}
