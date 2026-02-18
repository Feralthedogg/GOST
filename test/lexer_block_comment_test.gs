module main

fn main() -> i32 {
    let a: i32 = 40 /* comment with newline
        and nested comment: /* inner */
    */
    let b: i32 = 2
    let c: i32 = a /* inline */ + b
    if c != 42 {
        return 1
    }
    return 0
}
