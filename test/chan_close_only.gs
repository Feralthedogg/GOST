module main
import "std/fmt"

fn main() -> i32 {
    let ch = make_chan[unit](0)
    close(ch)
    println("after close")
    return 0
}
