module main
import "std/fmt"

fn main() -> i32 {
    let ch = make_chan[unit](0)
    close(ch)
    select {
        case recv(ch) => { println("recv")
 },
        default => { println("default")
 },
    }
    return 0
}
