module main
import "std/fmt"

fn main() -> i32 {
    let ch = make_chan[unit](0);
    let ch2 = ch;
    close(ch2);
    println("ok");
    return 0;
}
