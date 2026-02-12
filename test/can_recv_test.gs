module main
import "std/fmt"

fn main() -> i32 {
    let ch = make_chan[unit](0);
    close(ch);
    if __gost_chan_can_recv(ch) != 0 {
        println("ready");
    } else {
        println("not");
    }
    return 0;
}
