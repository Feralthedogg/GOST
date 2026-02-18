module main

import "std/fmt"
import "std/context"

fn main() -> i32 {
    let bg = background()
    let done1 = cancel_and_done(with_cancel(bg))
    select {
        case recv(done1) => {
            println("canceled")
        },
    }

    let bg2 = background()
    let done2 = done_chan(with_timeout(bg2, 5))
    select {
        case recv(done2) => {
            println("timeout")
        },
    }

    println("context ok")
    return 0
}
