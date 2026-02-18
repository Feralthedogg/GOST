module main

import "std/fmt"

fn make_ok(v: i32) -> Result[i32, error] {
    return Result.Ok[i32, error](v)
}

fn main() -> i32 {
    let ch = make_chan[unit](0)
    close(ch)

    let _r: Result[i32, error] = make_ok(7)

    let ready = __gost_chan_can_recv(ch)
    if ready != 0 && ready != 1 {
        return 1
    }

    println("tmp_quick_verify ok")
    return 0
}
