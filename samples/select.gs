module main

import "std/fmt"

fn main() -> unit {
    let ch = make_chan[i32](1);
    select {
        case recv(ch) => { println("got"); },
        default => { println("default"); },
    }
}
