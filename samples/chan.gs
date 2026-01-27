module main

import "std/fmt"

fn worker() -> unit {
    println("work");
}

fn close_it(ch: chan[i32]) -> error {
    close(ch)?;
    return nil;
}

fn main() -> unit {
    defer println("done");
    go worker();
    let ch = make_chan[i32](1);
    send(ch, 7);
    let pair = recv(ch);
    let _ = pair;
    select {
        case recv(ch) => |v, ok| { println("got"); let _ = v; let _ = ok; },
        default => { println("default"); },
    }
    let err = close_it(ch);
    let _ = err;
}
