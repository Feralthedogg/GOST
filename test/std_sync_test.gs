module main

import "std/sync"
import "std/fmt"

fn worker(m: Mutex, wg: WaitGroup, iters: i32, out: chan[i32]) {
    let i: i32 = 0
    while i < iters {
        lock(m)
        unlock(m)
        i = i + 1
    }
    send(out, iters)
    done(wg)
}

fn noop() {
}

fn main() -> i32 {
    let m = new_mutex()
    if !try_lock(m) { return 1 }
    if try_lock(m) { return 2 }
    unlock(m)
    if !try_lock(m) { return 3 }
    unlock(m)

    let wg = new_wait_group()
    let workers: i32 = 8
    let iters: i32 = 200
    let out = make_chan[i32](workers)

    add(wg, workers)
    let i: i32 = 0
    while i < workers {
        go worker(m, wg, iters, out)
        i = i + 1
    }
    wait(wg)

    let total: i32 = 0
    let j: i32 = 0
    while j < workers {
        select {
            case recv(out) => |v, ok| {
                if ok {
                    total = total + v
                }
            },
        }
        j = j + 1
    }
    if total != workers * iters { return 4 }

    let o = new_once()
    do_once(o, noop)
    do_once(o, noop)

    fmt.println("std_sync_test ok")
    return 0
}
