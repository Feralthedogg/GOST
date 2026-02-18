module main

import "std/fmt"

fn timer_worker(delay_ms: i32) {
    let t = after(delay_ms)
    let _ = recv(t)
}

fn select_worker(iters: i32, a_ms: i32, b_ms: i32) {
    let xs = make_slice[i32](iters, iters)
    for _ in &xs {
        select {
            case after(a_ms) => { },
            case after(b_ms) => { },
        }
    }
}

fn main() -> i32 {
    let N_TIMERS = 200000
    let N_SELECT = 5000
    let TIMER_MS = 10000
    let CHURN_MS = 12000
    let SELECT_ITERS = 10

    let ts = make_slice[i32](N_TIMERS, N_TIMERS)
    for _ in &ts {
        go timer_worker(TIMER_MS)
    }

    let ss = make_slice[i32](N_SELECT, N_SELECT)
    for _ in &ss {
        go select_worker(SELECT_ITERS, TIMER_MS, CHURN_MS)
    }

    select { case after(CHURN_MS + 2000) => { } }
    println("mixed_worst done")
    return 0
}
