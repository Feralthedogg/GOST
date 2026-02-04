module main

import "std/fmt"

fn timer_worker(delay_ms: i32) {
    let t = after(delay_ms);
    let _ = recv(t);
}

fn timer_stress(n: i32, delay_ms: i32, wait_ms: i32) {
    let xs = make_slice[i32](n, n);
    for _ in &xs {
        go timer_worker(delay_ms);
    }
    select { case after(wait_ms) => { } }
    println("timer_stress done");
}

fn main() -> i32 {
    timer_stress(200000, 5000, 6000);
    return 0;
}