module main

import "std/fmt"

fn timer_worker() {
    let t = after(1);
    let _ = recv(t);
}

fn timer_stress(n: i32, wait_ms: i32) {
    let xs = make_slice[i32](n, n);
    for _ in &xs {
        go timer_worker();
    }
    select { case after(wait_ms) => { } }
    println("timer_stress done");
}

fn main() -> i32 {
    timer_stress(200000, 5000);
    return 0;
}