module main

import "std/fmt"
import "std/error"

struct Point {
    x: i32;
    y: i32;
}

copy struct Pair {
    a: i32;
    b: i32;
}

enum Color {
    Red;
    Rgb(i32, i32, i32);
}

copy enum Flag {
    On;
    Off;
}

fn point_sum(p: Point) -> i32 {
    let rx = &p.x;
    let ry = &p.y;
    let x = *rx;
    let y = *ry;
    return x + y;
}

fn match_demo(v: i32) -> string {
    return match v {
        0 => "zero",
        1 => "one",
        _ => "other",
    };
}

fn enum_demo(c: Color) -> string {
    return match c {
        Color.Red => "red",
        Color.Rgb(r, g, b) => "rgb",
        _ => "other",
    };
}

fn try_ok() -> (i32, error) {
    return (7, nil);
}

fn try_demo() -> error {
    let v = try_ok()?;
    let t = v + 1;
    let unused = t;
    return nil;
}

fn ok_res() -> Result[i32, error] {
    return Result.Ok[i32, error](7);
}

fn fail_res() -> Result[i32, error] {
    return Result.Err[i32, error](error_new("x"));
}

fn ok2() -> (i32, error) {
    return (3, nil);
}

fn t1() -> error {
    let v = ok_res()?;
    let v2 = v;
    return nil;
}

fn t2() -> error {
    defer println("D");
    let unused = fail_res()?;
    println("X");
    return nil;
}

fn t3() -> error {
    let v = ok2()?;
    let v2 = v;
    return nil;
}

fn t4(r: Result[i32, error]) -> i32 {
    return match r {
        Result.Ok(x) => x,
        Result.Err(_) => 0,
        _ => 0,
    };
}

fn mk_res() -> Result[[]i32, error] {
    let xs = make_slice[i32](1, 1);
    return Result.Ok[[]i32, error](xs);
}

fn t5() -> error {
    let r = mk_res();
    let r2 = r;
    return nil;
}

fn ping() {
    println("ping");
}

fn work_shared(x: shared[i32]) {
    let r = shared_get[i32](&x);
    let v = *r;
    let v2 = v;
}

fn defer_capture_demo() {
    let x = "one";
    defer println(x);
    x = "two";
    println("defer capture set");
}

fn defer_loop_demo() {
    let xs = make_slice[i32](3, 3);
    slice_set[i32](&mut xs, 0, 1);
    slice_set[i32](&mut xs, 1, 2);
    slice_set[i32](&mut xs, 2, 3);
    for v in &xs {
        defer println("inner");
        if *v == 1 {
            continue;
        }
        if *v == 2 {
            break;
        }
    }
    println("after loop");
}

fn defer_return_demo() {
    defer println("outer");
    {
        defer println("inner");
        return;
    }
}

fn try_ok_demo() -> error {
    defer println("try defer");
    let v = try_ok()?;
    if v == 7 {
        return nil;
    }
    return nil;
}

fn inc(x: i32) -> i32 { x + 1 }
fn double(x: i32) -> i32 { x * 2 }
fn iter_pred(r: ref[i32]) -> bool { *r != 0 }
fn iter_map(r: ref[i32]) -> i32 { println("map"); let v = *r; v }

fn after_waiter() {
    let t = after(1);
    let _ = recv(t);
}

fn stress_after(n: i32) {
    let xs = make_slice[i32](n, n);
    for _ in &xs {
        go after_waiter();
    }
    select { case after(1000) => { } }
}

fn timer_worker() {
    let t = after(1);
    let _ = recv(t);
}

fn timer_stress(n: i32) {
    let xs = make_slice[i32](n, n);
    for _ in &xs {
        go timer_worker();
    }
    let wait = if n > 50000 { 2000 } else { 500 };
    select { case after(wait) => { } }
    println("timer_stress done");
}

fn select_worker() {
    select {
        case after(1) => { },
        case after(2) => { },
        case after(3) => { },
    }
}

fn mixed_after_worker() {
    select {
        case after(1) => { },
        case after(1000) => { },
    }
}

fn select_stress(n: i32) {
    let xs = make_slice[i32](n, n);
    for _ in &xs {
        go select_worker();
    }
    let wait = if n > 50000 { 2000 } else { 500 };
    select { case after(wait) => { } }
    println("select_stress done");
}

fn close_drop_stress(n: i32) {
    let xs = make_slice[i32](n, n);
    for _ in &xs {
        let ch = make_chan[i32](0);
        close(ch);
    }
    println("close_drop_stress done");
}

fn mixed_after_stress(n: i32) {
    let xs = make_slice[i32](n, n);
    for _ in &xs {
        go mixed_after_worker();
    }
    select { case after(1500) => { } }
    println("mixed_after_stress done");
}

fn main() -> i32 {
    println("start");
    defer println("defer");

    let n = 1;
    let m = n + 2;
    let cmp = (m > n) && (n == 1);
    let cmp2 = (n != 0) || (m >= n);
    let c = 'a';
    let c2 = c;
    let f = 1.5;
    let f2 = -f;
    let notv = !false;
    println("after basics");

    let block_val = { let a = 1; a + 2 };
    println("after block");

    let tup = (1, 2);
    let tup2 = tup;
    println("after tuple");

    let xs = make_slice[i32](3, 3);
    slice_set[i32](&mut xs, 0, 1);
    slice_set[i32](&mut xs, 1, 2);
    slice_set[i32](&mut xs, 2, 3);

    let first = xs[0];
    let first2 = first;

    let len = slice_len[i32](&xs);
    let len2 = len;

    {
        let r = slice_ref[i32](&xs, 1);
        let rv = *r;
    }

    {
        let mr = slice_mutref[i32](&mut xs, 2);
        let mrv = *mr;
    }

    xs[1] = 5;

    slice_push[i32](&mut xs, 4);
    let pop = slice_pop[i32](&mut xs);
    let pop2 = pop;
    println("after slice");

    let ys = make_slice[i32](2, 2);
    slice_set[i32](&mut ys, 0, 9);
    slice_set[i32](&mut ys, 1, 8);

    for v in ys {
        if v == 9 {
            continue;
        }
        if v == 8 {
            break;
        }
    }

    let zs = make_slice[i32](1, 1);
    slice_set[i32](&mut zs, 0, 4);
    for v in &mut zs {
        let vv = *v;
    }
    println("after loops");

    let its = make_slice[i32](3, 3);
    slice_set[i32](&mut its, 0, 1);
    slice_set[i32](&mut its, 1, 0);
    slice_set[i32](&mut its, 2, 2);
    for x in iter(&its) |> filter(iter_pred) |> map(iter_map) {
        let x2 = x;
    }
    println("after iter");

    let map = make_map[string, i32](4);
    map_set[string, i32](&mut map, "a", 10);
    let got = map_get[string, i32](&map, "a");
    let got2 = got;
    let mlen = map_len[string, i32](&map);
    let mlen2 = mlen;
    let md = map_del[string, i32](&mut map, "a");
    let md2 = md;
    println("after map");

    let sh = shared_new[i32](5);
    let rsh = shared_get[i32](&sh);
    let shv = *rsh;
    println("after shared");

    let ch = make_chan[i32](1);
    send(ch, 99);
    let recv_val = recv(ch);
    let recv_val2 = recv_val;
    println("after chan");

    select {
        case recv(ch) => |v, ok| {
            let v2 = v;
            let ok2 = ok;
            println("select recv");
        },
        case after(1) => {
            println("select after");
        },
        default => {
            println("select default");
        },
    }
    println("after select");

    close(ch);

    let color = Color.Rgb(1, 2, 3);
    let msg = enum_demo(color);
    println(msg);

    let msg2 = match_demo(1);
    println(msg2);
    println("after match");

    let err = try_demo();
    let err2 = err;
    println("after try");

    let r1 = t1();
    let r1b = r1;
    let r2 = t2();
    let r2b = r2;
    let r3 = t3();
    let r3b = r3;
    let mv = t4(ok_res());
    let mv2 = mv;
    let r5 = t5();
    let r5b = r5;
    println("after result");

    go ping();
    println("after go");

    let sh_stress = shared_new[i32](1);
    let stress = make_slice[i32](50, 50);
    for v in &stress {
        go work_shared(sh_stress);
    }
    select {
        case after(5) => { },
    }
    println("after go stress");

    stress_after(10000);
    println("after timer stress");

    let run_100k = false;

    timer_stress(10000);
    select_stress(10000);
    close_drop_stress(10000);
    mixed_after_stress(10000);

    if run_100k {
        timer_stress(100000);
        select_stress(100000);
        mixed_after_stress(100000);
    }

    if cmp {
        println("cmp true");
    }
    let piped = 3 |> inc |> double;
    let piped2 = piped;
    defer_capture_demo();
    defer_loop_demo();
    let ok = try_ok_demo();
    let ok2 = ok;
    defer_return_demo();
    println("before done");

    println("done");
    return 0;
}
