module main

import "std/fmt"

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

fn ping() {
    println("ping");
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

    go ping();
    println("after go");

    if cmp {
        println("cmp true");
    }
    println("before done");

    println("done");
    return 0;
}
