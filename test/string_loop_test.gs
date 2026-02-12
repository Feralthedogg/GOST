module main

import "std/fmt"

fn main() -> i32 {
    let s: string = "abc";
    let zero: i64 = string_len("");
    let one: i64 = string_len("a");
    let two: i64 = string_len("aa");
    let three: i64 = string_len("aaa");
    let four: i64 = string_len("aaaa");
    let eight: i64 = string_len("aaaaaaaa");
    let n: i64 = string_len(s);
    if n != three {
        println("len_fail");
        return 1;
    }

    // sum bytes using while
    let i: i64 = zero;
    let sum: i32 = 0;
    while i < n {
        let b: i32 = string_get(s, i);
        sum = sum + b;
        i = i + one;
    }

    let expected_sum: i32 = 294; // 'a'(97)+'b'(98)+'c'(99)
    if sum != expected_sum {
        println("sum_fail");
        return 2;
    }

    let sub: string = string_slice(s, one, two); // "bc"
    let join: string = string_concat(sub, "d");
    if string_len(join) != three {
        println("concat_len_fail");
        return 3;
    }
    let jb: i32 = string_get(join, zero);
    let jc: i32 = string_get(join, one);
    let jd: i32 = string_get(join, two);
    let eb: i32 = string_get("b", zero);
    let ec: i32 = string_get("c", zero);
    let ed: i32 = string_get("d", zero);
    if jb != eb || jc != ec || jd != ed {
        println("concat_fail");
        return 3;
    }

    // loop + break/continue
    let k: i64 = zero;
    let acc: i64 = zero;
    loop {
        k = k + one;
        if k == two {
            continue;
        }
        acc = acc + k;
        if k == four {
            break;
        }
    }
    // acc = 1 + 3 + 4 = 8
    if acc != eight {
        println("loop_fail");
        return 4;
    }

    println("string_loop_test ok");
    return 0;
}

// touch
