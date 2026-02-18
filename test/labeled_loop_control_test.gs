module main

fn main() -> i32 {
    let outer_seen: i64 = 0
    let inner_seen: i64 = 0
    let i: i64 = 0

    outer: while i < 5 {
        i = i + 1
        outer_seen = outer_seen + 1

        let j: i64 = 0
        inner: while j < 3 {
            j = j + 1
            if j == 2 {
                continue outer
            }
            inner_seen = inner_seen + 1
            if j > 10 {
                break inner
            }
        }
    }

    let k: i64 = 0
    done: loop {
        k = k + 1
        let t: i64 = 0
        while t < 5 {
            t = t + 1
            if t == 3 {
                break done
            }
        }
    }

    if outer_seen == 5 && inner_seen == 5 && k == 1 {
        return 0
    }
    return 1
}
