# Concurrency and Ownership in Practice

This guide focuses on real patterns with `go`, channels, and `own[T]`/`alias[T]`.
Use it with [owner_alias_model.md](owner_alias_model.md) and [ownership-cheatsheet.md](ownership-cheatsheet.md).

## Pattern 1: Transfer Ownership Across a Channel

Use `own[T]` when one goroutine should hand off exclusive ownership to another.

```gs
module main

import "std/fmt"

copy struct Job {
    id: i32
    payload: string
}

fn producer(out: chan[own[Job]]) {
    let j = own_new[Job](Job { id = 1, payload = "index docs" })
    send(out, j) // move ownership into channel send
    close(out)
}

fn consumer(in: chan[own[Job]]) {
    select {
        case recv(in) => |job, ok| {
            if ok {
                let v = own_borrow[Job](&job)
                fmt.println(v.payload)
            }
        },
    }
}
```

## Pattern 2: Broadcast Read-Only State

Use `freeze` to convert one owner into a shareable read-only handle.

```gs
copy struct Config {
    endpoint: string
    timeout_ms: i32
}

fn worker(cfg: alias[Config]) {
    let c = alias_borrow[Config](&cfg)
    // read-only access
    let _ = c.endpoint
}

fn start_workers() {
    let cfg_owner = own_new[Config](Config {
        endpoint = "https://api.example.com",
        timeout_ms = 5000,
    })
    let cfg_shared = freeze[Config](cfg_owner)
    go worker(cfg_shared)
    go worker(cfg_shared)
}
```

## Pattern 3: Mutable Shared Logic via Command Channel

For concurrent mutation, prefer a single owner goroutine plus message passing.

```gs
enum CounterCmd {
    Inc(i32)
    Snapshot(chan[i32])
    Stop
}

fn counter_loop(cmds: chan[CounterCmd]) {
    let state = own_new[i32](0)
    let done = false
    while !done {
        select {
            case recv(cmds) => |cmd, ok| {
                if !ok {
                    done = true
                } else {
                    match cmd {
                        CounterCmd.Inc(n) => {
                            let p = own_borrow_mut[i32](&mut state)
                            *p = *p + n
                        },
                        CounterCmd.Snapshot(reply) => {
                            let p = own_borrow[i32](&state)
                            send(reply, *p)
                        },
                        CounterCmd.Stop => {
                            done = true
                        },
                        _ => {},
                    }
                }
            },
        }
    }
}
```

## Pattern 4: Mutex for Critical Sections

`std/sync` mutexes are useful for local critical sections.
Still keep ownership explicit.

```gs
import "std/sync"

struct Queue {
    mu: sync.Mutex
    items: []i32
}

fn push(q: mutref[Queue], v: i32) {
    sync.lock(q.mu)
    slice_push[i32](&mut q.items, v)
    sync.unlock(q.mu)
}

fn build_queue() -> alias[Queue] {
    let q = own_new[Queue](Queue {
        mu = sync.new_mutex(),
        items = make_slice[i32](0 as i64, 0 as i64),
    })
    {
        let w = own_borrow_mut[Queue](&mut q)
        push(w, 10)
    }
    return freeze[Queue](q)
}
```

## Common Mistakes

- Mutating `alias[T]` directly: rejected by design (`cannot mutably borrow alias[T]`).
- Returning/storing `ref[T]`/`mutref[T]`: rejected as escaping views.
- Reusing moved `own[T]`: rejected (`use after move of own[T]`).

## Decision Rule

- Need exclusive mutation transfer: use `own[T]`.
- Need read-only fan-out: convert once with `freeze` to `alias[T]`.
- Need high-contention mutation: use command channels; add mutex only where critical sections are unavoidable.
