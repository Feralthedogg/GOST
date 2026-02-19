module context

struct Context {
    done: chan[unit]
    err: shared[error]
}

struct Cancel {
    ch: chan[unit]
}

struct WithCancel {
    ctx: Context
    cancel: Cancel
}

// helpers to avoid struct literals with local names in callers
fn ctx_new(d: chan[unit], e: shared[error]) -> Context {
    return Context { done = d, err = e }
}

fn with_cancel_new(ctx: Context, cancel: Cancel) -> WithCancel {
    return WithCancel { ctx = ctx, cancel = cancel }
}

fn background() -> Context {
    let e = shared_new[error](nil)
    let d = make_chan[unit](0)
    return ctx_new(d, e)
}

// Local wrapper to avoid module qualification issues in early std import model.
fn error_new(msg: string) -> error {
    return __gost_error_new(msg)
}

fn done(ctx: Context) -> chan[unit] {
    return ctx.done
}

fn err(ctx: Context) -> error {
    let p = shared_get[error](&ctx.err)
    return *p
}

fn cancel(c: Cancel) {
    close(c.ch)
}

// Consume WithCancel to cancel and return its done channel in one move.
fn cancel_and_done(wc: WithCancel) -> chan[unit] {
    close(wc.cancel.ch)
    return wc.ctx.done
}

// Consume WithCancel and return its done channel.
fn done_chan(wc: WithCancel) -> chan[unit] {
    return wc.ctx.done
}

fn watch_cancel(parent: Context, d: chan[unit], e: shared[error], cch: chan[unit]) {
    let _unused_err = e
    select {
        case recv(parent.done) => {
            close(d)
        },
        case recv(cch) => {
            close(d)
        },
    }
}

fn watch_timeout(parent: Context, d: chan[unit], e: shared[error], cch: chan[unit], ms: i32) {
    let _unused_err = e
    select {
        case recv(parent.done) => {
            close(d)
        },
        case recv(cch) => {
            close(d)
        },
        case after(ms) => {
            close(d)
        },
    }
}

fn with_cancel(parent: Context) -> WithCancel {
    let d = make_chan[unit](0)
    let e = shared_new[error](nil)
    let cch = make_chan[unit](0)
    let c = Cancel { ch = cch }

    go watch_cancel(parent, d, e, cch)

    return with_cancel_new(ctx_new(d, e), c)
}

fn with_timeout(parent: Context, ms: i32) -> WithCancel {
    let d = make_chan[unit](0)
    let e = shared_new[error](nil)
    let cch = make_chan[unit](0)
    let c = Cancel { ch = cch }

    go watch_timeout(parent, d, e, cch, ms)

    return with_cancel_new(ctx_new(d, e), c)
}
