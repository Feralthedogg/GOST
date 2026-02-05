module context

import "std/error"

struct Context {
    done: chan[unit];
    err: shared[error];
}

struct Cancel {
    ch: chan[unit];
}

fn background() -> Context {
    let e = shared_new[error](nil);
    let d = make_chan[unit](0);
    return Context { done: d, err: e };
}

fn done(ctx: &Context) -> chan[unit] {
    return ctx.done;
}

fn err(ctx: &Context) -> error {
    let p = shared_get[error](&ctx.err);
    return *p;
}

fn cancel(c: Cancel) {
    close(c.ch);
}

fn with_cancel(parent: Context) -> (Context, Cancel) {
    let d = make_chan[unit](0);
    let e = shared_new[error](nil);
    let cch = make_chan[unit](0);
    let c = Cancel { ch: cch };

    go fn() {
        select {
            case recv(parent.done) => {
                let pe = shared_get[error](&parent.err);
                let ce = shared_get[error](&e);
                *ce = *pe;
                close(d);
            },
            case recv(cch) => {
                let ce = shared_get[error](&e);
                *ce = error_new("context canceled");
                close(d);
            },
        }
    }();

    return (Context { done: d, err: e }, c);
}

fn with_timeout(parent: Context, ms: i32) -> (Context, Cancel) {
    let d = make_chan[unit](0);
    let e = shared_new[error](nil);
    let cch = make_chan[unit](0);
    let c = Cancel { ch: cch };

    go fn() {
        select {
            case recv(parent.done) => {
                let pe = shared_get[error](&parent.err);
                let ce = shared_get[error](&e);
                *ce = *pe;
                close(d);
            },
            case recv(cch) => {
                let ce = shared_get[error](&e);
                *ce = error_new("context canceled");
                close(d);
            },
            case after(ms) => {
                let ce = shared_get[error](&e);
                *ce = error_new("context deadline exceeded");
                close(d);
            },
        }
    }();

    return (Context { done: d, err: e }, c);
}
