module main

import "std/fmt"
import "std/net"

copy struct WorkerStat {
    attempts: i32;
    success: i32;
    connect_fail: i32;
    write_fail: i32;
    read_fail: i32;
    bad_response: i32;
}

fn blt_zero_i64() -> i64 { return string_len(""); }
fn blt_one_i64() -> i64 { return string_len("a"); }

fn blt_digit_char_i32(d: i32) -> string {
    if d == 0 { return "0"; }
    if d == 1 { return "1"; }
    if d == 2 { return "2"; }
    if d == 3 { return "3"; }
    if d == 4 { return "4"; }
    if d == 5 { return "5"; }
    if d == 6 { return "6"; }
    if d == 7 { return "7"; }
    if d == 8 { return "8"; }
    return "9";
}

fn blt_i32_to_string(v: i32) -> string {
    if v == 0 { return "0"; }
    if v < 0 { return "0"; }
    let n: i32 = v;
    let out: string = "";
    while n > 0 {
        let d: i32 = n % 10;
        out = string_concat(blt_digit_char_i32(d), out);
        n = n / 10;
    }
    return out;
}

fn blt_starts_with(s: string, prefix: string) -> bool {
    let ns: i64 = string_len(s);
    let np: i64 = string_len(prefix);
    if np > ns { return false; }
    let i: i64 = blt_zero_i64();
    while i < np {
        if string_get(s, i) != string_get(prefix, i) { return false; }
        i = i + blt_one_i64();
    }
    return true;
}

fn blt_contains(hay: string, needle: string) -> bool {
    let nh: i64 = string_len(hay);
    let nn: i64 = string_len(needle);
    if nn == blt_zero_i64() { return true; }
    if nn > nh { return false; }
    let i: i64 = blt_zero_i64();
    while i + nn <= nh {
        let j: i64 = blt_zero_i64();
        let ok = true;
        while j < nn {
            if string_get(hay, i + j) != string_get(needle, j) {
                ok = false;
                j = nn;
            } else {
                j = j + blt_one_i64();
            }
        }
        if ok { return true; }
        i = i + blt_one_i64();
    }
    return false;
}

fn blt_is_api_request(req: string) -> bool {
    if blt_starts_with(req, "GET /api ") { return true; }
    if blt_starts_with(req, "POST /api ") { return true; }
    return false;
}

fn blt_is_ok_response(resp: string) -> bool {
    if !blt_starts_with(resp, "HTTP/1.1 200") { return false; }
    return blt_contains(resp, "Hello, World!");
}

fn blt_write_ok(conn: TcpConn) {
    let head = "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 13\r\nConnection: close\r\n\r\n";
    let body = "Hello, World!";
    let _ = tcp_write(conn, string_concat(head, body));
}

fn blt_write_not_found(conn: TcpConn) {
    let head = "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 9\r\nConnection: close\r\n\r\n";
    let body = "Not Found";
    let _ = tcp_write(conn, string_concat(head, body));
}

fn blt_handle_conn(conn: TcpConn) {
    match tcp_read(conn, 2048) {
        Result.Ok(req) => {
            if blt_is_api_request(req) {
                blt_write_ok(conn);
            } else {
                blt_write_not_found(conn);
            }
        },
        Result.Err(_) => {},
    }
    let _ = tcp_close(conn);
}

fn blt_accept_loop(listener: ref[TcpListener], accepted: ref[shared[i32]], stop: ref[shared[i32]]) {
    loop {
        let s0 = shared_get[i32](&*stop);
        if *s0 != 0 {
            return;
        }
        match tcp_accept(*listener) {
            Result.Ok(conn) => {
                let s1 = shared_get[i32](&*stop);
                if *s1 != 0 {
                    let _ = tcp_close(conn);
                    return;
                } else {
                    let c = shared_get[i32](&*accepted);
                    *c = *c + 1;
                    go blt_handle_conn(conn);
                }
            },
            Result.Err(_) => {},
        }
    }
}

// result code
// 0: success
// 1: connect fail
// 2: write fail
// 3: read fail
// 4: bad response
fn blt_client_once(addr: string) -> i32 {
    let req = "GET /api HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n";
    let conn: TcpConn = TcpConn { id = blt_zero_i64() };
    match tcp_connect(addr) {
        Result.Ok(v) => { conn = v; },
        Result.Err(_) => { return 1; },
    }

    match tcp_write(conn, req) {
        Result.Ok(_) => {},
        Result.Err(_) => {
            let _ = tcp_close(conn);
            return 2;
        },
    }

    let resp = "";
    let done = false;
    let read_failed = false;
    while !done {
        match tcp_read(conn, 4096) {
            Result.Ok(chunk) => {
                if string_len(chunk) == blt_zero_i64() {
                    done = true;
                } else {
                    resp = string_concat(resp, chunk);
                    if blt_contains(resp, "Hello, World!") {
                        done = true;
                    }
                }
            },
            Result.Err(_) => {
                read_failed = true;
                done = true;
            },
        }
    }
    let _ = tcp_close(conn);
    if read_failed {
        return 3;
    }
    if blt_is_ok_response(resp) {
        return 0;
    }
    return 4;
}

fn blt_worker(addr: string, iters: i32, stat_ch: ref[chan[WorkerStat]]) {
    let i: i32 = 0;
    let st = WorkerStat {
        attempts = 0,
        success = 0,
        connect_fail = 0,
        write_fail = 0,
        read_fail = 0,
        bad_response = 0,
    };
    while i < iters {
        let sent = false;
        while !sent {
            let rc = blt_client_once(addr);
            st.attempts = st.attempts + 1;
            if rc == 0 {
                st.success = st.success + 1;
                sent = true;
            } else {
                if rc == 1 {
                    st.connect_fail = st.connect_fail + 1;
                } else {
                    if rc == 2 {
                        st.write_fail = st.write_fail + 1;
                    } else {
                        if rc == 3 {
                            st.read_fail = st.read_fail + 1;
                        } else {
                            st.bad_response = st.bad_response + 1;
                        }
                    }
                }
            }
        }
        i = i + 1;
    }
    send(*stat_ch, st);
}

fn main() -> i32 {
    let addr = "127.0.0.1:18080";
    let workers: i32 = 100;
    let iters_per_worker: i32 = 100;
    let total_requests: i32 = workers * iters_per_worker;

    let listener: TcpListener = TcpListener { id = blt_zero_i64() };
    match tcp_listen(addr) {
        Result.Ok(v) => { listener = v; },
        Result.Err(_) => {
            let msg = last_error();
            if string_len(msg) > blt_zero_i64() {
                println(string_concat("listen failed: ", msg));
            } else {
                println("listen failed");
            }
            return 1;
        },
    }

    println("backend goroutine stress test started");
    println("target: http://127.0.0.1:18080/api");
    println(string_concat("workers=", blt_i32_to_string(workers)));
    println(string_concat("iters_per_worker=", blt_i32_to_string(iters_per_worker)));
    println(string_concat("logical_requests=", blt_i32_to_string(total_requests)));
    println("load generation started...");
    let accepted_counter = shared_new[i32](0);
    let accept_stop = shared_new[i32](0);
    go blt_accept_loop(&listener, &accepted_counter, &accept_stop);
    let stat_ch = make_chan[WorkerStat](workers);
    let slots = make_slice[i32](workers, workers);
    for _ in &slots {
        go blt_worker(addr, iters_per_worker, &stat_ch);
    }

    let total_attempts: i32 = 0;
    let total_success: i32 = 0;
    let total_connect_fail: i32 = 0;
    let total_write_fail: i32 = 0;
    let total_read_fail: i32 = 0;
    let total_bad_response: i32 = 0;
    let got_workers: i32 = 0;
    while got_workers < workers {
        select {
            case recv(stat_ch) => |st, ok| {
                if ok {
                    got_workers = got_workers + 1;
                    total_attempts = total_attempts + st.attempts;
                    total_success = total_success + st.success;
                    total_connect_fail = total_connect_fail + st.connect_fail;
                    total_write_fail = total_write_fail + st.write_fail;
                    total_read_fail = total_read_fail + st.read_fail;
                    total_bad_response = total_bad_response + st.bad_response;
                }
            },
        }
    }

    let stop = shared_get[i32](&accept_stop);
    *stop = 1;
    // Wake accept loop so it can observe stop flag and exit.
    match tcp_connect(addr) {
        Result.Ok(c) => {
            let _ = tcp_close(c);
        },
        Result.Err(_) => {},
    }
    select { case after(200) => { } }

    let accepted: i32 = *shared_get[i32](&accepted_counter);
    let total_retries: i32 = total_attempts - total_success;
    let success_pct: i32 = 0;
    let retry_pct: i32 = 0;
    let connect_fail_pct: i32 = 0;
    let write_fail_pct: i32 = 0;
    let read_fail_pct: i32 = 0;
    let bad_resp_pct: i32 = 0;
    if total_attempts > 0 {
        success_pct = (total_success * 100) / total_attempts;
        retry_pct = (total_retries * 100) / total_attempts;
        connect_fail_pct = (total_connect_fail * 100) / total_attempts;
        write_fail_pct = (total_write_fail * 100) / total_attempts;
        read_fail_pct = (total_read_fail * 100) / total_attempts;
        bad_resp_pct = (total_bad_response * 100) / total_attempts;
    }

    select { case after(500) => { } }
    println("========== detailed result ==========");
    println(string_concat("logical requests: ", blt_i32_to_string(total_requests)));
    println(string_concat("accepted connections: ", blt_i32_to_string(accepted)));
    println(string_concat("worker reports: ", blt_i32_to_string(got_workers)));
    println(string_concat("total attempts: ", blt_i32_to_string(total_attempts)));
    println(string_concat("successful attempts: ", blt_i32_to_string(total_success)));
    println(string_concat("retries: ", blt_i32_to_string(total_retries)));
    println(string_concat("success rate(%): ", blt_i32_to_string(success_pct)));
    println(string_concat("retry rate(%): ", blt_i32_to_string(retry_pct)));
    println(string_concat("connect fail: ", blt_i32_to_string(total_connect_fail)));
    println(string_concat("connect fail rate(%): ", blt_i32_to_string(connect_fail_pct)));
    println(string_concat("write fail: ", blt_i32_to_string(total_write_fail)));
    println(string_concat("write fail rate(%): ", blt_i32_to_string(write_fail_pct)));
    println(string_concat("read fail: ", blt_i32_to_string(total_read_fail)));
    println(string_concat("read fail rate(%): ", blt_i32_to_string(read_fail_pct)));
    println(string_concat("bad response: ", blt_i32_to_string(total_bad_response)));
    println(string_concat("bad response rate(%): ", blt_i32_to_string(bad_resp_pct)));
    println("=====================================");
    println("load test finished");
    return 0;
}
