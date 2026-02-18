module main

import "std/fmt"
import "std/net"

fn zero_i64() -> i64 { return string_len("")
 }
fn one_i64() -> i64 { return string_len("a")
 }

fn backend_starts_with(s: string, prefix: string) -> bool {
    let ns: i64 = string_len(s)
    let np: i64 = string_len(prefix)
    if np > ns {
        return false
    }
    let i: i64 = zero_i64()
    while i < np {
        if string_get(s, i) != string_get(prefix, i) {
            return false
        }
        i = i + one_i64()
    }
    return true
}

fn is_api_request(req: string) -> bool {
    if backend_starts_with(req, "GET /api ") {
        return true
    }
    if backend_starts_with(req, "POST /api ") {
        return true
    }
    return false
}

fn write_ok(conn: TcpConn) {
    let head = "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 13\r\nConnection: close\r\n\r\n"
    let body = "Hello, World!"
    let _ = tcp_write(conn, string_concat(head, body))
}

fn write_not_found(conn: TcpConn) {
    let head = "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 9\r\nConnection: close\r\n\r\n"
    let body = "Not Found"
    let _ = tcp_write(conn, string_concat(head, body))
}

fn handle_conn(conn: TcpConn) {
    match tcp_read(conn, 4096) {
        Result.Ok(req) => {
            if is_api_request(req) {
                write_ok(conn)
            } else {
                write_not_found(conn)
            }
        },
        Result.Err(_) => {
            let _ = tcp_close(conn)
            return
        },
    }
    let _ = tcp_close(conn)
}

fn main() -> i32 {
    let listener: TcpListener = TcpListener { id = zero_i64() }
    match tcp_listen("127.0.0.1:8080") {
        Result.Ok(v) => {
            listener = v
        },
        Result.Err(_) => {
            let msg = last_error()
            if string_len(msg) > zero_i64() {
                println(string_concat("listen failed: ", msg))
            } else {
                println("listen failed")
            }
            return 1
        },
    }

    println("backend_test server listening on http://127.0.0.1:8080")
    println("try: curl http://127.0.0.1:8080/api")
    loop {
        match tcp_accept(listener) {
            Result.Ok(conn) => {
                go handle_conn(conn)
            },
            Result.Err(_) => {},
        }
    }
    return 0
}
