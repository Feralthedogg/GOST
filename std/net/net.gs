module net

copy struct TcpListener {
    id: i64;
}

copy struct TcpConn {
    id: i64;
}

copy struct TcpAccept {
    conn: TcpConn;
    peer: string;
}

copy struct UdpConn {
    id: i64;
}

copy struct UdpPacket {
    peer: string;
    data: string;
}

copy struct HttpResponse {
    status: i32;
    body: string;
}

fn starts_with(s: string, prefix: string) -> bool {
    let ns = string_len(s);
    let np = string_len(prefix);
    if np > ns {
        return false;
    }
    let i: i64 = string_len("");
    while i < np {
        if string_get(s, i) != string_get(prefix, i) {
            return false;
        }
        i = i + string_len("a");
    }
    return true;
}

fn is_http_url(url: string) -> bool {
    return starts_with(url, "http://");
}

fn is_https_url(url: string) -> bool {
    return starts_with(url, "https://");
}

fn error_new(msg: string) -> error {
    return __gost_error_new(msg);
}

fn singleton_acquire(name: string) -> i32 {
    return __gost_singleton_acquire(name);
}

fn non_empty_error(fallback: string) -> error {
    let msg = __gost_net_last_error();
    if string_len(msg) == string_len("") {
        return error_new(fallback);
    }
    return error_new(msg);
}

fn last_status() -> i32 {
    return __gost_net_last_status();
}

fn last_http_status() -> i32 {
    return __gost_net_last_http_status();
}

fn last_error() -> string {
    return __gost_net_last_error();
}

fn last_peer() -> string {
    return __gost_net_last_peer();
}

fn tcp_listen(addr: string) -> Result[TcpListener, error] {
    let id = __gost_net_tcp_listen(addr);
    if id == 0 {
        return Result.Err[TcpListener, error](non_empty_error("tcp listen failed"));
    }
    return Result.Ok[TcpListener, error](TcpListener { id = id });
}

fn tcp_accept(listener: TcpListener) -> Result[TcpConn, error] {
    let id = __gost_net_tcp_accept(listener.id);
    if id == 0 {
        return Result.Err[TcpConn, error](non_empty_error("tcp accept failed"));
    }
    return Result.Ok[TcpConn, error](TcpConn { id = id });
}

fn tcp_accept_with_peer(listener: TcpListener) -> Result[TcpAccept, error] {
    let id = __gost_net_tcp_accept(listener.id);
    if id == 0 {
        return Result.Err[TcpAccept, error](non_empty_error("tcp accept failed"));
    }
    return Result.Ok[TcpAccept, error](TcpAccept {
        conn = TcpConn { id = id },
        peer = last_peer(),
    });
}

fn tcp_connect(addr: string) -> Result[TcpConn, error] {
    let id = __gost_net_tcp_connect(addr);
    if id == 0 {
        return Result.Err[TcpConn, error](non_empty_error("tcp connect failed"));
    }
    return Result.Ok[TcpConn, error](TcpConn { id = id });
}

fn tcp_close(conn: TcpConn) -> i32 {
    return __gost_net_tcp_close(conn.id);
}

fn tcp_write(conn: TcpConn, data: string) -> Result[i64, error] {
    let n = __gost_net_tcp_write(conn.id, data);
    if n < 0 {
        return Result.Err[i64, error](non_empty_error("tcp write failed"));
    }
    return Result.Ok[i64, error](n);
}

fn tcp_read(conn: TcpConn, max_len: i32) -> Result[string, error] {
    let out = __gost_net_tcp_read(conn.id, max_len);
    let st = last_status();
    if st == 0 || st == 1 {
        return Result.Ok[string, error](out);
    }
    return Result.Err[string, error](non_empty_error("tcp read failed"));
}

fn udp_bind(addr: string) -> Result[UdpConn, error] {
    let id = __gost_net_udp_bind(addr);
    if id == 0 {
        return Result.Err[UdpConn, error](non_empty_error("udp bind failed"));
    }
    return Result.Ok[UdpConn, error](UdpConn { id = id });
}

fn udp_connect(addr: string) -> Result[UdpConn, error] {
    let id = __gost_net_udp_connect(addr);
    if id == 0 {
        return Result.Err[UdpConn, error](non_empty_error("udp connect failed"));
    }
    return Result.Ok[UdpConn, error](UdpConn { id = id });
}

fn udp_close(conn: UdpConn) -> i32 {
    return __gost_net_udp_close(conn.id);
}

fn udp_send(conn: UdpConn, data: string) -> Result[i64, error] {
    let n = __gost_net_udp_send(conn.id, data);
    if n < 0 {
        return Result.Err[i64, error](non_empty_error("udp send failed"));
    }
    return Result.Ok[i64, error](n);
}

fn udp_send_to(conn: UdpConn, addr: string, data: string) -> Result[i64, error] {
    let n = __gost_net_udp_send_to(conn.id, addr, data);
    if n < 0 {
        return Result.Err[i64, error](non_empty_error("udp send_to failed"));
    }
    return Result.Ok[i64, error](n);
}

fn udp_recv(conn: UdpConn, max_len: i32) -> Result[string, error] {
    let out = __gost_net_udp_recv(conn.id, max_len);
    if last_status() == 0 {
        return Result.Ok[string, error](out);
    }
    return Result.Err[string, error](non_empty_error("udp recv failed"));
}

fn udp_recv_from(conn: UdpConn, max_len: i32) -> Result[UdpPacket, error] {
    let out = __gost_net_udp_recv_from(conn.id, max_len);
    if last_status() != 0 {
        return Result.Err[UdpPacket, error](non_empty_error("udp recv_from failed"));
    }
    return Result.Ok[UdpPacket, error](UdpPacket {
        peer = last_peer(),
        data = out,
    });
}

fn http_request(method: string, url: string, body: string, content_type: string) -> Result[HttpResponse, error] {
    if !is_http_url(url) && !is_https_url(url) {
        return Result.Err[HttpResponse, error](error_new("http request url must start with http:// or https://"));
    }
    let out = __gost_net_http_request(method, url, body, content_type);
    if last_status() != 0 {
        return Result.Err[HttpResponse, error](non_empty_error("http request failed"));
    }
    return Result.Ok[HttpResponse, error](HttpResponse {
        status = last_http_status(),
        body = out,
    });
}

fn http_request_headers(
    method: string,
    url: string,
    body: string,
    content_type: string,
    headers: string,
) -> Result[HttpResponse, error] {
    if !is_http_url(url) && !is_https_url(url) {
        return Result.Err[HttpResponse, error](error_new("http request url must start with http:// or https://"));
    }
    let out = __gost_net_http_request_headers(method, url, body, content_type, headers);
    if last_status() != 0 {
        return Result.Err[HttpResponse, error](non_empty_error("http request failed"));
    }
    return Result.Ok[HttpResponse, error](HttpResponse {
        status = last_http_status(),
        body = out,
    });
}

fn http_get(url: string) -> Result[HttpResponse, error] {
    return http_request("GET", url, "", "");
}

fn http_post(url: string, body: string) -> Result[HttpResponse, error] {
    return http_request("POST", url, body, "application/octet-stream");
}

fn https_request(method: string, url: string, body: string, content_type: string) -> Result[HttpResponse, error] {
    if !is_https_url(url) {
        return Result.Err[HttpResponse, error](error_new("https request url must start with https://"));
    }
    return http_request(method, url, body, content_type);
}

fn https_get(url: string) -> Result[HttpResponse, error] {
    return https_request("GET", url, "", "");
}

fn https_post(url: string, body: string) -> Result[HttpResponse, error] {
    return https_request("POST", url, body, "application/octet-stream");
}
