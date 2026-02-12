module socket

copy struct WebSocket {
    id: i64;
}

fn ws_error_new(msg: string) -> error {
    return __gost_error_new(msg);
}

fn ws_non_empty_error(fallback: string) -> error {
    let msg = __gost_net_last_error();
    if string_len(msg) == string_len("") {
        return ws_error_new(fallback);
    }
    return ws_error_new(msg);
}

fn ws_last_status() -> i32 {
    return __gost_net_last_status();
}

fn ws_connect(url: string) -> Result[WebSocket, error] {
    let id = __gost_net_ws_connect(url);
    if id == 0 {
        return Result.Err[WebSocket, error](ws_non_empty_error("ws connect failed"));
    }
    return Result.Ok[WebSocket, error](WebSocket { id = id });
}

fn ws_close(ws: WebSocket) -> i32 {
    return __gost_net_ws_close(ws.id);
}

fn ws_send_text(ws: WebSocket, payload: string) -> Result[i32, error] {
    let rc = __gost_net_ws_send_text(ws.id, payload);
    if rc < 0 {
        return Result.Err[i32, error](ws_non_empty_error("ws send failed"));
    }
    return Result.Ok[i32, error](rc);
}

// Returns text frame and updates ws_last_status():
// 0=ok(text), 1=closed, 2=would-block/no frame now, -1=error.
fn ws_recv_text(ws: WebSocket) -> string {
    return __gost_net_ws_recv_text(ws.id);
}
