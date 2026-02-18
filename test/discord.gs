module main

import "std/fmt"
import "std/net"
import "std/net/socket"

fn bot_token() -> string {
    return "BOT_TOKEN"
}

fn channel_id() -> string {
    return "CHANNEL_ID"
}

fn zero_i64() -> i64 { return string_len("")
 }
fn one_i64() -> i64 { return string_len("a")
 }

fn byte(c: string) -> i32 {
    return string_get(c, zero_i64())
}

fn str_eq(a: string, b: string) -> bool {
    let na: i64 = string_len(a)
    let nb: i64 = string_len(b)
    if na != nb { return false
 }
    let i: i64 = zero_i64()
    while i < na {
        if string_get(a, i) != string_get(b, i) { return false
 }
        i = i + one_i64()
    }
    return true
}

fn find_sub(hay: string, needle: string, start: i64) -> i64 {
    let nh: i64 = string_len(hay)
    let nn: i64 = string_len(needle)
    if nn == zero_i64() { return start
 }
    let i: i64 = start
    while i + nn <= nh {
        let j: i64 = zero_i64()
        let ok: bool = true
        while j < nn {
            if string_get(hay, i + j) != string_get(needle, j) {
                ok = false
                break
            }
            j = j + one_i64()
        }
        if ok { return i
 }
        i = i + one_i64()
    }
    return zero_i64() - one_i64()
}

fn is_ws(b: i32) -> bool {
    return b == byte(" ") || b == byte("\n") || b == byte("\r") || b == byte("\t")
}

fn json_key_value_pos(obj: string, key: string, start: i64) -> i64 {
    let qkey: string = string_concat(string_concat("\"", key), "\"")
    let pos: i64 = find_sub(obj, qkey, start)
    if pos < zero_i64() { return zero_i64() - one_i64()
 }
    let i: i64 = pos + string_len(qkey)
    let n: i64 = string_len(obj)
    while i < n && is_ws(string_get(obj, i)) {
        i = i + one_i64()
    }
    if i >= n || string_get(obj, i) != byte(":") {
        return zero_i64() - one_i64()
    }
    i = i + one_i64()
    while i < n && is_ws(string_get(obj, i)) {
        i = i + one_i64()
    }
    return i
}

fn json_string_field_from(obj: string, key: string, start: i64) -> string {
    let i: i64 = json_key_value_pos(obj, key, start)
    if i < zero_i64() { return ""
 }
    let n: i64 = string_len(obj)
    if i >= n || string_get(obj, i) != byte("\"") { return "" }
    i = i + one_i64()
    let out: string = ""
    let esc: bool = false

    while i < n {
        let c: i32 = string_get(obj, i)
        if esc {
            out = string_concat(out, string_slice(obj, i, one_i64()))
            esc = false
            i = i + one_i64()
            continue
        }
        if c == byte("\\") {
            esc = true
            i = i + one_i64()
            continue
        }
        if c == byte("\"") {
            return out
        }
        out = string_concat(out, string_slice(obj, i, one_i64()))
        i = i + one_i64()
    }
    return ""
}

fn json_string_field(obj: string, key: string) -> string {
    return json_string_field_from(obj, key, zero_i64())
}

fn json_object_field_pos_from(obj: string, key: string, start: i64) -> i64 {
    let i: i64 = json_key_value_pos(obj, key, start)
    if i < zero_i64() { return zero_i64() - one_i64()
 }
    let n: i64 = string_len(obj)
    if i >= n { return zero_i64() - one_i64()
 }
    if string_get(obj, i) != byte("{") { return zero_i64() - one_i64()
 }
    return i
}

fn json_int_field_from(obj: string, key: string, start: i64) -> i64 {
    let i: i64 = json_key_value_pos(obj, key, start)
    if i < zero_i64() { return zero_i64() - one_i64()
 }
    let n: i64 = string_len(obj)
    if i >= n { return zero_i64() - one_i64()
 }

    let neg: bool = false
    if string_get(obj, i) == byte("-") {
        neg = true
        i = i + one_i64()
    }

    let got: bool = false
    let v: i64 = zero_i64()
    while i < n {
        let c: i32 = string_get(obj, i)
        if c < byte("0") || c > byte("9") { break
 }
        got = true
        v = v * 10 + (c - byte("0"))
        i = i + one_i64()
    }
    if !got { return zero_i64() - one_i64()
 }
    if neg { return zero_i64() - v
 }
    return v
}

fn json_int_field(obj: string, key: string) -> i64 {
    return json_int_field_from(obj, key, zero_i64())
}

fn json_bool_field_from(obj: string, key: string, start: i64) -> i32 {
    let i: i64 = json_key_value_pos(obj, key, start)
    if i < zero_i64() { return -1
 }
    if find_sub(obj, "true", i) == i { return 1
 }
    if find_sub(obj, "false", i) == i { return 0
 }
    return -1
}

fn digit_char(d: i64) -> string {
    if d == 0 { return "0"
 }
    if d == 1 { return "1"
 }
    if d == 2 { return "2"
 }
    if d == 3 { return "3"
 }
    if d == 4 { return "4"
 }
    if d == 5 { return "5"
 }
    if d == 6 { return "6"
 }
    if d == 7 { return "7"
 }
    if d == 8 { return "8"
 }
    return "9"
}

fn i64_to_string(v: i64) -> string {
    if v == zero_i64() { return "0"
 }
    if v < zero_i64() { return "0"
 }
    let n: i64 = v
    let out: string = ""
    while n > zero_i64() {
        let d: i64 = n % 10
        let ch: string = digit_char(d)
        out = string_concat(ch, out)
        n = n / 10
    }
    return out
}

fn utf8_from_bytes4(b0: i32, b1: i32, b2: i32, b3: i32) -> string {
    let s: string = ""
    s = string_concat(s, string_from_byte(b0))
    s = string_concat(s, string_from_byte(b1))
    s = string_concat(s, string_from_byte(b2))
    s = string_concat(s, string_from_byte(b3))
    return s
}

fn utf8_ping_kr() -> string {
    // "?? = 237 149 145
    let s: string = "!"
    s = string_concat(s, string_from_byte(237))
    s = string_concat(s, string_from_byte(149))
    s = string_concat(s, string_from_byte(145))
    return s
}

fn utf8_pong_kr() -> string {
    // "?PING? = 237 144 129 240 159 143 147
    let s: string = ""
    s = string_concat(s, string_from_byte(237))
    s = string_concat(s, string_from_byte(144))
    s = string_concat(s, string_from_byte(129))
    s = string_concat(s, utf8_from_bytes4(240, 159, 143, 147))
    return s
}

fn is_ping_cmd(content: string) -> bool {
    if str_eq(content, "!ping") { return true
 }
    if str_eq(content, utf8_ping_kr()) { return true
 }
    return false
}

fn discord_headers(token: string) -> string {
    let h: string = string_concat("Authorization: Bot ", token)
    let h2: string = string_concat(h, "\nUser-Agent: gost-discord-bot (gost, 0.1)")
    return string_concat(h2, "\nAccept: application/json")
}

fn post_url(ch: string) -> string {
    let u: string = "https://discord.com/api/v10/channels/"
    let u2: string = string_concat(u, ch)
    return string_concat(u2, "/messages")
}

fn gateway_url() -> string {
    return "wss://gateway.discord.gg/?v=10&encoding=json"
}

fn send_pong(ch: string, headers: string) -> bool {
    let p: string = utf8_pong_kr()
    let body: string = string_concat(string_concat("{\"content\":\"", p), "\"}")
    let r = http_request_headers("POST", post_url(ch), body, "application/json", headers)
    match r {
        Result.Ok(resp) => {
            if resp.status == 200 || resp.status == 201 {
                return true
            }
            return false
        },
        Result.Err(_) => {
            return false
        },
        _ => {
            return false
        },
    }
    return false
}

fn send_identify(ws: WebSocket, token: string) -> bool {
    let intents: string = "37377"
    let p0: string = "{\"op\":2,\"d\":{\"token\":\""
    let p1: string = string_concat(p0, token)
    let p2: string = string_concat(p1, "\",\"intents\":")
    let p3: string = string_concat(p2, intents)
    let p4: string = string_concat(p3, ",\"properties\":{\"os\":\"windows\",\"browser\":\"gost\",\"device\":\"gost\"}}}")
    let sr = ws_send_text(ws, p4)
    match sr {
        Result.Ok(_) => { return true
 },
        Result.Err(_) => { return false
 },
        _ => { return false
 },
    }
    return false
}

fn send_heartbeat(ws: WebSocket, seq: i64) -> bool {
    let payload: string = ""
    if seq < zero_i64() {
        payload = "{\"op\":1,\"d\":null}"
    } else {
        let s: string = i64_to_string(seq)
        let a: string = "{\"op\":1,\"d\":"
        payload = string_concat(string_concat(a, s), "}")
    }
    let sr = ws_send_text(ws, payload)
    match sr {
        Result.Ok(_) => { return true
 },
        Result.Err(_) => { return false
 },
        _ => { return false
 },
    }
    return false
}

fn recv_hello_interval(ws: WebSocket) -> i64 {
    let attempts: i64 = zero_i64()
    while attempts < 300 {
        let msg: string = ws_recv_text(ws)
        let st: i32 = ws_last_status()
        if st == 0 {
            let op: i64 = json_int_field(msg, "op")
            if op == 10 {
                return json_int_field(msg, "heartbeat_interval")
            }
        } else {
            if st < 0 {
                return zero_i64() - one_i64()
            }
        }
        select {
            case after(200) => {},
        }
        attempts = attempts + one_i64()
    }
    return zero_i64() - one_i64()
}

fn main() -> i32 {
    let token: string = bot_token()
    let ch: string = channel_id()
    if str_eq(token, "YOUR_BOT_TOKEN") || string_len(token) == zero_i64() {
        println("set bot_token() in discord.gs")
        return 1
    }
    if str_eq(ch, "YOUR_CHANNEL_ID") || string_len(ch) == zero_i64() {
        println("set channel_id() in discord.gs")
        return 1
    }

    if singleton_acquire("gost.discord.bot.singleton") == 0 {
        println("another local bot instance is already running")
        return 1
    }

    let headers: string = discord_headers(token)
    let ws: WebSocket = WebSocket { id = zero_i64() }
    let c = ws_connect(gateway_url())
    match c {
        Result.Ok(v) => {
            ws = v
        },
        Result.Err(_) => {
            let e: string = last_error()
            if string_len(e) > zero_i64() {
                println(string_concat("ws connect failed: ", e))
            } else {
                println("ws connect failed")
            }
            return 1
        },
        _ => {
            println("ws connect failed")
            return 1
        },
    }

    println("discord websocket bot started. waiting for !ping / !KR-ping")
    let hb_interval: i64 = recv_hello_interval(ws)
    if hb_interval <= zero_i64() {
        println("failed to receive hello")
        ws_close(ws)
        return 1
    }
    if !send_identify(ws, token) {
        println("identify failed")
        ws_close(ws)
        return 1
    }

    let seq: i64 = zero_i64() - one_i64()
    let elapsed: i64 = zero_i64()
    let self_user_id: string = ""
    let last_replied_msg_id: string = ""

    loop {
        if elapsed >= hb_interval {
            if !send_heartbeat(ws, seq) {
                println("heartbeat send failed")
                break
            }
            elapsed = zero_i64()
        }

        let msg: string = ws_recv_text(ws)
        let st: i32 = ws_last_status()
        if st == 0 {
            let op: i64 = json_int_field(msg, "op")
            if op == 0 {
                let s: i64 = json_int_field(msg, "s")
                if s >= zero_i64() { seq = s
 }
                let t: string = json_string_field(msg, "t")
                if str_eq(t, "READY") {
                    let d_obj: i64 = json_object_field_pos_from(msg, "d", zero_i64())
                    let d_idx: i64 = if d_obj < zero_i64() { zero_i64() } else { d_obj }
                    let user_idx: i64 = json_object_field_pos_from(msg, "user", d_idx)
                    if user_idx >= zero_i64() {
                        let uid: string = json_string_field_from(msg, "id", user_idx)
                        if string_len(uid) > zero_i64() {
                            self_user_id = uid
                            println(string_concat("bot user_id: ", self_user_id))
                        }
                    }
                }
                if str_eq(t, "MESSAGE_CREATE") {
                    let d_obj: i64 = json_object_field_pos_from(msg, "d", zero_i64())
                    let d_idx: i64 = if d_obj < zero_i64() { zero_i64() } else { d_obj }
                    let msg_id: string = json_string_field_from(msg, "id", d_idx)
                    let author_idx: i64 = json_object_field_pos_from(msg, "author", d_idx)
                    let author_id: string = ""
                    let author_bot: i32 = -1
                    if author_idx >= zero_i64() {
                        author_id = json_string_field_from(msg, "id", author_idx)
                        author_bot = json_bool_field_from(msg, "bot", author_idx)
                    }
                    let from_bot: bool = author_bot == 1
                    let from_self: bool = string_len(self_user_id) > zero_i64() && str_eq(author_id, self_user_id)
                    if from_bot || from_self {
                        select {
                            case after(500) => {},
                        }
                        elapsed = elapsed + 500
                        continue
                    }

                    let content: string = json_string_field_from(msg, "content", d_idx)
                    let msg_ch: string = json_string_field_from(msg, "channel_id", d_idx)
                    println(string_concat("message channel_id: ", msg_ch))
                    println(string_concat("message content: ", content))
                    let is_ping: bool = is_ping_cmd(content)
                    let channel_ok: bool = str_eq(msg_ch, ch) || string_len(ch) == zero_i64()
                    if is_ping && channel_ok {
                        let dupe: bool = string_len(msg_id) > zero_i64() && str_eq(msg_id, last_replied_msg_id)
                        if dupe {
                            println("duplicate message id skipped")
                        } else {
                            if send_pong(ch, headers) {
                                if string_len(msg_id) > zero_i64() {
                                    last_replied_msg_id = msg_id
                                }
                                println("ping detected -> pong sent")
                            } else {
                                let e: string = last_error()
                                if string_len(e) > zero_i64() {
                                    println(string_concat("send failed: ", e))
                                } else {
                                    println("send failed")
                                }
                            }
                        }
                    } else {
                        if is_ping && !channel_ok {
                            println("ping matched but channel_id mismatch")
                        }
                    }
                }
            } else {
                if op == 7 {
                    println("gateway requested reconnect")
                    break
                } else {
                    if op == 9 {
                        println("invalid session")
                        break
                    }
                }
            }
        } else {
            if st == 1 {
                println("gateway closed")
                break
            } else {
                if st < 0 {
                    let e: string = last_error()
                    if string_len(e) > zero_i64() {
                        println(string_concat("ws error: ", e))
                    } else {
                        println("ws error")
                    }
                    break
                }
            }
        }

        select {
            case after(500) => {},
        }
        elapsed = elapsed + 500
    }

    ws_close(ws)
    return 0
}
