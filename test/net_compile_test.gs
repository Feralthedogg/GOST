module main

import "std/net"
import "std/net/socket"

fn z() -> i64 { return string_len(""); }

fn main() -> i32 {
    let _st = last_status();
    let _hs = last_http_status();
    let _le = last_error();
    let _lp = last_peer();

    let l = TcpListener { id = z() };
    let c = TcpConn { id = z() };
    let u = UdpConn { id = z() };

    let _r1 = tcp_listen("127.0.0.1:0");
    let _r2 = tcp_accept(l);
    let _r3 = tcp_accept_with_peer(l);
    let _r4 = tcp_connect("127.0.0.1:1");
    let _r5 = tcp_close(c);
    let _r6 = tcp_write(c, "x");
    let _r7 = tcp_read(c, 16);

    let _u1 = udp_bind("127.0.0.1:0");
    let _u2 = udp_connect("127.0.0.1:53");
    let _u3 = udp_close(u);
    let _u4 = udp_send(u, "abc");
    let _u5 = udp_send_to(u, "127.0.0.1:53", "abc");
    let _u6 = udp_recv(u, 16);
    let _u7 = udp_recv_from(u, 16);

    let _h1 = http_request("GET", "https://example.com", "", "");
    let _h2 = http_get("http://example.com");
    let _h3 = http_post("http://example.com", "x");
    let _h4 = https_get("https://example.com");
    let _h5 = https_post("https://example.com", "x");
    let _h6 = https_request("GET", "https://example.com", "", "");
    let _h7 = http_request_headers("GET", "https://example.com", "", "", "Authorization: Bot abc");

    let ws = WebSocket { id = z() };
    let _w1 = ws_connect("wss://gateway.discord.gg/?v=10&encoding=json");
    let _w2 = ws_send_text(ws, "{}");
    let _w3 = ws_recv_text(ws);
    let _w4 = ws_last_status();
    let _w5 = ws_close(ws);

    return 0;
}
