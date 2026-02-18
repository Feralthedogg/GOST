module http

copy struct Request {
    method: string
    url: string
    body: string
    content_type: string
    headers: string
}

copy struct Response {
    status: i32
    body: string
}

copy struct Client {
    user_agent: string
    timeout_ms: i32
}

private fn err_new(msg: string) -> error {
    return __gost_error_new(msg)
}

private fn starts_with(s: string, prefix: string) -> bool {
    let ns: i64 = string_len(s)
    let np: i64 = string_len(prefix)
    if np > ns {
        return false
    }
    let i: i64 = 0
    while i < np {
        if string_get(s, i) != string_get(prefix, i) {
            return false
        }
        i = i + 1
    }
    return true
}

fn default_client() -> Client {
    return Client {
        user_agent = "gost-http/1.0",
        timeout_ms = 30000,
    }
}

fn new_request(method: string, url: string, body: string) -> Request {
    return Request {
        method = method,
        url = url,
        body = body,
        content_type = "application/octet-stream",
        headers = "",
    }
}

private fn url_ok(url: string) -> bool {
    return starts_with(url, "http://") || starts_with(url, "https://")
}

private fn req_error(fallback: string) -> error {
    let msg = __gost_net_last_error()
    if string_len(msg) == 0 {
        return err_new(fallback)
    }
    return err_new(msg)
}

fn do(client: Client, req: Request) -> Result[Response, error] {
    let _ = client
    if !url_ok(req.url) {
        return Result.Err[Response, error](err_new("http url must start with http:// or https://"))
    }
    let out: string = ""
    if string_len(req.headers) == 0 {
        out = __gost_net_http_request(req.method, req.url, req.body, req.content_type)
    } else {
        out = __gost_net_http_request_headers(
            req.method,
            req.url,
            req.body,
            req.content_type,
            req.headers,
        )
    }
    if __gost_net_last_status() != 0 {
        return Result.Err[Response, error](req_error("http request failed"))
    }
    return Result.Ok[Response, error](Response {
        status = __gost_net_last_http_status(),
        body = out,
    })
}

fn get(url: string) -> Result[Response, error] {
    let c = default_client()
    let req = new_request("GET", url, "")
    return do(c, req)
}

fn post(url: string, body: string) -> Result[Response, error] {
    let c = default_client()
    let req = Request {
        method = "POST",
        url = url,
        body = body,
        content_type = "application/octet-stream",
        headers = "",
    }
    return do(c, req)
}

