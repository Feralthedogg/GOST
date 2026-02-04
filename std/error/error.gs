module error

fn error_new(msg: string) -> error {
    return __gost_error_new(msg);
}
