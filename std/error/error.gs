module error

fn error_new(msg: string) -> error {
    return __gost_error_new(msg)
}

fn new(msg: string) -> error {
    return __gost_error_new(msg)
}

fn message(err: error) -> string {
    if err == nil {
        return ""
    }
    return __gost_error_message(err)
}

fn is_nil(err: error) -> bool {
    return err == nil
}

fn non_nil(err: error) -> bool {
    return err != nil
}
