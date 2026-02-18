#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Arity {
    Any,
    Exactly(usize),
    AtLeast(usize),
    AtMost(usize),
}

impl Arity {
    fn matches(self, n: usize) -> bool {
        match self {
            Arity::Any => true,
            Arity::Exactly(v) => n == v,
            Arity::AtLeast(v) => n >= v,
            Arity::AtMost(v) => n <= v,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct IntrinsicSignature {
    pub type_args: Arity,
    pub args: Arity,
    pub type_args_error: Option<&'static str>,
    pub args_error: Option<&'static str>,
    pub internal_std_only_error: Option<&'static str>,
}

const fn sig(
    type_args: Arity,
    args: Arity,
    type_args_error: Option<&'static str>,
    args_error: Option<&'static str>,
    internal_std_only_error: Option<&'static str>,
) -> IntrinsicSignature {
    IntrinsicSignature {
        type_args,
        args,
        type_args_error,
        args_error,
        internal_std_only_error,
    }
}

const fn no_type_args(
    args: Arity,
    type_args_error: &'static str,
    args_error: Option<&'static str>,
    internal_std_only_error: Option<&'static str>,
) -> IntrinsicSignature {
    sig(
        Arity::Exactly(0),
        args,
        Some(type_args_error),
        args_error,
        internal_std_only_error,
    )
}

pub fn intrinsic_signature(name: &str) -> Option<IntrinsicSignature> {
    let sync_internal = Some("__gost_sync_* is internal to the standard library");
    let os_internal = Some("__gost_os_* is internal to the standard library");
    let net_internal = Some("__gost_net_* is internal to the standard library");
    match name {
        "panic" => Some(no_type_args(
            Arity::Exactly(1),
            "panic does not take type arguments",
            Some("panic expects 1 argument"),
            None,
        )),
        "recover" => Some(no_type_args(
            Arity::Exactly(0),
            "recover does not take type arguments",
            Some("recover expects 0 arguments"),
            None,
        )),
        "__gost_println" => Some(no_type_args(
            Arity::Exactly(1),
            "__gost_println does not take type arguments",
            Some("__gost_println expects 1 argument"),
            Some("__gost_println is internal to the standard library"),
        )),
        "string_len" => Some(no_type_args(
            Arity::Exactly(1),
            "string_len does not take type arguments",
            Some("string_len expects 1 argument"),
            None,
        )),
        "string_get" => Some(no_type_args(
            Arity::Exactly(2),
            "string_get does not take type arguments",
            Some("string_get expects 2 arguments"),
            None,
        )),
        "string_slice" => Some(no_type_args(
            Arity::Exactly(3),
            "string_slice does not take type arguments",
            Some("string_slice expects 3 arguments"),
            None,
        )),
        "string_concat" => Some(no_type_args(
            Arity::Exactly(2),
            "string_concat does not take type arguments",
            Some("string_concat expects 2 arguments"),
            None,
        )),
        "string_from_byte" => Some(no_type_args(
            Arity::Exactly(1),
            "string_from_byte does not take type arguments",
            Some("string_from_byte expects 1 argument"),
            None,
        )),
        "asm" | "asm_pure" | "asm_volatile" => Some(sig(
            Arity::AtMost(1),
            Arity::AtLeast(1),
            Some("asm expects at most 1 type argument (return type)"),
            Some("asm expects at least 1 argument (template string literal)"),
            None,
        )),
        "asm_label" => Some(no_type_args(
            Arity::Exactly(1),
            "asm_label does not take type arguments",
            Some("asm_label expects 1 argument"),
            None,
        )),
        "asm_goto" => Some(no_type_args(
            Arity::AtLeast(3),
            "asm_goto does not take type arguments",
            Some("asm_goto expects at least 3 arguments"),
            None,
        )),
        "__gost_error_new" => Some(no_type_args(
            Arity::Exactly(1),
            "__gost_error_new does not take type arguments",
            Some("__gost_error_new expects 1 argument"),
            Some("__gost_error_new is internal to the standard library"),
        )),
        "__gost_error_message" => Some(no_type_args(
            Arity::Exactly(1),
            "__gost_error_message does not take type arguments",
            Some("__gost_error_message expects 1 argument"),
            Some("__gost_error_message is internal to the standard library"),
        )),
        "__gost_singleton_acquire" => Some(no_type_args(
            Arity::Exactly(1),
            "__gost_singleton_acquire does not take type arguments",
            Some("__gost_singleton_acquire expects 1 argument"),
            Some("__gost_singleton_acquire is internal to the standard library"),
        )),
        "__gost_now_ms" => Some(no_type_args(
            Arity::Exactly(0),
            "__gost_now_ms does not take type arguments",
            Some("__gost_now_ms expects 0 arguments"),
            None,
        )),
        "__gost_process_exit" => Some(no_type_args(
            Arity::Exactly(1),
            "__gost_process_exit does not take type arguments",
            Some("__gost_process_exit expects 1 argument"),
            None,
        )),
        "__gost_sync_mutex_new" | "__gost_sync_waitgroup_new" | "__gost_sync_once_new" => {
            Some(no_type_args(
                Arity::Exactly(0),
                "__gost_sync_* does not take type arguments",
                Some("__gost_sync_* expects 0 arguments"),
                sync_internal,
            ))
        }
        "__gost_sync_mutex_lock"
        | "__gost_sync_mutex_try_lock"
        | "__gost_sync_mutex_unlock"
        | "__gost_sync_waitgroup_wait"
        | "__gost_sync_once_begin" => Some(no_type_args(
            Arity::Exactly(1),
            "__gost_sync_* does not take type arguments",
            Some("__gost_sync_* expects 1 argument"),
            sync_internal,
        )),
        "__gost_sync_waitgroup_add" => Some(no_type_args(
            Arity::Exactly(2),
            "__gost_sync_waitgroup_add does not take type arguments",
            Some("__gost_sync_waitgroup_add expects 2 arguments"),
            sync_internal,
        )),
        "__gost_os_last_status" => Some(no_type_args(
            Arity::Exactly(0),
            "__gost_os_last_status does not take type arguments",
            Some("__gost_os_last_status expects 0 arguments"),
            os_internal,
        )),
        "__gost_os_last_error" | "__gost_os_last_output" | "__gost_os_getwd" | "__gost_os_args" => {
            Some(no_type_args(
                Arity::Exactly(0),
                "__gost_os_* does not take type arguments",
                Some("__gost_os_* expects 0 arguments"),
                os_internal,
            ))
        }
        "__gost_os_exec" | "__gost_os_pipe" => Some(no_type_args(
            Arity::Exactly(1),
            "__gost_os_* does not take type arguments",
            Some("__gost_os_* expects 1 argument"),
            os_internal,
        )),
        "__gost_os_read_file" | "__gost_os_readdir" | "__gost_os_getenv" => Some(no_type_args(
            Arity::Exactly(1),
            "__gost_os_* does not take type arguments",
            Some("__gost_os_* expects 1 argument"),
            os_internal,
        )),
        "__gost_os_write_file" | "__gost_os_setenv" => Some(no_type_args(
            Arity::Exactly(2),
            "__gost_os_* does not take type arguments",
            Some("__gost_os_* expects 2 arguments"),
            os_internal,
        )),
        "__gost_os_remove" | "__gost_os_mkdir" | "__gost_os_chdir" | "__gost_os_stat_size" => {
            Some(no_type_args(
                Arity::Exactly(1),
                "__gost_os_* does not take type arguments",
                Some("__gost_os_* expects 1 argument"),
                os_internal,
            ))
        }
        "__gost_net_last_status" => Some(no_type_args(
            Arity::Exactly(0),
            "__gost_net_last_status does not take type arguments",
            Some("__gost_net_last_status expects 0 arguments"),
            net_internal,
        )),
        "__gost_net_last_http_status" => Some(no_type_args(
            Arity::Exactly(0),
            "__gost_net_last_http_status does not take type arguments",
            Some("__gost_net_last_http_status expects 0 arguments"),
            net_internal,
        )),
        "__gost_net_last_error" | "__gost_net_last_peer" => Some(no_type_args(
            Arity::Exactly(0),
            "__gost_net_last_error does not take type arguments",
            Some("__gost_net_last_error expects 0 arguments"),
            net_internal,
        )),
        "__gost_net_tcp_listen"
        | "__gost_net_tcp_connect"
        | "__gost_net_udp_bind"
        | "__gost_net_udp_connect"
        | "__gost_net_ws_connect" => Some(no_type_args(
            Arity::Exactly(1),
            "__gost_net_* does not take type arguments",
            Some("__gost_net_* expects 1 argument"),
            net_internal,
        )),
        "__gost_net_tcp_accept" => Some(no_type_args(
            Arity::Exactly(1),
            "__gost_net_tcp_accept does not take type arguments",
            Some("__gost_net_tcp_accept expects 1 argument"),
            net_internal,
        )),
        "__gost_net_tcp_close" | "__gost_net_udp_close" | "__gost_net_ws_close" => {
            Some(no_type_args(
                Arity::Exactly(1),
                "__gost_net_*_close does not take type arguments",
                Some("__gost_net_*_close expects 1 argument"),
                net_internal,
            ))
        }
        "__gost_net_ws_send_text" => Some(no_type_args(
            Arity::Exactly(2),
            "__gost_net_ws_send_text does not take type arguments",
            Some("__gost_net_ws_send_text expects 2 arguments"),
            net_internal,
        )),
        "__gost_net_tcp_write" | "__gost_net_udp_send" => Some(no_type_args(
            Arity::Exactly(2),
            "__gost_net_* does not take type arguments",
            Some("__gost_net_* expects 2 arguments"),
            net_internal,
        )),
        "__gost_net_udp_send_to" => Some(no_type_args(
            Arity::Exactly(3),
            "__gost_net_udp_send_to does not take type arguments",
            Some("__gost_net_udp_send_to expects 3 arguments"),
            net_internal,
        )),
        "__gost_net_tcp_read" | "__gost_net_udp_recv" | "__gost_net_udp_recv_from" => {
            Some(no_type_args(
                Arity::Exactly(2),
                "__gost_net_* does not take type arguments",
                Some("__gost_net_* expects 2 arguments"),
                net_internal,
            ))
        }
        "__gost_net_ws_recv_text" => Some(no_type_args(
            Arity::Exactly(1),
            "__gost_net_ws_recv_text does not take type arguments",
            Some("__gost_net_ws_recv_text expects 1 argument"),
            net_internal,
        )),
        "__gost_net_http_request" => Some(no_type_args(
            Arity::Exactly(4),
            "__gost_net_http_request does not take type arguments",
            Some("__gost_net_http_request expects 4 arguments"),
            net_internal,
        )),
        "__gost_net_http_request_headers" => Some(no_type_args(
            Arity::Exactly(5),
            "__gost_net_http_request does not take type arguments",
            Some("__gost_net_http_request_headers expects 5 arguments"),
            net_internal,
        )),
        "make_chan" => Some(sig(
            Arity::Exactly(1),
            Arity::Exactly(1),
            Some("make_chan expects one type argument"),
            Some("make_chan expects one argument"),
            None,
        )),
        "__gost_chan_can_send" => Some(no_type_args(
            Arity::Exactly(1),
            "__gost_chan_can_send does not take type arguments",
            Some("__gost_chan_can_send expects 1 argument"),
            None,
        )),
        "__gost_chan_can_recv" => Some(no_type_args(
            Arity::Exactly(1),
            "__gost_chan_can_recv does not take type arguments",
            Some("__gost_chan_can_recv expects 1 argument"),
            None,
        )),
        "__gost_select_wait" => Some(no_type_args(
            Arity::Any,
            "__gost_select_wait does not take type arguments",
            None,
            Some("__gost_select_wait is internal to the standard library"),
        )),
        "iter" | "iter_mut" => Some(no_type_args(
            Arity::Exactly(1),
            "iterator helpers do not take type arguments",
            Some("iter expects one argument"),
            None,
        )),
        "filter" => Some(no_type_args(
            Arity::Exactly(2),
            "iterator helpers do not take type arguments",
            Some("filter expects two arguments"),
            None,
        )),
        "map" => Some(no_type_args(
            Arity::Exactly(2),
            "iterator helpers do not take type arguments",
            Some("map expects two arguments"),
            None,
        )),
        "make_slice" => Some(sig(
            Arity::Exactly(1),
            Arity::Exactly(2),
            Some("make_slice expects one type argument"),
            Some("make_slice expects two arguments"),
            None,
        )),
        "slice_len" => Some(sig(
            Arity::Exactly(1),
            Arity::Exactly(1),
            Some("slice_len expects one type argument"),
            Some("slice_len expects one argument"),
            None,
        )),
        "slice_get_copy" => Some(sig(
            Arity::Exactly(1),
            Arity::Exactly(2),
            Some("slice_get_copy expects one type argument"),
            Some("slice_get_copy expects two arguments"),
            None,
        )),
        "slice_set" => Some(sig(
            Arity::Exactly(1),
            Arity::Exactly(3),
            Some("slice_set expects one type argument"),
            Some("slice_set expects three arguments"),
            None,
        )),
        "slice_ref" | "slice_mutref" => Some(sig(
            Arity::Exactly(1),
            Arity::Exactly(2),
            Some("slice_ref expects one type argument"),
            Some("slice_ref expects two arguments"),
            None,
        )),
        "slice_push" => Some(sig(
            Arity::Exactly(1),
            Arity::Exactly(2),
            Some("slice_push expects one type argument"),
            Some("slice_push expects two arguments"),
            None,
        )),
        "slice_pop" => Some(sig(
            Arity::Exactly(1),
            Arity::Exactly(1),
            Some("slice_pop expects one type argument"),
            Some("slice_pop expects one argument"),
            None,
        )),
        "shared_new" => Some(sig(
            Arity::Exactly(1),
            Arity::Exactly(1),
            Some("shared_new expects one type argument"),
            Some("shared_new expects one argument"),
            None,
        )),
        "own_new" => Some(sig(
            Arity::Exactly(1),
            Arity::Exactly(1),
            Some("own_new expects one type argument"),
            Some("own_new expects one argument"),
            None,
        )),
        "shared_get" | "shared_get_mut" => Some(sig(
            Arity::Exactly(1),
            Arity::Exactly(1),
            Some("shared_get expects one type argument"),
            Some("shared_get expects one argument"),
            None,
        )),
        "own_borrow" | "own_borrow_mut" | "alias_borrow" | "own_into_value" => Some(sig(
            Arity::Exactly(1),
            Arity::Exactly(1),
            Some("handle intrinsic expects one type argument"),
            Some("handle intrinsic expects one argument"),
            None,
        )),
        "freeze" => Some(sig(
            Arity::Exactly(1),
            Arity::Exactly(1),
            Some("freeze expects one type argument"),
            Some("freeze expects one argument"),
            None,
        )),
        "make_map" => Some(sig(
            Arity::Exactly(2),
            Arity::Exactly(1),
            Some("make_map expects two type arguments"),
            Some("make_map expects one argument"),
            None,
        )),
        "map_get" => Some(sig(
            Arity::Exactly(2),
            Arity::Exactly(2),
            Some("map_get expects two type arguments"),
            Some("map_get expects two arguments"),
            None,
        )),
        "map_set" => Some(sig(
            Arity::Exactly(2),
            Arity::Exactly(3),
            Some("map_set expects two type arguments"),
            Some("map_set expects three arguments"),
            None,
        )),
        "map_del" => Some(sig(
            Arity::Exactly(2),
            Arity::Exactly(2),
            Some("map_del expects two type arguments"),
            Some("map_del expects two arguments"),
            None,
        )),
        "map_len" => Some(sig(
            Arity::Exactly(2),
            Arity::Exactly(1),
            Some("map_len expects two type arguments"),
            Some("map_len expects one argument"),
            None,
        )),
        _ => None,
    }
}

pub fn is_intrinsic_name(name: &str) -> bool {
    intrinsic_signature(name).is_some()
}

pub fn intrinsic_type_args_error(name: &str, type_args_len: usize) -> Option<&'static str> {
    let sig = intrinsic_signature(name)?;
    if sig.type_args.matches(type_args_len) {
        None
    } else {
        sig.type_args_error
    }
}

pub fn intrinsic_args_error(name: &str, args_len: usize) -> Option<&'static str> {
    let sig = intrinsic_signature(name)?;
    if sig.args.matches(args_len) {
        None
    } else {
        sig.args_error
    }
}

pub fn intrinsic_internal_std_only_error(name: &str) -> Option<&'static str> {
    intrinsic_signature(name)?.internal_std_only_error
}
