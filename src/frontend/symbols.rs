pub const IMPL_MANGLE_PREFIX: &str = "__impl_";

fn stable_fnv1a64(text: &str) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in text.bytes() {
        hash ^= byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

fn normalize_ident_component(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len());
    for ch in raw.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch);
        } else {
            out.push('_');
        }
    }
    if out.is_empty() {
        "anon".to_string()
    } else {
        out
    }
}

pub fn module_symbol_key(package: &str, disambiguator: &str) -> String {
    if disambiguator.is_empty() {
        package.to_string()
    } else {
        format!("{}::{}", package, disambiguator)
    }
}

pub fn mangle_impl_method(module_key: &str, recv_name: &str, method_name: &str) -> String {
    let module_hash = stable_fnv1a64(module_key);
    let recv_norm = normalize_ident_component(recv_name);
    format!(
        "{}{:016x}__{}__{}",
        IMPL_MANGLE_PREFIX, module_hash, recv_norm, method_name
    )
}

pub fn is_impl_mangled(symbol: &str) -> bool {
    symbol.starts_with(IMPL_MANGLE_PREFIX)
}

pub fn logical_method_name(symbol: &str) -> &str {
    if !is_impl_mangled(symbol) {
        return symbol;
    }
    let rest = &symbol[IMPL_MANGLE_PREFIX.len()..];
    let mut parts = rest.splitn(3, "__");
    let _ = parts.next();
    let _ = parts.next();
    let method = parts.next().unwrap_or(symbol);
    method.split('$').next().unwrap_or(method)
}
