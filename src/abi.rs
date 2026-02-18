pub const EXTERN_ABI_C: &str = "c";
pub const EXTERN_ABI_SYSTEM: &str = "system";
pub const EXTERN_ABI_STDCALL: &str = "stdcall";
pub const EXTERN_ABI_FASTCALL: &str = "fastcall";
pub const EXTERN_ABI_VECTORCALL: &str = "vectorcall";
pub const EXTERN_ABI_THISCALL: &str = "thiscall";
pub const EXTERN_ABI_WIN64: &str = "win64";
pub const EXTERN_ABI_SYSV64: &str = "sysv64";
pub const EXTERN_ABI_AAPCS: &str = "aapcs";

pub const SUPPORTED_EXTERN_ABIS: &[&str] = &[
    EXTERN_ABI_C,
    EXTERN_ABI_SYSTEM,
    EXTERN_ABI_STDCALL,
    EXTERN_ABI_FASTCALL,
    EXTERN_ABI_VECTORCALL,
    EXTERN_ABI_THISCALL,
    EXTERN_ABI_WIN64,
    EXTERN_ABI_SYSV64,
    EXTERN_ABI_AAPCS,
];

pub fn canonicalize_extern_abi(abi: &str) -> String {
    if abi.is_empty() {
        EXTERN_ABI_C.to_string()
    } else {
        abi.to_ascii_lowercase()
    }
}

pub fn is_supported_extern_abi(abi: &str) -> bool {
    let canonical = canonicalize_extern_abi(abi);
    SUPPORTED_EXTERN_ABIS
        .iter()
        .any(|candidate| *candidate == canonical)
}

pub const fn supported_extern_abis_display() -> &'static str {
    "\"C\", \"system\", \"stdcall\", \"fastcall\", \"vectorcall\", \"thiscall\", \"win64\", \"sysv64\", \"aapcs\""
}

pub fn llvm_call_conv(abi: Option<&str>) -> &'static str {
    let Some(raw) = abi else {
        return "";
    };
    let canonical = canonicalize_extern_abi(raw);
    match canonical.as_str() {
        EXTERN_ABI_C | EXTERN_ABI_SYSTEM => "",
        EXTERN_ABI_STDCALL => "x86_stdcallcc ",
        EXTERN_ABI_FASTCALL => "x86_fastcallcc ",
        EXTERN_ABI_VECTORCALL => "x86_vectorcallcc ",
        EXTERN_ABI_THISCALL => "x86_thiscallcc ",
        EXTERN_ABI_WIN64 => "win64cc ",
        EXTERN_ABI_SYSV64 => "x86_64_sysvcc ",
        EXTERN_ABI_AAPCS => "arm_aapcscc ",
        _ => "",
    }
}

