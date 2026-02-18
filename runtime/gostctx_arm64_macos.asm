// runtime/gostctx_arm64_macos.asm

.text
.globl _gost_ctx_swap
_gost_ctx_swap:
    mov x2, sp
    str x2, [x0, #0]
    adr x2, 1f
    str x2, [x0, #8]
    str x19, [x0, #16]
    str x20, [x0, #24]
    str x21, [x0, #32]
    str x22, [x0, #40]
    str x23, [x0, #48]
    str x24, [x0, #56]
    str x25, [x0, #64]
    str x26, [x0, #72]
    str x27, [x0, #80]
    str x28, [x0, #88]
    str x29, [x0, #96]
    str x30, [x0, #104]

    ldr x19, [x1, #16]
    ldr x20, [x1, #24]
    ldr x21, [x1, #32]
    ldr x22, [x1, #40]
    ldr x23, [x1, #48]
    ldr x24, [x1, #56]
    ldr x25, [x1, #64]
    ldr x26, [x1, #72]
    ldr x27, [x1, #80]
    ldr x28, [x1, #88]
    ldr x29, [x1, #96]
    ldr x30, [x1, #104]
    ldr x2, [x1, #0]
    mov sp, x2
    ldr x2, [x1, #8]
    br x2
1:
    ret

.globl _gost_ctx_start
_gost_ctx_start:
    mov x0, x19
    bl _gost_ctx_entry
    brk #0
