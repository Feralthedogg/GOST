# runtime/gostctx_x86_64_sysv.asm

.text
.globl gost_ctx_swap
.type gost_ctx_swap,@function
# SysV: from=rdi, to=rsi
gost_ctx_swap:
    # save callee-saved into *from
    movq %rsp, 0(%rdi)
    leaq 1f(%rip), %rax
    movq %rax, 8(%rdi)
    movq %rbx, 16(%rdi)
    movq %rbp, 24(%rdi)
    movq %r12, 32(%rdi)
    movq %r13, 40(%rdi)
    movq %r14, 48(%rdi)
    movq %r15, 56(%rdi)

    # restore callee-saved from *to
    movq 16(%rsi), %rbx
    movq 24(%rsi), %rbp
    movq 32(%rsi), %r12
    movq 40(%rsi), %r13
    movq 48(%rsi), %r14
    movq 56(%rsi), %r15
    movq 0(%rsi), %rsp
    movq 8(%rsi), %rax
    jmp *%rax
1:
    ret
    .size gost_ctx_swap,.-gost_ctx_swap

.globl gost_ctx_start
.type gost_ctx_start,@function
# entry: r12 contains g pointer
gost_ctx_start:
    movq %r12, %rdi
    call gost_ctx_entry@PLT
    ud2
    .size gost_ctx_start,.-gost_ctx_start
