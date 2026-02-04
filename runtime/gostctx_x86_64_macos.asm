.text
.globl _gost_ctx_swap
_gost_ctx_swap:
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

.globl _gost_ctx_start
_gost_ctx_start:
    movq %r12, %rdi
    call _gost_ctx_entry
    ud2
