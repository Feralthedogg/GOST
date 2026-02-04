.text
.globl gost_ctx_swap
# Win64: from=rcx, to=rdx
gost_ctx_swap:
    # save into *from (rcx)
    movq %rsp, 0(%rcx)
    leaq 1f(%rip), %rax
    movq %rax, 8(%rcx)
    movq %rbx, 16(%rcx)
    movq %rbp, 24(%rcx)
    movq %rdi, 32(%rcx)
    movq %rsi, 40(%rcx)
    movq %r12, 48(%rcx)
    movq %r13, 56(%rcx)
    movq %r14, 64(%rcx)
    movq %r15, 72(%rcx)
    movdqu %xmm6, 80(%rcx)
    movdqu %xmm7, 96(%rcx)
    movdqu %xmm8, 112(%rcx)
    movdqu %xmm9, 128(%rcx)
    movdqu %xmm10, 144(%rcx)
    movdqu %xmm11, 160(%rcx)
    movdqu %xmm12, 176(%rcx)
    movdqu %xmm13, 192(%rcx)
    movdqu %xmm14, 208(%rcx)
    movdqu %xmm15, 224(%rcx)

    # restore from *to (rdx)
    movq 16(%rdx), %rbx
    movq 24(%rdx), %rbp
    movq 32(%rdx), %rdi
    movq 40(%rdx), %rsi
    movq 48(%rdx), %r12
    movq 56(%rdx), %r13
    movq 64(%rdx), %r14
    movq 72(%rdx), %r15
    movdqu 80(%rdx), %xmm6
    movdqu 96(%rdx), %xmm7
    movdqu 112(%rdx), %xmm8
    movdqu 128(%rdx), %xmm9
    movdqu 144(%rdx), %xmm10
    movdqu 160(%rdx), %xmm11
    movdqu 176(%rdx), %xmm12
    movdqu 192(%rdx), %xmm13
    movdqu 208(%rdx), %xmm14
    movdqu 224(%rdx), %xmm15
    movq 0(%rdx), %rsp
    movq 8(%rdx), %rax
    jmp *%rax
1:
    ret

.globl gost_ctx_start
# entry: r12 contains g pointer
gost_ctx_start:
    subq $40, %rsp
    movq %r12, %rcx
    call gost_ctx_entry
    ud2
