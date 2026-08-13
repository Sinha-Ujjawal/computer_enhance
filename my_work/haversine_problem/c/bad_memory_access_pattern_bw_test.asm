global Mov2_32bytes_Strided ; Using AVX SIMD

; ymm0-15 are AVX registers
; This Assembly Routine is written for Linux x86-64 following the System-V ABI
; rdi: outer loop count
; rsi: inner loop count
; rdx: data pointer
; rcx: stride
Mov2_32bytes_Strided:
    align 64
.outer:
    mov rax, rdx
    mov r9, rsi
    .inner:
        ; One Cache Line
        vmovdqu ymm0, [rax]
        vmovdqu ymm1, [rax + 32]

        add rax, rcx

        sub r9, 1
        jnz .inner
    sub rdi, 1
    jnz .outer

section .note.GNU-stack noalloc noexec nowrite progbits
