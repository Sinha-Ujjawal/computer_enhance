global Mov8_32bytes    ; Using AVX SIMD
global Mov8_32bytes_v2 ; Using AVX SIMD. This supports non-power of two as well

; ymm0-15 are AVX registers
; This Assembly Routine is written for Linux x86-64 following the System-V ABI
; rdi: count (must be divisible by 256)
; rsi: data pointer
; rdx: mask
Mov8_32bytes:
    xor r9, r9
    mov rax, rsi
    align 64
.loop:
    ; First 128 bytes
    vmovdqu ymm0, [rax]
    vmovdqu ymm1, [rax + 32]
    vmovdqu ymm2, [rax + 64]
    vmovdqu ymm3, [rax + 96]

    ; Next 128 bytes
    vmovdqu ymm4, [rax + 128]
    vmovdqu ymm5, [rax + 160]
    vmovdqu ymm6, [rax + 192]
    vmovdqu ymm7, [rax + 224]

    add r9, 256
    and r9, rdx
    mov rax, r9
    add rax, rsi

    sub rdi, 256
    jnz .loop

    ret

; ymm0-15 are AVX registers
; This Assembly Routine is written for Linux x86-64 following the System-V ABI
; rdi: outer loop count
; rsi: inner loop count
; rdx: data pointer
Mov8_32bytes_v2:
    align 64
.outer:
    mov rax, rdx
    mov r9, rsi
    .inner:
        ; First 128 bytes
        vmovdqu ymm0, [rax]
        vmovdqu ymm1, [rax + 32]
        vmovdqu ymm2, [rax + 64]
        vmovdqu ymm3, [rax + 96]

        ; Next 128 bytes
        vmovdqu ymm4, [rax + 128]
        vmovdqu ymm5, [rax + 160]
        vmovdqu ymm6, [rax + 192]
        vmovdqu ymm7, [rax + 224]

        add rax, 256

        sub r9, 1
        jnz .inner
    sub rdi, 1
    jnz .outer

section .note.GNU-stack noalloc noexec nowrite progbits
