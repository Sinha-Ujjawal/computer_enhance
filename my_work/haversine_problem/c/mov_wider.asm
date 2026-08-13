global Mov2_4bytes
global Mov2_8bytes
global Mov2_16bytes ; Using SIMD
global Mov2_32bytes ; Using SIMD
; global Mov2_64bytes ; Using SIMD; NOTE: I am poor so I cannot test for 2 x 64 bytes, as that requures AVX-512 support.

Mov2_4bytes:
    xor rax, rax
    align 64
.loop:
    mov r8d, [rsi]
    mov r8d, [rsi + 4]
    add rax, 8
    cmp rax, rdi
    jb .loop
    ret

Mov2_8bytes:
    xor rax, rax
    align 64
.loop:
    mov r8, [rsi]
    mov r8, [rsi + 8]
    add rax, 16
    cmp rax, rdi
    jb .loop
    ret

; xmm0-15 are SSE registers
Mov2_16bytes:
    xor rax, rax
    align 64
.loop:
    vmovdqu xmm0, [rsi]
    vmovdqu xmm0, [rsi + 16]
    add rax, 32
    cmp rax, rdi
    jb .loop
    ret

; ymm0-15 are AVX registers
Mov2_32bytes:
    xor rax, rax
    align 64
.loop:
    vmovdqu ymm0, [rsi]
    vmovdqu ymm0, [rsi + 32]
    add rax, 64
    cmp rax, rdi
    jb .loop
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
