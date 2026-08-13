global MOVAllBytesASM
global NOPAllBytesASM
global CMPAllBytesASM
global DECAllBytesASM
global NOP3x1AllBytesASM
global NOP1x3AllBytesASM
global NOP1x6AllBytesASM
global NOP1x9AllBytesASM
global NOP1x18AllBytesASM

section .text

MOVAllBytesASM:
    xor rax, rax
.loop:
    mov [rsi + rax], al
    inc rax
    cmp rax, rdi
    jb .loop
    ret

NOPAllBytesASM:
    xor rax, rax
.loop:
    db 0x0f, 0x1f, 0x00 ; NOP
    inc rax
    cmp rax, rdi
    jb .loop
    ret

CMPAllBytesASM:
    xor rax, rax
.loop:
    inc rax
    cmp rax, rdi
    jb .loop
    ret

DECAllBytesASM:
.loop:
    dec rdi
    jnz .loop
    ret

NOP3x1AllBytesASM:
    xor rax, rax
.loop:
    db 0x0f, 0x1f, 0x00; This is byte sequence for 3-byte NOP on x86-64
    inc rax
    cmp rax, rdi
    jb .loop
    ret

NOP1x3AllBytesASM:
    xor rax, rax
.loop:
    nop ; nop is a single byte NOP on x86-64, so three of them is 3 single byte NOP
    nop
    nop
    inc rax
    cmp rax, rdi
    jb .loop
    ret

NOP1x6AllBytesASM:
    xor rax, rax
.loop:
    nop
    nop
    nop
    nop
    nop
    nop
    inc rax
    cmp rax, rdi
    jb .loop
    ret

NOP1x9AllBytesASM:
    xor rax, rax
.loop:
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    inc rax
    cmp rax, rdi
    jb .loop
    ret

NOP1x18AllBytesASM:
    xor rax, rax
.loop:
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    inc rax
    cmp rax, rdi
    jb .loop
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
