global Mov2_1byte
global Mov2_8bytes
global MovZX2_1byte

Mov2_1byte:
align 64
    xor rax, rax
.loop:
    mov cl, [rsi]
    mov cl, [rsi]
    sub rdi, 2
    jnle .loop
    ret

Mov2_8bytes:
align 64
    xor rax, rax
.loop:
    mov rcx, [rsi]
    mov rcx, [rsi]
    sub rdi, 2
    jnle .loop
    ret

MovZX2_1byte:
align 64
    xor rax, rax
.loop:
    movzx rcx, byte [rsi]
    movzx rcx, byte [rsi]
    sub rdi, 2
    jnle .loop
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
