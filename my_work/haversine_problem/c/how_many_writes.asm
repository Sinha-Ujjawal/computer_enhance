global Mov1
global Mov2
global Mov3
global Mov4
global Mov5

section .text

Mov1:
align 64
    xor rax, rax
.loop:
    mov [rsi], rcx
    sub rdi, 1
    jnle .loop
    ret

Mov2:
align 64
    xor rax, rax
.loop:
    mov [rsi], rcx
    mov [rsi], rcx
    sub rdi, 2
    jnle .loop
    ret

Mov3:
align 64
    xor rax, rax
.loop:
    mov [rsi], rcx
    mov [rsi], rcx
    mov [rsi], rcx
    sub rdi, 3
    jnle .loop
    ret

Mov4:
align 64
    xor rax, rax
.loop:
    mov [rsi], rcx
    mov [rsi], rcx
    mov [rsi], rcx
    mov [rsi], rcx
    sub rdi, 4
    jnle .loop
    ret

Mov5:
align 64
    xor rax, rax
.loop:
    mov [rsi], rcx
    mov [rsi], rcx
    mov [rsi], rcx
    mov [rsi], rcx
    mov [rsi], rcx
    sub rdi, 5
    jnle .loop
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
