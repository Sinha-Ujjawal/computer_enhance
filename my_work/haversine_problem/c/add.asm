global IncRCXTwice
global IncRCXTwiceWithRAXMov
global IncRCXOnceWithRAXMov

section .text

IncRCXTwice:
align 64
    mov rax, 1000000000
.loop:
    add rcx, 1
    add rcx, 1
    dec rax
    jnz .loop
    ret

IncRCXTwiceWithRAXMov:
align 64
    mov rax, 1000000000
.loop:
    mov rcx, rax
    add rcx, 1
    mov rcx, rax
    add rcx, 1
    dec rax
    jnz .loop
    ret

IncRCXOnceWithRAXMov:
align 64
    mov rax, 1000000000
.loop:
    mov rcx, rax
    add rcx, 1
    dec rax
    jnz .loop
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
