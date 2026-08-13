global ConditionalNOP

section .text

ConditionalNOP:
    xor rax, rax
.loop:
    mov r10, [rsi + rax]
    inc rax
    test r10, 1 ; 1 is when nop branch is taken, and 0 is not taken
    jnz .skip
    nop
.skip:
    cmp rax, rdi
    jb .loop
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
