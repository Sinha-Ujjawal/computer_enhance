;; Register Renaming Exercises


;;; Exercise 1
    mov rax, 1
    mov rbx, 2
    mov rcx, 3
    mov rdx, 4
    add rax, rbx
    add rcx, rdx
    add rax, rcx
    mov rcx, rbx
    inc rax
    dec rcx
    sub rax, rbx
    sub rcx, rdx
    sub rax, rcx
    ; Translates to after register renaming
    mov T1, 1         ; rax => T1
    mov T2, 2         ; rax => T1 , rbx => T2
    mov T3, 3         ; rax => T1 , rbx => T2, rcx => T3
    mov T4, 4         ; rax => T1 , rbx => T2, rcx => T3 , rdx => T4
    add T5, T1, T2    ; rax => T5 , rbx => T2, rcx => T3 , rdx => T4
    add T6, T3, T4    ; rax => T5 , rbx => T2, rcx => T6 , rdx => T4
    add T7, T5, T6    ; rax => T7 , rbx => T2, rcx => T6 , rdx => T4
    mov T8, T6, T2    ; rax => T7 , rbx => T2, rcx => T8 , rdx => T4
    inc T9, T7        ; rax => T9 , rbx => T2, rcx => T8 , rdx => T4
    dec T10, T8       ; rax => T9 , rbx => T2, rcx => T10, rdx => T4
    sub T11, T9, T2   ; rax => T11, rbx => T2, rcx => T10, rdx => T4
    sub T12, r10, T4  ; rax => T11, rbx => T2, rcx => T12, rdx => T4
    sub T13, T11, T12 ; rax => T13, rbx => T2, rcx => T12, rdx => T4


;;; Exercise 2
    top:
        pop rcx
        sub rsp, rdx
        mov rbx, rax
        shl rbx, 0
        not rbx
        loopne top
    ; Translates to after register renaming
    top:
        ; pop rcx
          mov T2, [T1]    ; rsp => T1, rcx => T2
          add T3, T1, 8   ; rsp => T3, rcx => T2, rdx => T4
        sub T5, T3, T4    ; rsp => T5, rcx => T2, rdx => T4, rax => T6, rbx => T7
        mov T8, T7, T6    ; rsp => T5, rcx => T2, rdx => T4, rax => T6, rbx => T8
        shl T9, T8, 0     ; rsp => T5, rcx => T2, rdx => T4, rax => T6, rbx => T9
        not T10, T9       ; rsp => T5, rcx => T2, rdx => T4, rax => T6, rbx => T10
        loopne top
