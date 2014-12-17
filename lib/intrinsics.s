.global asm_write
asm_write:
        mov r1, [rbp+4]
        write r1
        ret
