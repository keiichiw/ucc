.global asm_write
asm_write:
        mov $1, [$bp+2]
        write $1
        ret
