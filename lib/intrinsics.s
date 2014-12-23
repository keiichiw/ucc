.global __gaia_write
__gaia_write:
    mov r1, [rbp + 4]
    write r1
    ret
.global __gaia_read
__gaia_read:
    read r1
    ret
