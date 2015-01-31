typedef int jmp_buf[4];
int setjmp (jmp_buf );
void longjmp (jmp_buf , int );
