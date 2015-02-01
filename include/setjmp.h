#ifndef _SETJMP_H
#define _SETJMP_H

typedef int jmp_buf[4];
int _setjmp(jmp_buf);
void _longjmp(jmp_buf, int);

#define setjmp(env) _setjmp(env)
#define longjmp _longjmp

#endif  /* setjmp.h */
