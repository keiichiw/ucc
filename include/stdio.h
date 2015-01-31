#ifndef _STDIO_H
#define _STDIO_H

int _putchar(int);
int _getchar(void);
int _printf(const char*, ...);

#define putchar(c) _putchar(c)
#define getchar() _getchar()
#define printf _printf

#endif  /* stdio.h */
