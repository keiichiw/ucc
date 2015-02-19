#ifndef _UCC_STDIO_H
#define _UCC_STDIO_H

#include <stdarg.h>
#include <stddef.h>

#define EOF (-1)

typedef struct {
  int (*read)(void);
  int (*write)(int);
} FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

int putchar(int);
int getchar(void);
int putc(int, FILE *);
int fputc(int, FILE *);
int getc(FILE *);
int fgetc(FILE *);

int printf(const char *, ...);
int fprintf(FILE *, const char *, ...);
int sprintf(char *, const char *, ...);
int vprintf(const char *, va_list);
int vfprintf(FILE *, const char *, va_list);
int vsprintf(char *, const char *, va_list);

#endif  /* stdio.h */
