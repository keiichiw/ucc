#ifndef _UCC_STDLIB_H
#define _UCC_STDLIB_H

#include <stddef.h>

#define RAND_MAX 2147483647

void abort(void);
int abs(int);
long strtol(const char *, char **, int);
int rand(void);
void srand(unsigned);
void *malloc(size_t);
void *realloc(void *, size_t);
void free(void *);

#endif  /* stdlib.h */
