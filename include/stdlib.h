#ifndef _STDLIB_H
#define _STDLIB_H

#include <stddef.h>

#define RAND_MAX 2147483647

void _abort(void);
void *_malloc(size_t);
void _free(void *);
void *_realloc(void *, size_t);
int _rand(void);
void _srand(unsigned);

#define abort _abort
#define malloc _malloc
#define free _free
#define realloc _realloc
#define rand _rand
#define srand _srand

#endif  /* stdlib.h */
