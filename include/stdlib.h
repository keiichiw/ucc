#ifndef _STDLIB_H
#define _STDLIB_H

#include <stddef.h>

void _abort(void);
void *_malloc(size_t);
void _free(void *);
void *_realloc(void *, size_t);

#define abort _abort
#define malloc _malloc
#define free _free
#define realloc _realloc

#endif  /* stdlib.h */
