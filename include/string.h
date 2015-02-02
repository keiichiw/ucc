#ifndef _STRING_H
#define _STRING_H

#include <stddef.h>

void *_memcpy(void *, const void *, size_t);

#define memcpy _memcpy

#endif  /* string.h */
