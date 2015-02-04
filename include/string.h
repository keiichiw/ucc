#ifndef _STRING_H
#define _STRING_H

#include <stddef.h>

size_t _strlen(const char *);
void *_memcpy(void *, const void *, size_t);

#define strlen _strlen
#define memcpy _memcpy

#endif  /* string.h */
