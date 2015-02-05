#ifndef _ASSERT_H
#define _ASSERT_H

#ifdef NDEBUG

#define assert(expr) ((void) 0)

#else

void _assert_fail(const char *, const char *, int);
#define assert(expr) \
  ((expr) ? (void) 0 : _assert_fail(#expr, __FILE__, __LINE__))

#endif

#endif  /* assert.h */
