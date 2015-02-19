#ifndef _UCC_ASSERT_H
#define _UCC_ASSERT_H

#ifdef NDEBUG

#define assert(expr) ((void) 0)

#else

void __assert_fail(const char *, const char *, int);
#define assert(expr) \
  ((expr) ? (void) 0 : __assert_fail(#expr, __FILE__, __LINE__))

#endif

#endif  /* assert.h */
