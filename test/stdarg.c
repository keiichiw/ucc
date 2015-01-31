/*
0
10
6
*/
#include <stdarg.h>
#include "test.h"

#ifndef __UCC__
int sum (int count, ...) {
#else
int sum (int count) {
#endif
  int s = 0;
  va_list ap;
  va_start(ap, count);
  while (count-- > 0) {
    s += va_arg(ap, int);
  }
  va_end(ap);
  return s;
}

int main () {
  printf("%d\n", sum(0));
  printf("%d\n", sum(1, 10));
  printf("%d\n", sum(3, 1, 2, 3));
}
