/*
8
127
-128
255
127
-128
32767
-32768
65535
2147483647
-2147483648
4294967295
2147483647
-2147483648
4294967295
*/

#include <limits.h>
#include "test.h"
int main () {
  printf("%d\n", CHAR_BIT);
  printf("%d\n", SCHAR_MAX);
  printf("%d\n", SCHAR_MIN);
  printf("%d\n", UCHAR_MAX);
  printf("%d\n", CHAR_MAX);
  printf("%d\n", CHAR_MIN);
  printf("%d\n", SHRT_MAX);
  printf("%d\n", SHRT_MIN);
  printf("%d\n", USHRT_MAX);
  printf("%d\n",  INT_MAX);
  printf("%d\n",  INT_MIN);
  printf("%u\n",  UINT_MAX);
  printf("%ld\n", LONG_MAX);
  printf("%ld\n", LONG_MIN);
  printf("%lu\n", ULONG_MAX);
  return 0;
}
