/*
32
-2147483648
2147483647
-2147483648
2147483647
*/

#include <limits.h>
#include "test.h"
int main () {
  printf("%d\n", CHAR_BIT);
  printf("%d\n",  INT_MIN);
  printf("%d\n",  INT_MAX);
  printf("%d\n", LONG_MIN);
  printf("%d\n", LONG_MAX);
  return 0;
}
