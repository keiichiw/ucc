/*
4095
42949672
0
330
2
1
0
1
3
*/

#include <stdio.h>

typedef struct { int a[4]; } foo;

int main()
{
  static int a[] = {
    (unsigned)(-1) >> 20,
    (unsigned)(-1) / 100,
    (unsigned)(-1) < 1,
    (int)((foo *)10 + 20),
    (int)((foo *)64 - (foo *)32),
    (int)1.23,
    12 && 0,
    12 || 34,
    1 + 2 ? 3 : 4
  };

  int i;
  for (i = 0; i < sizeof a / sizeof(int); ++i) {
    printf("%d\n", a[i]);
  }
}
