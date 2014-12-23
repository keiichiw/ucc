/*
-593279510
458299110
-1794094678
-661847888
516391518
-1917697722
-1695017917
717229868
137866584
395339113
*/
#include "test.h"

unsigned xor128(void) {
  static unsigned x = 123456789;
  static unsigned y = 362436069;
  static unsigned z = 521288629;
  static unsigned w = 88675123;
  unsigned t;

  t = x ^ (x << 11);
  x = y; y = z; z = w;
  return w = (w ^ (w >> 19)) ^ (t ^ (t >> 8));
}

int main () {
  static unsigned a = 12;
  int i;
  for (i=0; i<10;i++) {
    printf("%d\n", xor128());
  }
}
