/*
1
*/

#include "test.h"

typedef struct {
  int fst, snd;
} pair;

int main()
{
  pair P[100] = {0};
  pair *p = P + 99;
  int i, ok = 1;

  for (i = 0; i < 25; ++i) {
    p->snd = 42;
    p -= 4;
  }

  for (i = 0; i < 100; ++i) {
    if (P[i].fst != 0)
      ok = 0;
    if (i % 4 == 3) {
      if (P[i].snd != 42)
        ok = 0;
    } else {
      if (P[i].snd != 0)
        ok = 0;
    }
  }

  print_int(ok);
}
