/*
1
*/

#include "test.h"

typedef struct {
  int fst, snd;
} pair;

int main()
{
  pair P[100];
  pair *p = P;
  int i;
  int ok = 1;

  for (i = 0; i < 100; ++i) {
    P[i].fst = i;
    P[i].snd = i;
  }

  for (i = 0; i < 100; ++i) {
    (*p).fst += i + 42;
    p += 1;
  }

  for (i = 0; i < 100; ++i) {
    if (! (P[i].fst == i * 2 + 42 && P[i].snd == i))
      ok = 0;
  }

  print_int(ok);

  return 0;
}
