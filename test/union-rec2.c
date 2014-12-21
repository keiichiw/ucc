/*
1
*/

#include "test.h"

union foo {
  union foo *p;
  int x;
};

struct baz {
  union bar {
    union bar *b;
    int x;
  } u;
  union bar *x;
  struct baz *y;
  int z;
};

int main()
{
  union foo f;
  struct baz b;

  print_int(1);

  return 0;
}

