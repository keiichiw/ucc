/*
1
*/
#include "test.h"

struct foo {
  int x;
};

struct bar {
  int y;
};

int main()
{
  struct foo *foo;
  int i;

  /* int -> ??? */
  i = (int)2;
  foo = (struct foo *)2;

  /* ptr -> ??? */
  i = (int)foo;
  foo = (struct foo *)foo;
  foo = (struct bar *)foo;

  print_int(1);

  return 0;
}
