/*
1
*/

#include "test.h"

union foo {
  union foo *p;
  int x;
};

int main()
{
  union foo f;

  print_int(1);

  return 0;
}

