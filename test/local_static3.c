/*
0
1
2
*/

#include "test.h"

int foo()
{
  static int x;                 /* no init */

  return x++;
}

int main()
{
  print_int(foo());
  print_int(foo());
  print_int(foo());
}
