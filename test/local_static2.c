/*
0
1
2
0
1
2
*/

#include "test.h"

int foo()
{
  static int a = 0;

  return a++;
}

int bar()
{
  static int a = 0;

  return a++;
}

int main()
{
  print_int(foo());
  print_int(foo());
  print_int(foo());
  print_int(bar());
  print_int(bar());
  print_int(bar());
}
