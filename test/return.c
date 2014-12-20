/*
2
3
5
*/

#include "test.h"

void foo()
{
  print_int(3);
  return;
  print_int(4);
}

int main()
{
  print_int(2);
  foo();
  print_int(5);
  return 0;
}


