/*
0
1
42
*/

#include "test.h"

int say_one()
{
  print_int(1);
  return 1;
}

int main()
{
  int a[3] = {0};

  print_int(a[1]);

  a[say_one()] += 42;

  print_int(a[1]);

  return 0;
}
