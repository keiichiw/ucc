/*
1
1
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

  ++a[say_one()];

  print_int(a[1]);
}
