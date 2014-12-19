/*
1
2
3
*/
#include "test.h"

int main()
{
  int *a[3];
  int x = 1;
  int y = 2;
  int z = 3;

  a[0] = &x;
  a[1] = &y;
  a[2] = &z;

  print_int(*a[0]);
  print_int(*a[1]);
  print_int(*a[2]);
}
