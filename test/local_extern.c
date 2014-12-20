/*
3
*/

#include "test.h"

int main()
{
  extern int add(int a, int b);

  print_int(add(1, 2));
}

int add(int a, int b)
{
  return a + b;
}
