/*
5
*/
#include "ucc.h"

int print_int(int i);

int foo(int a, int b);

int foo(int a, int b)
{
  return a + b;
}

int main()
{
  print_int(foo(2,3));
}
