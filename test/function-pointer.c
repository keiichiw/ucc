/*
5
6
5
6
*/
#include "test.h"

int (*f)(int a, int b);

int add(int a, int b)
{
  return a + b;
}

int mul(int a, int b)
{
  return a * b;
}

int main()
{
  int (*g)(int a, int b);

  f = add;

  print_int(f(2,3));

  f = mul;

  print_int(f(2,3));

  g = add;

  print_int(g(2,3));

  g = mul;

  print_int(g(2,3));

  return 0;
}
