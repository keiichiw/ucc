/*
7
*/

#include "test.h"

int add(int a, int b)
{
  return a + b;
}

int main()
{
  int add(int, int);

  printf("%d\n", add(3,4));
}
