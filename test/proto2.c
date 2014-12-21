/*
5
6
*/

#include "test.h"

int calc(int (*)(int, int), int, int);

int add(int a, int b) {
  return a + b;
}

int mul(int a, int b) {
  return a * b;
}

int main()
{
  print_int(calc(add, 2, 3));
  print_int(calc(mul, 2, 3));
}

int calc(int (*f)(int, int), int a, int b) {
  return f(a,b);
}
