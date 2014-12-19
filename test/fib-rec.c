/*
832040
*/
#include "test.h"

int fib (int x) {
  if (x <= 1) {
    return x;
  } else {
    int a, b;
    a = fib(x-1);
    b = fib(x-2);
    return a+b;
  }
}

int main () {
  print_int(fib(30));
  return 0;
}
