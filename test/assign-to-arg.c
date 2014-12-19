/*
2
1
2
*/
#include "test.h"

int fun (int x) {
  print_int(x);
  x -= 1;
  print_int(x);
  return 0;
}

int main () {
  int a=2;
  fun(a);
  print_int(a);
}
