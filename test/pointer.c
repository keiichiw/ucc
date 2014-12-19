/*
1
2
3
*/
#include "ucc.h"

int main () {
  int a, *b, **c, **d;
  b = &a;
  c = &b;
  **c = 1;
  print_int(a);   // 1
  d = c;
  a = 2;
  print_int(**d); // 2
  **(&(*d)) = 3;
  print_int(a);   // 3
  return 0;
}
