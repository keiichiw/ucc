/*
1
2
3
*/
#include "test.h"

int main () {
  long a[3] = {1,2,3};
  int *p;
  p = (int*)(void*)a;
  print_int(p[0]);
  print_int(p[1]);
  print_int(p[2]);
}
