/*
*/
#include "test.h"

#ifndef __UCC__
#include <stdio.h>
int print_int(int n){
  printf("%d\n", n);
}
#endif

int main () {
  int a[2][2];
  a[3][3];
  return 0;
}
