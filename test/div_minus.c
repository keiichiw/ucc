/*
7
-7
-7
7
*/

#ifndef __UCC__
#include <stdio.h>
int print_int(int n){
  printf("%d\n", n);
}
#endif

int main () {
  int i, r;
  r = 23/3;
  print_int(r);
  r = (-23)/3;
  print_int(r);
  r = 23/(-3);
  print_int(r);
  r = (-23)/(-3);
  print_int(r);

  return 0;
}
