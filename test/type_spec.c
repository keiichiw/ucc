/*
0
1
0
*/
#include <stdio.h>

typedef unsigned long ulong;
int main () {
  unsigned int a=0;
  unsigned long int b = -1;
  ulong c = 2;
  printf("%d\n", a);
  printf("%d\n", b>c);
  printf("%d\n", b<c);
  return 0;
}
