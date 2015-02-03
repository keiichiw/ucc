/*
0
1
1
*/

#include <stdio.h>

typedef unsigned long ulong;
typedef unsigned int short ushort;
typedef char unsigned uchar;

int main () {
  ulong  a;
  ushort b;
  uchar  c;
  a = -1; b = 1; c = 3;
  printf("%d\n", a<b);
  printf("%d\n", b<c);
  printf("%d\n", c<a);
}
