/*
50
1
4
9
16
1
0
*/

#include "test.h"
int x = (1+2+3+4)*5;
int a[4] = {1*1, 2*2, 3*3, 4*4};
int main () {
  int b[2] = {1==1, 1==0};
  print_int(x);
  print_int(a[0]);
  print_int(a[1]);
  print_int(a[2]);
  print_int(a[3]);
  print_int(b[0]);
  print_int(b[1]);
}
