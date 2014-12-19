/*
1
2
3
4
5
6
*/
#include "ucc.h"

int foo()
{
  goto L1;
 L1:
  print_int(2);
  return 0;
}

int main()
{
  int a = 0;

  goto L1;
 L1:
  print_int(1);
  foo();
  print_int(3);
 L2:
  if (a == 0) {
    print_int(4);
    a = 1;
    goto L2;
  } else {
    print_int(5);
    goto L3;
  }
  print_int(0);
 L3:
  print_int(6);
  return 0;
}
