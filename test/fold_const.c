/*
1
2
3
4
5
6
*/
#include "test.h"

void fun (int n) {
  switch (n) {
  case 1 + 2:
    print_int(1);
    break;
  case 3 - 4:
    print_int(2);
    break;
  case 4 == 4:
    print_int(3);
    break;
  case 3 < 1:
    print_int(4);
    break;
  case 6 << 1:
    print_int(5);
    break;
  case 8 >> 2:
    print_int(6);
    break;
  }
}

int
main()
{
  fun(3);
  fun(-1);
  fun(1);
  fun(0);
  fun(12);
  fun(2);

  return 0;
}
