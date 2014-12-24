/*
12345679
0
0
-1406078410
1
0
12345679
0
*/

#include "test.h"

int main()
{
  unsigned x = 2888888886, y = x;
  print_int(x / 234);
  print_int(x % 234);
  print_int(x / -1);
  print_int(x % -1);
  print_int(x / y);
  print_int(x % y);
  x /= 234;
  y %= 234;
  print_int(x);
  print_int(y);
}
