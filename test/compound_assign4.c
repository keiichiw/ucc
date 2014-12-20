/*
4
9
6
24
12
4
15
24
168
56
0
*/

#include "test.h"

int main()
{
  int a = 4;

  print_int(a); a += 5;
  print_int(a); a -= 3;
  print_int(a); a <<= 2;
  print_int(a); a >>= 1;
  print_int(a); a &= 7;
  print_int(a); a |= 15;
  print_int(a); a ^= 23;
  print_int(a); a *= 7;
  print_int(a); a /= 3;
  print_int(a); a %= 4;
  print_int(a);
}
