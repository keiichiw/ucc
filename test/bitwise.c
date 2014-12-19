/*
4
15
-14
11
*/
#include "ucc.h"

int main () {
  print_int(13 & 6); // 4
  print_int(13 | 6); // 15
  print_int(~13);    // -14
  print_int(13 ^ 6); // 11
  return 0;
}
