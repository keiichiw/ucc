/*
7
-7
-7
7
*/
#include "test.h"

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
