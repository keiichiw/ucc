/*
0
1
1
1
0
*/
#include "test.h"

int main () {
  unsigned a = 0, b=1, c=2, d=3;
  print_int(b && c && a && d);
  print_int(b && c && d);

  print_int(b || c || a);
  print_int(a || c || b);
  print_int(a || a || a);

}
