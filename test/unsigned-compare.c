/*
1
1
0
0
0
0
0
1
1
0
0
1
0
1
1
*/

#include "test.h"

int main () {
  int a = -1;
  int b = 2;
  print_int( a <  b);
  print_int( a <= b);
  print_int( a >  b);
  print_int( a >= b);
  print_int( a == b);

  print_int((unsigned)a <  (unsigned)b);
  print_int((unsigned)a <= (unsigned)b);
  print_int((unsigned)a >  (unsigned)b);
  print_int((unsigned)a >= (unsigned)b);
  print_int((unsigned)a == (unsigned)b);

  print_int((unsigned)a <  (unsigned)a);
  print_int((unsigned)a <= (unsigned)a);
  print_int((unsigned)a >  (unsigned)a);
  print_int((unsigned)a >= (unsigned)a);
  print_int((unsigned)a == (unsigned)a);

}
