/*
0
0
1
1
0
1
0
0
1
1
0
*/

#include "test.h"

int main () {
  int a;
  {
    int b;
    {
      int *p=&a, *q=&b;
      print_int(p <  q);
      print_int(p <= q);
      print_int(p >  q);
      print_int(p >= q);
      print_int(p == q);
      q = &a;
      print_int(p == q);
      print_int(p <  0);
      print_int(p <= 0);
      print_int(p >  0);
      print_int(p >= 0);
      print_int(p == 0);

    }
  }
}
