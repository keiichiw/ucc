/*
0
1
2
3
2
1
*/
#include "ucc.h"
int main () {
  int i=0;
  print_int(i++);
  print_int(i++);
  print_int(i++);
  print_int(i--);
  print_int(i--);
  print_int(i--);
}
