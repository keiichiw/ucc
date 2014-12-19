/*
1
1
0
0
0
1
0
1
1
1
0
0
1
1
0
0
0
0
1
1
0
0
1
1
0
0
1
1
0
1
0
1
*/
#include "ucc.h"

int main () {
  print_int(1< 2);
  print_int(1<=2);
  print_int(1> 2);
  print_int(1>=2);
  print_int(1< 1);
  print_int(1<=1);
  print_int(1> 1);
  print_int(1>=1);

  print_int(-1< 2);
  print_int(-1<=2);
  print_int(-1> 2);
  print_int(-1>=2);
  print_int(-1< 1);
  print_int(-1<=1);
  print_int(-1> 1);
  print_int(-1>=1);

  print_int(1< -2);
  print_int(1<=-2);
  print_int(1> -2);
  print_int(1>=-2);
  print_int(1< -1);
  print_int(1<=-1);
  print_int(1> -1);
  print_int(1>=-1);

  print_int(-1< -2);
  print_int(-1<=-2);
  print_int(-1> -2);
  print_int(-1>=-2);
  print_int(-1< -1);
  print_int(-1<=-1);
  print_int(-1> -1);
  print_int(-1>=-1);
  return 0;
}
