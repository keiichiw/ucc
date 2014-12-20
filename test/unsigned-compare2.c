/*
0
1
0
1
1
1
1
0
0
0
1
1
0
0
0
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
1
1
0
0
0
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
0
1
1
0
0
1
0
1
1
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
0
1
1
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

int main() {
  unsigned a[4] = {2, 3, 1<<17, (1<<17)+1};
  int i;
  for (i=0;i<4;i++) {
    int j;
    for (j=0;j<4;j++) {
      print_int(a[i] <  a[j]);
      print_int(a[i] <= a[j]);
      print_int(a[i] >  a[j]);
      print_int(a[i] >= a[j]);
      print_int(a[i] == a[j]);
    }
  }
}
