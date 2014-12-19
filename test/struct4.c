/*
0
1
4
9
16
25
36
49
64
81
*/
#include "ucc.h"

struct pair {
  int x, y;
};

int main () {
  int i;
  struct pair a[10];
  for (i=0; i<10; i++) {
    a[i].x = a[i].y = i;
  }
  for (i=0; i<10; i++) {
    print_int(a[i].x * a[i].y);
  }
  return 0;
}
