/*
5
3
1
*/

#include "test.h"
typedef struct {
    int x, y[4], z;
} st;
int main () {
  st a, b[10];
  st *p, *q;
  int *pi, *qi;

  pi = &a.x;
  qi = &a.z;
  print_int (qi-pi);

  p = &b[3];
  q = &b[6];
  print_int (q-p);

  p = &b[9];
  print_int ((p+1) - p);
  return 0;
}
