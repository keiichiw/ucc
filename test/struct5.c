/*
32
*/
#include "ucc.h"


struct tri {
  int x, y, z;
};

int inner_product (struct tri* a, struct tri* b) {
  int ans = 0;
  ans += a->x * b->x;
  ans += a->y * b->y;
  ans += a->z * b->z;
  return ans;
}

int main () {
  struct tri p, q;
  int ans;
  p.x = 1;
  p.y = 2;
  p.z = 3;
  q.x = 4;
  q.y = 5;
  q.z = 6;
  ans = inner_product (&p, &q);
  print_int(ans);
  return 0;
}
