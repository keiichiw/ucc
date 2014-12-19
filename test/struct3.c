/*
12
*/
#include "test.h"

struct point {
  int x, y;
};
struct rect {
  struct point a, b;
};

int abs (int a) {
  return 0<=a ? a : -a;
}

int main () {
  struct rect sq;
  int p;
  sq.a.x = 0;
  sq.a.y = 2;
  sq.b.x = 3;
  sq.b.y = 6;
  p = abs(sq.a.x-sq.b.x) * abs(sq.a.y-sq.b.y);
  print_int(p);
  return 0;
}
