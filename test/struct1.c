/*
1
2
3
4
5
*/
#include "test.h"

struct pair{
  int x, y;
};
int main () {
  struct tri{
    int x, y, z;
  };
  struct pair a;
  struct tri  b;
  a.x = 1;
  a.y = 2;
  b.x = 3;
  b.y = 4;
  b.z = 5;
  print_int(a.x);
  print_int(a.y);
  print_int(b.x);
  print_int(b.y);
  print_int(b.z);
}
