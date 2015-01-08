/*
1
*/
#include "test.h"

static struct s {
  int a;
} x;

int main () {
  x.a = 1;
  print_int(x.a);
}
