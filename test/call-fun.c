/*
2
*/
#include "test.h"

int f (int x) {
  return x+1;
}

int g(int x) {
  return f(x+1);
}

int main () {
  print_int(g(0));
  return 0;
}
