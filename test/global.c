/*
100
10
*/
#include "test.h"

int count = 100;

int inc () {
  count += 1;
  return 0;
}

int main () {
  int i;
  print_int(count);
  count = 0;
  for (i=0; i< 10; i++) {
    inc ();
  }
  print_int(count);
  return 0;
}
