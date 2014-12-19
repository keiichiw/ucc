/*
1
2
3
3
2
1
*/

#include "test.h"

int main () {
  int i;
  int a[3] = {1,2,3};
  int *p = a;
  for (i=0; i<3; i++) {
    print_int(*(p++));
  }
  --p;
  for (i=2; i>=0; i--) {
    print_int(*(p--));
  }
}
