/*
1
1
1
1
*/

#include "test.h"
int main () {
  int a[5];
  print_int((a+1)-a);
  print_int((void*)(a+1)-(void*)a);
  print_int((void*)((void*)(a)+1) - (void*) a);
  print_int((void*)(a+1) - (void*)a);
  return 0;
}
