/*
0
0
0
*/
#include "test.h"

int main()
{
  volatile int i = 0;
  int volatile * j = 0;
  int * volatile k = 0;

  print_int(i);
  print_int(j);
  print_int(k);
}
