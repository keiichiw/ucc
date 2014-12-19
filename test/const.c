/*
0
0
0
*/
#include "test.h"

int main()
{
  const int i = 0;
  int const * j = 0;
  int * const k = 0;

  print_int(i);
  print_int(j);
  print_int(k);
}
