/*
72
101
108
108
111
10
0
*/
#include "test.h"

int
main()
{

  char a[7] = "Hello\n";
  int i;

  for (i = 0; i < 7; ++i) {
    print_int(a[i]);
  }
  return 0;
}
