/*
72
101
108
108
111
44
32
87
111
114
108
100
33
10
*/
#include "ucc.h"

int
main()
{

  char *p = "Hello, World!\n";
  int i = 0;

  while (p[i]) {
    print_int(p[i++]);
  }

  return 0;
}
