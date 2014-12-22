/*
256
1024
*/

#include "test.h"

int main()
{
  char buf[256];
  struct { int a, b[3]; } buf2[256];

  print_int(sizeof(buf));
  print_int(sizeof buf2);
}
