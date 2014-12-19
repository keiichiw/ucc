/*
1
*/
#include "test.h"

int
main()
{
  int n = 1;

  switch (n) {
  case 1 + 2:
    break;
  case 3 - 4:
    break;
  case 4 == 4:
    print_int(1);
    break;
  case 3 < 1:
    break;
  }

  return 0;
}
