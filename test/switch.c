/*
1
2
2
1
1
2
1
2
5
*/
#include "ucc.h"

int
main()
{
  switch (1) {
  case 0:
    print_int(0);
  case 1:
    print_int(1);
  case 2:
    print_int(2);
  }

  /* w/default */
  switch (3) {
  case 0:
    print_int(0);
  case 1:
    print_int(1);
  default:
    print_int(2);
  }

  /* w/break */
  switch (1) {
  case 0:
    print_int(0);
  case 1:
    print_int(1);
    break;
  default:
    print_int(2);
  }

  /* cases inside nested block */
  switch (1) {
    do {
    case 0:
      print_int(0);
    case 1:
      print_int(1);
    } while (0);
  default:
    print_int(2);
  }

  /* nested switches */
  switch (1) {
  case 0:
    print_int(0);
  case 1:
    print_int(1);
    switch (0) {
    case 0:
      print_int(2);
      break;
    case 1:
      print_int(3);
      break;
    default:
      print_int(4);
      break;
    }
  default:
    print_int(5);
    break;
  }
}
