/*
3
3
*/
#include "test.h"

int main()
{
  int a = 3;
  if (a == 3)
    print_int(3);

  if (a == 4)
    print_int(5);
  else
    if (a == 3)
      print_int(3);
    else
      print_int(4);
}
