/*
3
5
*/

#include "test.h"

void assign5();

int main()
{
  extern int a;

  print_int(a);

  assign5();

  print_int(a);
}

int a = 3;

void assign5()
{
  a = 5;
}
