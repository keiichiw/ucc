/*
ok
*/

#include "test.h"

struct foo {
  int bar;
};

typedef struct foo bar;

int main()
{
  bar a;
  a.bar;

  printf("ok\n");
}
