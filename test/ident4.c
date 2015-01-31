/*
ok
*/

#include "test.h"

void foo(void) { typedef int hoge; }
void bar(void) { typedef int hoge; }

int main()
{
  foo, bar;

  printf("ok\n");
}
