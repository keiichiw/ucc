/*
hello
abcd1234
*/

#include <stdio.h>

char buf[][4] = { "abcd", "1234", "\n" };
char *p = (char*)buf;

void foo()
{
  printf("hello\n");
}

int main()
{
  static void (*f)() = foo;
  f();
  for (; *p; ++p) putchar(*p);
}
