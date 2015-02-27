/*
42
Hello!!
FFFFFFFF
000000FF
FFFFABCD
0000ABCD
1 2
*/

#include <stdio.h>

char str[] = "Hello!";
unsigned short ss[] = {0xabcd, 0};

int main() {
  int a;
  char c1, c2, *p;
  unsigned char uc;
  short s;
  struct {short a; int b;} v1 = { 1, 2}, v2;
  a = 42;
  p = str; c1 = str[5];
  uc = 255; c2 = uc;
  s = ss[0]; ss[0] = s;
  v2 = v1;
  printf("%d\n", a);
  printf("%s%c\n", p, c1);
  printf("%08X\n%08X\n", c2, uc);
  printf("%08X\n", s);
  printf("%08X\n", ss[0]);
  printf("%d %d\n", v2.a, v2.b);
}
