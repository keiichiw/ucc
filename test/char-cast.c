/*
260
4
4
-42
214
*/
#include <stdio.h>

int main() {
  float f = 260, g = -42.5;
  int a, b;
  char c;
  unsigned char uc;
  a = f;
  b = (char)f;
  c = a;
  printf("%d\n%d\n%d\n", a, b, c);
  c  = g;
  uc = c;
  printf("%d\n%d\n", c, uc);
}
