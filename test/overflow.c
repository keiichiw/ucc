/*
00000000
000000FF
FFFFFFFE
0000FFFF
*/

#include <stdio.h>

int main() {
  char c = 255;
  unsigned char uc = 0;
  short s = 32767;
  unsigned short us = 0;
  c++;
  uc -= 1;
  s *= 2;
  --us;
  printf("%08X\n", c);
  printf("%08X\n", uc);
  printf("%08X\n", s);
  printf("%08X\n", us);
}
