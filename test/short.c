/*
2
10
0000abcd
2
2
2
FFFFFF9C
FF9C
0
1
2
3
4
*/

#include <stdio.h>

short a = 1;

int main() {
  short b;
  static short c;
  short ss[5] = {0};
  int i;
  signed char sc = -100;
  unsigned short us;
  union {short s; unsigned u;} uni;

  printf("%ld\n", sizeof(short));
  printf("%ld\n", sizeof(ss));

  uni.u = 0;
  uni.s = 0xabcd;
  printf("%08x\n", uni.u);


  b = a;
  c = b++;
  a = ++c;
  printf("%d\n", a);
  printf("%d\n", b);
  printf("%d\n", c);

  b  = sc;
  us = sc;
  printf("%X\n", b);
  printf("%X\n", us);

  for (i = 1; i < 5; ++i) {
    ss[i] = ss[i-1] + 1;
  }
  for (i = 0; i < 5; ++i) {
    printf("%d\n", ss[i]);
  }


}
