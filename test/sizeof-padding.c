/*
5
10
16
12
*/

#include <stdio.h>

int main()
{
  struct foo {
    char c;
    char a[2];
    struct { unsigned char x, y; } s;
  } f[2];

  struct bar {
    char c;
    short s;
    int i;
    char d;
  };

  struct baz {
    char c;
    union { char c; } u;
    struct {
      short s;
      int i;
    } s;
  };

  printf("%lu\n", sizeof (struct foo));
  printf("%lu\n", sizeof f);
  printf("%lu\n", sizeof (struct bar));
  printf("%lu\n", sizeof (struct baz));
}
