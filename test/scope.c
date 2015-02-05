/*
3, 2, 3, 1, 2, 1
0, 42
123, 0
3, 42
3, 2
*/

#include <stdio.h>
#include <stddef.h>

typedef struct foo foo;
typedef union bar bar;

struct foo {
    foo *p;
    bar *q;
    union { int a; float b; } u;
};

union bar {
    foo *p;
    bar *q;
    struct { int a; float b; } s;
};

enum { baz };

int test()
{
  int s1 = sizeof (foo);
  int s2 = sizeof (bar);

  struct foo { int a; };
  union bar { int a; };

  int s3 = sizeof (foo);
  int s4 = sizeof (struct foo);
  int s5 = sizeof (bar);
  int s6 = sizeof (union bar);

  int x = baz;
  int baz = 42;

  printf("%d, %d, %d, %d, %d, %d\n", s1, s2, s3, s4, s5, s6);
  printf("%d, %d\n", x, baz);

  {
    foo foo = { NULL, NULL, { 123 } };
    enum { baz };

    printf("%d, %d\n", foo.u.a, baz);
  }

  printf("%d, %d\n", sizeof (foo), baz);
}

int main()
{
  test();

  printf("%d, %d\n", sizeof (struct foo), sizeof (union bar));
}
