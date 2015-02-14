/*
1
2, 3, 4
1, 3, 4
2
*/

#include <stdio.h>

int main()
{
    typedef struct { int x; } foo_t;
    typedef struct { int x, y, z; } bar_t;
    typedef union { bar_t x; int y; } baz_t;

    foo_t foo;
    bar_t bar;
    baz_t baz;

    foo_t a = { 1 };
    bar_t b = { 2, 3, 4 };
    baz_t c = { { 5, 6, 7 } };

    foo = a;
    bar = b;
    baz = c;

    c.x = (&baz)->x = *(&b);
    baz.y = foo.x;

    printf("%d\n", foo.x);
    printf("%d, %d, %d\n", bar.x, bar.y, bar.z);
    printf("%d, %d, %d\n", baz.x.x, baz.x.y, baz.x.z);
    printf("%d\n", c.x.x);
}
