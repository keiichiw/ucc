/*
200
25
-1011703408
159868227
-39488
-20
*/

#include <stdio.h>

int main()
{
    int i = 100;
    unsigned u = 0x98765432;
    long l = -1234;

    printf("%d\n", i << 1);
    printf("%d\n", i >> 2);
    printf("%d\n", u << 3);
    printf("%d\n", u >> 4l);
    printf("%d\n", l << 5);
    printf("%d\n", l >> 6u);
}
