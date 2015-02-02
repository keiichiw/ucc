/*
0
-15
-120
-15
-1
15
0
-7
3
*/

#include <stdio.h>

int main()
{
    int s = -15;
    unsigned u = 123;

    printf("%d\n", s * 0);
    printf("%d\n", s * 1);
    printf("%d\n", s * 8);
    printf("%d\n", s / 1);
    printf("%d\n", s / 8);
    printf("%d\n", u / 8);
    printf("%d\n", s % 1);
    printf("%d\n", s % 8);
    printf("%d\n", u % 8);
}
