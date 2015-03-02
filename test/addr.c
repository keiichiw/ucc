/*
5, 8
*/

#include <stdio.h>

int main()
{
    int a[3][3] = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };

    (&printf)("%d, %d\n", (&*a)[1][2], ((int *)(&a + 1))[-1]);
}
