/*
2.0000
3.0000
*/

#include <stdio.h>

int main()
{
    printf("%.4f\n", 2 * (0 ? 1.5 : 1));
    printf("%.4f\n", 2 * (1 ? 1.5 : 1));
}
