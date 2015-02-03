/*
OK
*/

#include <stdio.h>
#include <stdlib.h>

int main()
{
    int i;
    char *p[1200];

    while (i < 1200) {
        int s1 = rand() % 4096;
        int s2 = rand() % 4096;

        if (! (p[i] = malloc(s1))) goto end;
        p[i++][s1 - 1] = 0;

        if (! (p[i] = malloc(s2))) goto end;
        p[i++][s2 - 1] = 0;

        free(p[i / 2]);
    }

    printf("NG\n");
    return 0;

end:
    printf(i > 600 ? "OK\n" : "NG\n");
}
