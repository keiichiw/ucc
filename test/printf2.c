/*
42
         +0000000042
0x00ffffffff
10.2300
INF
Hello, world!
       Hello, world!
Hello, wor          $
   1  10 100
123: Hello, world!
10000000.000000, 1.000000e+07, 1e+07
4275817112, 012
*/

#include <stdio.h>

int main()
{
    float inf = 1.0 / 0.0;
    char *str = "Hello, world!";
    char buf[32];

    printf("%d\n", 42);
    printf("%+20.10d\n", 42);
    printf("%#012x\n", 0xffffffffu);
    printf("%#.6g\n", 10.23);
    printf("%F\n", inf);
    printf("%s\n", str);
    printf("%20s\n", str);
    printf("%-20.10s$\n", str);
    printf("%*d%*d%*d\n", 4, 1, 4, 10, 4, 100);

    sprintf(buf, "%d: %s", 123, str);
    printf("%s\n", buf);

    fprintf(stdout, "%f, %e, %g\n", 1e7, 1e7, 1e7);
    fprintf(stderr, "%u, %#.3o\n", 0xfedbca98, 012);
}
