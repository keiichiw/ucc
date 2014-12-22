/*
0
2
1
Hello, world!
one two three
9
14
3
*/

#include "test.h"

int main()
{
    int a[][3] = { { 1 }, { 1, 2 }, { 1, 2, 3 } };
    char s[] = "Hello, world!";
    char *p[] = { "one", "two", "three" };

    print_int(a[0][2]);
    print_int(a[1][1]);
    print_int(a[2][0]);
    printf("%s\n", s);
    printf("%s %s %s\n", p[0], p[1], p[2]);
    print_int(sizeof(a));
    print_int(sizeof(s));
    print_int(sizeof(p));
}
