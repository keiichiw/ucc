/*
6
12
*/

#include "test.h"

int main() {
    struct tag {
        int a;
        union { int b; int c[2]; } u;
    } x;
    struct tag y;
    x.a = 1;
    x.u.b = 2;
    y.u.c[0] = 3;
    print_int(x.a + y.u.b + x.u.c[0]);
    print_int(sizeof(struct tag));
}
