/*
8
2
*/

#include "test.h"

int main() {
    union tag {
        int a;
        union { int b; int c[2]; } u;
    } x;
    union tag y;
    x.a = 1;
    x.u.b = 2;
    y.u.c[0] = 3;
    print_int(y.a + y.u.b + x.u.c[0]);
    print_int(sizeof(union tag));
}
