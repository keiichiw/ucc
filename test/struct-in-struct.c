/*
6
12
*/

#include "test.h"

int main() {
    struct tag {
        int a;
        struct { int b; char c; } s;
    } x;
    struct tag y;
    x.a = 1;
    x.s.b = 2;
    y.s.c = 3;
    print_int(x.a + x.s.b + y.s.c);
    print_int(sizeof(struct tag));
}
