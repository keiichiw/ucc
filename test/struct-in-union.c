/*
7
2
*/

#include "test.h"

int main() {
    union tag {
        int a;
        struct { int b; char c; } s;
    } x;
    union tag y;
    x.a = 1;
    x.s.b = 2;
    y.s.c = 3;
    print_int(x.a + x.s.b + y.s.c);
    print_int(sizeof(union tag));
}
