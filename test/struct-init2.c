/*
1
2
3
4
5
6
7
8
*/
#include "ucc.h"

struct foo {
    int a;
    int b[2];
};

struct bar {
    struct foo x[2];
    int y[2];
};

int main() {
    struct bar b = { { { 1, { 2, 3 } }, { 4, { 5, 6 } } }, { 7, 8 } };
    print_int(b.x[0].a);
    print_int(b.x[0].b[0]);
    print_int(b.x[0].b[1]);
    print_int(b.x[1].a);
    print_int(b.x[1].b[0]);
    print_int(b.x[1].b[1]);
    print_int(b.y[0]);
    print_int(b.y[1]);
}
