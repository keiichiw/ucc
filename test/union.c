/*
0
2
42
20
*/

#include "test.h"

typedef struct {
    int a;
    int b[2];
} foo;

typedef struct {
    int a[2];
    int b;
} bar;

typedef union {
    foo f;
    bar b;
    int i[5];
} uni;

int main() {
    uni u = { 0, { 1, 2 } };
    print_int(u.f.a);
    print_int(u.i[2]);
    u.b.b = 42;
    print_int(u.i[2]);
    print_int(sizeof(uni));
}
