/*
1
2
3
4
*/
#include "ucc.h"

struct foo {
    int a;
    int b[2];
    int c;
};

struct foo f = { 1, { 2, 3 }, 4 };

int main() {
    print_int(f.a);
    print_int(f.b[0]);
    print_int(f.b[1]);
    print_int(f.c);
}
