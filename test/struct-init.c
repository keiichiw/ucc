/*
1
2
0
4
1
2
3
0
*/

struct foo {
    int a;
    int b[2];
    int c;
};

int main() {
    struct foo x = { 1, { 2 }, 4 };
    struct foo y = { 1, 2, 3 };
    print_int(x.a);
    print_int(x.b[0]);
    print_int(x.b[1]);
    print_int(x.c);
    print_int(y.a);
    print_int(y.b[0]);
    print_int(y.b[1]);
    print_int(y.c);
}
