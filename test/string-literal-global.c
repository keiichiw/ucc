/*
72
101
108
108
111
44
32
87
111
114
108
100
33
10
72
101
108
108
111
44
32
87
111
114
108
100
33
10
*/
#include "test.h"

char a[32] = "Hello, World!\n";
char *p = "Hello, World!\n";

int main() {
    int i = 0, j = 0;

    while (a[i]) {
        print_int(a[i++]);
    }

    while (p[j]) {
        print_int(p[j++]);
    }
}
