/*
Hello world!
Hello world!
*/

#include "test.h"

int main() {
    char a[2][10] = { "Hello", "world!" };
    char *p[2] = { "Hello", "world!" };
    printf("%s %s\n", a[0], a[1]);
    printf("%s %s\n", p[0], p[1]);
}
