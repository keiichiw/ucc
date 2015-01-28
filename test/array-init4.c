/*
HelloWorld
*/

#include "test.h"

char buf[][5] = { "Hello", "World" };

int main()
{
    int i;
    for (i = 0; i < sizeof buf; ++i) {
        _putc(((char*)buf)[i]);
    }
    printf("\n");
}
