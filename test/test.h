int __mul(int, int);
int __div(int, int);
int __mod(int, int);
void _putc(char);
char _getc();
int _printf();

#ifdef __UCC__
#define printf(...) _printf(__VA_ARGS__)
#else
#include <stdio.h>
#endif

#define print_int(n) printf("%d\n", n)
