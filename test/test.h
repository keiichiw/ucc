void _signed_divmod(int, int, int*, int*);
void _unsigned_divmod(unsigned, unsigned, unsigned*, unsigned*);
void _putc(char);
char _getc();
int  _printf();

#ifdef __UCC__
#define printf(...) _printf(__VA_ARGS__)
#else
#include <stdio.h>
#endif

#define print_int(n) printf("%d\n", n)
