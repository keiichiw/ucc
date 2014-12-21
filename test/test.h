int __mul(int, int);
int __div(int, int);
int __mod(int, int);
void _putc (char);
int _printf();

#ifdef __UCC__
void print_int(int n) {
  _printf("%d\n", n);
}
#define printf(...) _printf(__VA_ARGS__)
#else
#include <stdio.h>
void print_int(int n){
  printf("%d\n", n);
}
#endif
