int _mul(int a, int b);
int _div(int a, int b);
int _mod(int a, int b);
int _printint(int a, int b, int c);
void _putc (char c);
int _printf();

#ifndef __UCC__
#include <stdio.h>
void print_int(int n){
  printf("%d\n", n);
}
#else
void print_int(int n) {
  _printint(n, 10, 1);
  _putc('\n');
}
#define printf(...) _printf(__VA_ARGS__)
#endif
