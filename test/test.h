int __mul(int a, int b);
int __div(int a, int b);
int __mod(int a, int b);
int __printint(int a, int b, int c);
void __putc (char c);
#ifndef __UCC__
#include <stdio.h>
void print_int(int n){
  printf("%d\n", n);
}
#else
void print_int(int n) {
  __printint(n, 10, 1);
  __putc('\n');
}
#endif
