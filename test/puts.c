/*
Hello World!
Hello World!
1 13
*/

#include <stdio.h>

int main (){
  int a, b;

  a = fputs("Hello World!\n", stdout);
  b = puts("Hello World!");

  printf("%d %d\n", a, b);

  return 0;

}
