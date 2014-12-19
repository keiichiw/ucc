/*
Hello! UCC
ABC
1 + 2 = 3
42 = 0x2A
*/

#include "test.h"

int main () {
  int a = 1, b = 2;
  int num = 42;
  char cc[5] = "UCC";

  printf("Hello! %s\n", cc);
  printf("%c%c%c\n", 'A', 'B', 'C');
  printf("%d + %d = %d\n", a, b, a+b);
  printf("%d = 0x%x\n", num, num);
  return 0;
}
