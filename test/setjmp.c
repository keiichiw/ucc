/*
sub start
jump: 123
x = 1
main end
*/

#include <setjmp.h>
#include "test.h"

jmp_buf jmpbuf1;

void sub(void) {
  printf("sub start\n");
  longjmp(jmpbuf1, 123);
  printf("sub end\n");
}

int main() {
  int x = 0;
  int c = setjmp(jmpbuf1);
  if (c == 0) {
    x = 1;
    sub();
  } else {
    printf("jump: %d\n", c);
    printf("x = %d\n", x);
  }
  printf("main end\n");
}
