/*
sub start
x = 1
main end
*/

#include <setjmp.h>
#include <stdio.h>

int x;
jmp_buf buf;

void sub(void) {
  printf("sub start\n");
  longjmp(buf, 123);
  printf("sub end\n");
}

int main() {
  switch (setjmp(buf)) {
    case 0:
      x = 1;
      sub();
      break;
    case 123:
      printf("x = %d\n", x);
      break;
    default:
      printf("NG\n");
      break;
  }
  printf("main end\n");
}
