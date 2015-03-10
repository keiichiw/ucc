/*
1 2.000000 3.000000
*/
#include <stdio.h>
typedef struct {int x; float a;} foo;
int main() {
  foo a = { 1, 2.0};
  foo b = a;
  float f = b.a + 1;

  printf("%d %f %f\n", b.x, b.a, f);

  return 0;

}
