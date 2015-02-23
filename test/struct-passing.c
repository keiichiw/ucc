/*
{
 a = 1,
 f =  {x = 2.000000, y = 3.000000}
}
*/
#include <stdio.h>

typedef struct {float x, y;} foo;
typedef struct {int a; foo f;} bar;


void print_foo(foo f)
{
  printf(" {x = %f, y = %f}\n", f.x, f.y);
}

void print_bar(bar b)
{
  printf("{\n a = %d,\n f = ", b.a);
  print_foo(b.f);
  printf("}\n");
}

int main() {
  bar b = { 1, { 2.0, 3.0}};
  print_bar(b);
  return 0;
}
