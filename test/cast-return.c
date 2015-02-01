/*
0.5000
0.0000
0
1
*/
#include <stdio.h>

float
f(float x)
{
  return x;
}

int
g(float x)
{
  return x;
}

unsigned int
h(int x)
{
  return x;
}

int main()
{
  float x;
  x = f(0.5);
  printf("%.4f\n", x);
  x = g(0.5);
  printf("%.4f\n", x);
  printf("%d\n", 1 < -1);
  printf("%d\n", 1 < h(-1));
}
