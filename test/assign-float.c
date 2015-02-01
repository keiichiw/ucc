/*
0.5000
1.5000
1.7500
3.5000
0.8750
*/
#include <stdio.h>

int main()
{
  float a = 0.5;
  printf("%.4f\n", a);

  a = 1.5;
  printf("%.4f\n", a);

  a += 0.25;
  printf("%.4f\n", a);

  a *= 2;
  printf("%.4f\n", a);

  a /= 4;
  printf("%.4f\n", a);

  return 0;
}
