/*
1.5000
2.0000
1
2
*/
#include <stdio.h>

void
p_float(float f)
{
  printf("%.4f\n", f);
}

void
p_int(int i)
{
  printf("%d\n", i);
}

int
main()
{
  float a = 1.5;
  int   b = 2;
  p_float(a);
  p_float(b);
  p_int(a);
  p_int(b);
}
