/*
0
0
1
1
0
OK
OK
*/

#include <stdio.h>

int main()
{
  float t = 0.1;
  float f = -0.0;
  printf("%d\n",  0.0 ? 1 : 0);
  printf("%d\n", -0.0 ? 1 : 0);
  printf("%d\n",  0.1 ? 1 : 0);
  printf("%d\n", t ? 1 : 0);
  printf("%d\n", f ? 1 : 0);
  if ( 0.0) printf("NG\n");
  if (-0.0) printf("NG\n");
  if ( 0.1) printf("OK\n");
  if (t) printf("OK\n");
  if (f) printf("NG\n");
}
