/*
0.5000
-0.5000
-1.2500
1.2500
-0.7500
1.7500
-2.2500
-0.6250
-0.4000
-2.5000
0
-1
0
1
*/
#include "test.h"

int main () {
  float a=0.5, b=-1.25;
  printf("%.4f\n",  a);
  printf("%.4f\n", -a);
  printf("%.4f\n",  b);
  printf("%.4f\n", -b);
  printf("%.4f\n", a+b);
  printf("%.4f\n", a-b);
  printf("%.4f\n", b-1);
  printf("%.4f\n", a*b);
  printf("%.4f\n", a/b);
  printf("%.4f\n", b/a);
  printf("%d\n", (int)a);
  printf("%d\n", (int)b);
  printf("%d\n", (int)-a);
  printf("%d\n", (int)-b);
  return 0;
}
