/*
1.414
1.732
2.236
3.162
*/

#include <stdio.h>
#define EPS 0.00001

float myfabs(float a, float b) {
  return a < b ? b-a : a-b;
}

/*
 calculate square-root by Newton Method
*/
float mysqrt(float a) {
  float x0, x1;
  x0 = 1.0;
  x1 = 0;
  while (myfabs(x0, x1) > EPS) {
    float f, fd;
    f  = x0 * x0 - a;
    fd = 2  * x0;
    x1 = x0;
    x0 -= f/fd;
  }
  return x0;
}

int main() {
  printf("%.3f\n", mysqrt(2));
  printf("%.3f\n", mysqrt(3));
  printf("%.3f\n", mysqrt(5));
  printf("%.3f\n", mysqrt(10));
  return 0;
}
