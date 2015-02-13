/*
3.900
1.000
1.000
7.000
4.000
*/
#include <stdio.h>

float a = 1.9+2.0;
float b = 1;
float c[] = {
  1,
  1 + 2 * 3,
  3 / 2 * 4.0
};

int main () {
  printf("%.3f\n", a);
  printf("%.3f\n", b);
  printf("%.3f\n", c[0]);
  printf("%.3f\n", c[1]);
  printf("%.3f\n", c[2]);
}
