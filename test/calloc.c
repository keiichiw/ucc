/*
OK
*/
#include <stdio.h>
#include <stdlib.h>
#define N 100
typedef struct {int x, y;} foo;
int main () {
  unsigned i;
  char *c  = (char*) calloc(N, sizeof(foo));
  for (i = 0; i < sizeof(foo)*N/sizeof(char); ++i) {
    if (c[i] != 0) {
      printf("NG\n");
      return 0;
    }
  }
  printf("OK\n");
  return 0;
}
