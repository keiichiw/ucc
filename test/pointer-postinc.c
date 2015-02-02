/*
OK
*/
#include "test.h"

struct foo {
  int a, b;
};

static struct foo fa[100];

int main()
{
  struct foo *fp;
  int i = 0;

  for (fp = fa; fp < fa + 100; fp++) {
    fp->a = i++;
    fp->b = i++;
  }

  i = 0;

  for (fp = fa; fp < fa + 100; fp++) {
    if (fp->a != i++)
      printf("error");
    if (fp->b != i++)
      printf("error");
  }
  printf("OK\n");
}
