/*
OK
OK
OK
OK
*/

#include <stdio.h>
#include <stdlib.h>

#define N 10000

#define check(c)                                \
  do {                                          \
    if (c)                                      \
      printf("OK\n");                           \
    else                                        \
      printf("NG\n");                           \
  } while (0)


int
my_rand(int mod)
{
  static int seed = 876543;

  return seed = (seed * 11 + 5) % mod;
}


int
test(int *list[N])
{
  int i, j;
  for (i = 0; i < N; ++i) {
    int s;
    if (list[i] == NULL) {
      continue;
    }
    s = list[i][0];
    for (j = 0; j < s; ++j) {
      if (s != list[i][j]) {
        return 0;
      }
    }
  }
  return 1;
}



int main()
{
  int i, j;
  int *list[N];

  // initialize
  for (i = 0; i < N; ++i) {
    int s = my_rand(100);
    list[i] = malloc((s + 1) * sizeof(int));
    for (j = 0; j < s; ++j) {
      list[i][j] = s;
    }
  }

  check(test(list));

  for (i = 0; i < N; i+=3) {
    free(list[i]);
    list[i] = NULL;
  }

  check(test(list));

  for (i = 1; i < N; i+=3) {
    free(list[i]);
    list[i] = NULL;
  }

  check(test(list));

  for (i = 2; i < N; i+=3) {
    free(list[i]);
    list[i] = NULL;
  }

  check(test(list));

  return 0;
}
