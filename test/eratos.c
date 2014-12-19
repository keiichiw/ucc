/*
2
3
5
7
11
13
17
19
23
29
31
37
41
43
47
53
59
61
67
71
73
79
83
89
97
*/
#include "test.h"

#define N 100

int main () {
  int isprime[N];
  int i;

  for (i=0; i<N; ++i) {
    isprime[i] = 1;
  }

  isprime[0] = isprime[1] = 0;
  for (i=2; i<N; ++i) {
    if (isprime[i]==1) {
      int j;
      for (j=2; i*j < N; ++j) {
        isprime[i*j] = 0;
      }
    }
  }

  for (i=0; i<N; ++i) {
    if (isprime[i] == 1) {
      print_int(i);
    }
  }

  return 0;
}
