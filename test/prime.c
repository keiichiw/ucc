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

int main () {
  int i, j;
  for (i=2; i<=100; i+=1) {
    int flg;
    flg = 1;
    for (j=2; j<=(i-1); j+=1) {
      flg = 1;
      if (i%j==0) {
        flg = 0;
        break;
      }
    }
    if (flg == 1) {
      print_int(i);
    }
  }
  return 0;
}
