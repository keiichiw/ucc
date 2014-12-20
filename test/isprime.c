/*
2
3
5
7
11
13
*/
#include "test.h"

int isPrime (int p) {
  int i, flg;
  flg = 1;
  for (i=2; i<p; ++i) {
    int a;
    if (p%i == 0) {
      flg = 0;
      break;
    }
    flg = 1;
  }
  return flg;
}

int main () {
  int i;
  for (i=2;i<15;++i) {
    if (isPrime(i))
      print_int(i);
  }
  return 0;
}
