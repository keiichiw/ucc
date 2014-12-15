/*
210
455
*/

#ifndef __UCC__
#include <stdio.h>
int print_int(int n){
  printf("%d\n", n);
}
#endif

int dp[30][30];

int comb (int a, int b) {
  if (dp[a][b] != 0)
    return dp[a][b];
  if (a < b)
    return -1;
  if (a == b || b == 0) {
    dp[a][b] = 1;
  } else {
    dp[a][b] = comb(a-1, b-1) + comb(a-1, b);
  }
  return dp[a][b];
}

int main () {
  print_int(comb(10, 4));
  print_int(comb(15, 3));
}
