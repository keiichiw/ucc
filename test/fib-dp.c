/*
1134903170
44
832040
44
*/
#include "ucc.h"
int count;
int dp[50];
int fib (int x) {
  if (x < 2)
    return x;
  if (dp[x] > 0)
    return dp[x];
  count++;
  return dp[x] = fib(x-1) + fib(x-2);
}

int main () {
  print_int (fib(45));
  print_int (count);
  print_int (fib(30));
  print_int (count);
}
