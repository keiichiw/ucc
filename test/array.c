/*
  Test for Array Expression
  output: 4 3 2 1 0
 */

int main () {
  int a[10], i;
  for (i=0;i<10;i++) {
    a[i] = i;
  }
  i = 9;
  while (i>=0) {
    print_int(a[i--]);
  }
  return 0;
}
