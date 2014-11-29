/*
  Test for Array Expression
  output: 4 3 2 1 0
 */

int main () {
  int a[10], i;
  for (i=0;i<=4;i+=1) {
    a[i] = i;
  }
  for (i=4;i>=0;i-=1) {
    print_int(a[i]);
  }
  return 0;
}
