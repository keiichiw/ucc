int main () {
  int a, b, i, n;
  a = 0;b = 1;n = 10;
  for (i=1; i<=n; i=i+1) {
    int t;
    t = b;
    b = a + b;
    a = t;
  }
  print_int(a);
  return 0;
}
