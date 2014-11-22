int main () {
  int a, b, i, n;
  a = 0;b = 1;n = 30;
  for (i=1; i<=n; i+=1) {
    int t;
    t = b;
    b += a;
    a = t;
  }
  print_int(a);
  return 0;
}
