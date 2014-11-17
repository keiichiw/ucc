int x, y, z;
int a, b, c;
int fib (int x) {
  if (x < 2) {
    return x;
  } else {
    return fib(x-1) + fib(x-2);
  }
}
