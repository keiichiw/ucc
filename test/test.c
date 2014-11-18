int x, y, z;

int proto(int, int);

int fib (int x)  {
  if (x < 2) {
    return x;
  } else {
    return fib(x-1) + fib(x-2);
  }
}

int fib2 (int n) {
  int i,a,b;
  a=0;
  b=1;
  for (i=0;i<n;i=i+1) {
    int t;
    t = b;
    b = a+b;
    a = b;
  }
  return a;
}

int sum (int n) {
  int i, sum;
  i = 0;
  sum = 0;
  while (i != n) {
    sum = sum + i;
    i = i + 1;
  }
  return sum;
}
