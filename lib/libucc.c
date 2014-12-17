int print_int (int n) {
  asm_write(n >> 24);
  asm_write(n >> 16);
  asm_write(n >>  8);
  asm_write(n);
  return 0;
}

int __mul (int a, int b) {
  int i, r;
  r = 0;
  for (i = 31; i >= 0; --i) {
    r = r << 1;
    if ((b >> i) & 1) r += a;
  }
  return r;
}

int __div_kernel (int n, int d, int *qp, int *rp) {
  int sign = ((n >> 31) ^ (d >> 31)) & 1;
  int i, q = 0, r = 0;
  if (n < 0) n = -n;
  if (d < 0) d = -d;
  for (i = 31; i >= 0; --i) {
    r <<= 1;
    r += (n >> i) & 1;
    if (r >= d) {
      q += 1 << i;
      r -= d;
    }
  }
  *qp = sign ? -q : q;
  *rp = sign ? -r : r;
}

int __div (int n, int d) {
  int p, q;
  __div_kernel(n, d, &p, &q);
  return p;
}

int __mod (int n, int d) {
  int p, q;
  __div_kernel(n, d, &p, &q);
  return q;
}

