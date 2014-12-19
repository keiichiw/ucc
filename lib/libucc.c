int asm_write(int n);


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


void __putc (char c) {
  asm_write(c);
}

void __printint(int xx, int base, int sgn) {
  char digits[20] = "0123456789ABCDEF";
  char buf[16];
  int i, neg;
  unsigned x;

  neg = 0;
  if(sgn && xx < 0) {
    neg = 1;
    x = -xx;
  } else {
    x = xx;
  }

  i = 0;
  do{
    buf[i++] = digits[x % base];
  }while((x /= base) != 0);

  if(neg)
    buf[i++] = '-';

  while(--i >= 0)
    __putc(buf[i]);
}
