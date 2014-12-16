int print_int (int a) {
  unsigned n = a;
  asm_write(n>>24);
  asm_write(n>>16);
  asm_write(n>>8);
  asm_write(n);
  return 0;
}

int __bitget (unsigned n, unsigned i) {
  return (n<<(31-i))>>31;
}

int __bitset (int n, int i, int x) {
  if (__bitget(n, i) == x) {
    return n;
  } else {
    if (x==1) {
      return n + (1<<i);
    } else {
      return n - (1<<i);
    }
  }
}

int __div_kernel (int n, int d, int *q1, int *r1) {
  int q, r, i, nflg;
  int ntmp, dtmp;
  q = r = 0;

  nflg = (n < 0) ^ (d < 0);
  ntmp = (n < 0) ? -n : n;
  dtmp = (d < 0) ? -d : d;

  if (dtmp == 0) {
    return 0;
  }
  for (i=ntmp-1; i>=0; i-=1) {
    int ni;
    r = r << 1;
    ni = __bitget(ntmp, i);
    r = __bitset(r, 0, ni);
    if (r >= dtmp) {
      r = r - dtmp;
      q = __bitset(q, i, 1);
    }
  }

  if (nflg == 1) {
    q = -q;
    r = -r;
  }

  *q1 = q;
  *r1 = r;
  return 0;
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

int __mul (int a, int b) {
  int i,r;
  r=0;
  for (i=31; i>=0; i-=1) {
    r = r<<1;
    if (__bitget(b,i)) {
      r += a;
    }
  }
  return r;
}

int __and(int a, int b) {
  int i, x;

  x = 0;
  for (i = 31; i >= 0; --i) {
    x = x << 1;
    if (__bitget(a, i) + __bitget(b, i) >= 2) {
      x += 1;
    }
  }

  return x;
}

int __or(int a, int b) {
  int i;
  unsigned x = 0;
  for (i = 31; i >= 0; --i) {
    x = x << 1;
    if (__bitget(a, i) + __bitget(b, i) >= 1) {
      x += 1;
    }
  }

  return x;
}

int __not(int a) {
  return (-a)-1;
}

int __xor(int a, int b) {
  int i, x;
  x = 0;
  for (i = 31; i >= 0; --i) {
    x = x << 1;
    if (__bitget(a, i) + __bitget(b, i) == 1) {
      x += 1;
    }
  }

  return x;
}

int __ashr(int a, int b) {
  unsigned ua = a;
  unsigned ub = b;
  int r;
  if (a < 0) {
    int x1, x2;
    unsigned x = ~0;
    x >>= ub;
    x1 = ~x;
    x2 = (ua>>ub);
    r = x1 | x2;
  } else {
    r = (ua >> ub);
  }
  return r;
}

int __ash (int a, int b) {
  int r;
  if (0>b) {
    r = __ashr(a, -b);
  } else {
    unsigned ua = a;
    unsigned ub = b;
    r = (ua << ub);
  }
  return r;
}
