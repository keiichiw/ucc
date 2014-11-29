int print_int (int n) {
  asm_write(n>>24);
  asm_write(n>>16);
  asm_write(n>>8);
  asm_write(n);
  return 0;
}

int __bitget (int n,  i) {
  return (n<<(31-i))>>31;
}

int __bitset (int n,  i,  x) {
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

int __div_kernel (int n, d, *q1, *r1) {
  int q, r, i;
  q = r = 0;
  if (d == 0) {
    return 0;
  }
  for (i=n-1; i>=0; i-=1) {
    int ni;
    r = r << 1;
    ni = __bitget(n, i);
    r = __bitset(r, 0, ni);
    if (r >= d) {
      r = r - d;
      q = __bitset(q, i, 1);
    }
  }
  *q1 = q;
  *r1 = r;
  return 0;
}

int __div (int n, d) {
  int p, q;
  __div_kernel(n, d, &p, &q);
  return p;
}
int __mod (int n, d) {
  int p, q;
  __div_kernel(n, d, &p, &q);
  return q;
}

int __mul (int a, b) {
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


/*
int main () {
  int i, j;
  for (i=2; i<=100; i+=1) {
    int flg;
    flg = 1;
    for (j=2; j<=(i-1); j+=1) {
      if (i%j==0) {
        flg = 0;
      }
    }
    if (flg == 1) {
      print_int(i);
    }
  }
  return 0;
}



int main () {
  int isprime[1001];
  int i;
  for (i=0; i<=1000; i+=1) {
    isprime[i] = 1;
  }
  isprime[0] = isprime[1] = 0;
  for (i=2; i<=1000; i+=1) {
    if (isprime[i]==0) {
      //continue;
    } else {
      int j;
      for (j=2; i*j <=1000 ; j+=1) {
        isprime[i*j] = 0;
      }
    }
  }
  for (i=0; i<=1000; i+=1) {
    if (isprime[i] == 1) {
      print_int(i);
    }
  }
  return 0;
}
*/
