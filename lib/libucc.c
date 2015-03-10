
int __mul(int a, int b) {
  __asm("\
  mov r1, [rbp + 4]     \n\
  mov r2, [rbp + 8]     \n\
  xor r3, r1, r2        \n\
  sar r4, r1, 31        \n\
  sar r5, r2, 31        \n\
  xor r1, r1, r4        \n\
  xor r2, r2, r5        \n\
  sub r1, r1, r4        \n\
  sub r2, r2, r5        \n\
  mov r4, 0             \n\
  cmplt r5, r1, r2      \n\
  bz r5, __mul_L1       \n\
  mov r5, r1            \n\
  mov r1, r2            \n\
  mov r2, r5            \n\
__mul_L1:               \n\
  and r5, r2, 1         \n\
  bz r5, __mul_L2       \n\
  add r4, r4, r1        \n\
__mul_L2:               \n\
  add r1, r1, r1        \n\
  shr r2, r2, 1         \n\
  bnz+ r2, __mul_L1     \n\
  cmplt r3, r3, 0       \n\
  neg r3, r3            \n\
  xor r1, r4, r3        \n\
  sub r1, r1, r3        \n\
  ret                   \n\
");
}

static void signed_divmod(int n, int d, int *qp, int *rp) {
  int is_neg = (n ^ d) < 0;
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
  *qp = is_neg ? -q : q;
  *rp = is_neg ? -r : r;
}

static void unsigned_divmod(unsigned n, unsigned d, unsigned *qp, unsigned *rp) {
  int i;
  unsigned q = 0, r = 0;
  for (i = 31; i >= 0; --i) {
    r <<= 1;
    r += (n >> i) & 1;
    if (r >= d) {
      q += 1 << i;
      r -= d;
    }
  }
  *qp = q;
  *rp = r;
}

int __signed_div(int n, int d) {
  int p, q;
  signed_divmod(n, d, &p, &q);
  return p;
}

int __signed_mod(int n, int d) {
  int p, q;
  signed_divmod(n, d, &p, &q);
  return q;
}

unsigned __unsigned_div(unsigned n, unsigned d) {
  unsigned p, q;
  unsigned_divmod(n, d, &p, &q);
  return p;
}

unsigned __unsigned_mod(unsigned n, unsigned d) {
  unsigned p, q;
  unsigned_divmod(n, d, &p, &q);
  return q;
}


float __fdiv(float a, float b) {
  __asm("\
  mov     r1, [rbp + 8]   \n\
  mov     r6, [rbp + 4]   \n\
  shr     r2, r1, 23      \n\
  mov     r3, 254         \n\
  sub     r2, r3, r2      \n\
  shl     r2, r2, 23      \n\
  mov     r3, 1.0         \n\
  shl     r1, r1, 9       \n\
  shr     r1, r1, 9       \n\
  or      r1, r1, r3      \n\
  mov     r3, -0.47058823 \n\
  mov     r4, 1.4117647   \n\
  mov     r5, 2.0         \n\
  fmul    r3, r1, r3      \n\
  fadd    r3, r3, r4      \n\
  fmul    r4, r1, r3      \n\
  fsub    r4, r5, r4      \n\
  fmul    r3, r3, r4      \n\
  fmul    r4, r1, r3      \n\
  fsub    r4, r5, r4      \n\
  fmul    r3, r3, r4      \n\
  fmul    r4, r1, r3      \n\
  fsub    r4, r5, r4      \n\
  fmul    r3, r3, r4      \n\
  fmul    r1, r2, r3      \n\
  fmul    r1, r1, r6      \n\
  ret\n");
}
