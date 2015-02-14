
#define NULL 0
typedef unsigned size_t;
typedef int ptrdiff_t;


/*
  Arithmetic
*/

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


/*
  stdio.h
*/

int _putchar(int c) {
  __asm("\
  mov r1, [rbp + 4] \n\
  write r1          \n\
  ret               \n\
");
}

int _getchar(void) {
  __asm("\
    read r1 \n\
    ret     \n\
");
}

static void print_int(int xx, int base, int sgn) {
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
    _putchar(buf[i]);
}

static void print_float (float f, unsigned round) {
  int num, frac, diff, t;
  float abs = f<0 ? -f : f;
  unsigned i;
  diff = 1;
  for (i=0; i<round; i++) {
    diff *= 10;
  }
  num  = (int) abs;
  frac = (int) ((abs - num) * (float)diff);
  if (f < 0) _putchar('-');
  print_int( num, 10, 1);
  _putchar('.');
  t = frac;
  i=1;
  while (t>1) {
    t /= 10; ++i;
  }
  while (i<round) {
    _putchar('0');++i;
  }
  print_int(frac, 10, 1);
}


int _printf(const char *fmt, ...) {
  // Print to the given fd. Only understands %d, %x, %f, %s.
  char *s;
  int c, i, state;
  unsigned* ap;
  unsigned round = 4;
  state = 0;
  ap = (unsigned*)(void*)&fmt + 1;
  for (i = 0; fmt[i]; i++){
    c = fmt[i] & 0xff;
    if (state == 0) {
      if (c == '%') {
        state = '%';
      } else {
        _putchar(c);
      }
    } else if (state == '%'){
      if (c == '.') {
        state = '.';
        round = 0;
      } else {
        if (c == 'd'){
          print_int(*ap, 10, 1);
          ap++;
        } else if (c == 'f') {
          union {float f;unsigned u;} t;
          t.u = *ap;
          print_float(t.f, round);
          ap++;
        } else if(c == 'x' || c == 'p'){
          print_int(*ap, 16, 0);
          ap++;
        } else if(c == 's'){
          s = (char*)*ap;
          ap++;
          if(s == 0)
            s = "(null)";
          while(*s != 0){
            _putchar(*s);
            s+=1;
          }
        } else if(c == 'c'){
          _putchar(*ap);
          ap++;
        } else if(c == '%'){
          _putchar(c);
        } else {
          // Unknown % sequence.  Print it to draw attention.
          _putchar('%');
          _putchar(c);
        }
        state = 0;
      }
    } else if (state == '.') {
      if ('0'<=c && c<='9') {
        round = round*10 + (c-'0');
      } else if (c == 'f') {
        union {float f;unsigned u;} t;
        t.u = *ap;
        print_float(t.f, round);
        ap++;
        round = 4;
        state = 0;
      } else {
        // Unknown %. sequence.  Print it to draw attention.
        _putchar('@');
        _putchar('.');
        print_int(round, 10, 0);
        _putchar(c);
        round = 4;
        state = 0;
      }
    }
  }
  return 0;
}


/*
  setjmp.h
*/

typedef int jmp_buf[4];

int _setjmp (jmp_buf buf) {
  __asm("\
  mov r1, [rbp + 4]     # r1 <- buf                    \n\
  mov r2, [rbp]         # r2 <- caller rbp             \n\
  mov [r1], r2          # save caller rbp              \n\
  mov [r1 + 4], r28     # save r28 (return address)    \n\
  mov [r1 + 8], rbp     # save current rbp             \n\
  jl  r2, 0             # get PC                       \n\
  add r2, r2, 12        # r2 <- address of 'ret'       \n\
  mov [r1 + 12], r2     # save r2                      \n\
  mov r1, 0             # return 0                     \n\
  ret                                                  \n\
");
}

void _longjmp (jmp_buf buf, int val) {
  __asm("\
  mov r1, [rbp + 8]     # r1 <- val                   \n\
  mov r2, [rbp + 4]     # r2 <- buf                   \n\
  mov r3, [r2]          # r3 <- caller rbp            \n\
  mov r28, [r2 + 4]     # restore r28                 \n\
  mov rbp, [r2 + 8]     # restore setjmp rbp          \n\
  mov [rbp], r3         # restore setjmp's caller rbp \n\
  mov r2, [r2 + 12]     # r2 <- address of 'ret'      \n\
  jr  r2                # jump                        \n\
");
}


/*
  string.h
*/

size_t
_strlen(const char *str)
{
  size_t len = 0;

  while (str[len]) {
    ++len;
  }
  return len;
}

void *
_memcpy(void *dst, const void *src, size_t n)
{
  const char *s = src;
  char *d = dst;

  while (n-- > 0) {
    *d++ = *s++;
  }
  return d;
}


/*
  stdlib.h
*/

void _abort (void) {
  _printf("abort!\n");
  __asm("  halt\n");
}

int _abs(int n) {
  __asm("\
  mov r1, [rbp + 4] \n\
  sar r2, r1, 31    \n\
  xor r1, r1, r2    \n\
  sub r1, r1, r2    \n\
  ret               \n\
");
}


#define NALLOC 1024

struct header {
  struct header *next;
  size_t size;
};

typedef struct header Header;

static Header base;
static Header *freep;

static Header *morecore(size_t);

void *
_malloc(size_t nbytes)
{
  Header *p, *prevp;
  size_t nunits;

  nunits = (nbytes + sizeof(Header) - 1) / sizeof(Header) + 1;
  if (! (prevp = freep)) {  /* no free list yet */
    base.next = freep = prevp = &base;
    base.size = 0;
  }
  for (p = prevp->next; ; prevp = p, p = p->next) {
    if (p->size >= nunits) {  /* big enough */
      if (p->size == nunits) {  /* exactly */
        prevp->next = p->next;
      } else {  /* allocate tail end */
        p->size -= nunits;
        p += p->size;
        p->size = nunits;
      }
      freep = prevp;
      return (void *)(p + 1);
    }
    if (p == freep)
      if (! (p = morecore(nunits)))
        return NULL;
  }
}

void
_free(void *ap)
{
  Header *bp, *p;

  if (! ap)
    return;

  bp = (Header *)ap - 1;
  for (p = freep; !(p < bp && bp < p->next); p = p->next)
    if (p->next <= p && (p < bp || bp < p->next))
      break;

  if (bp + bp->size == p->next) {
    bp->size += p->next->size;
    bp->next = p->next->next;
  } else {
    bp->next = p->next;
  }

  if (p + p->size == bp) {
    p->size += bp->size;
    p->next = bp->next;
  } else {
    p->next = bp;
  }

  freep = p;
}

static size_t
malloc_size(void *ap)
{
  Header *p;

  p = (Header *)ap - 1;
  return (p->size - 1) * sizeof(Header);
}

void *
_realloc(void *ptr, size_t size)
{
  void *new;

  if (! ptr)
    return _malloc(size);

  if (size <= malloc_size(ptr))
    return ptr;

  new = _malloc(size);
  if (! new)
    return NULL;

  _memcpy(new, ptr, malloc_size(ptr));
  _free(ptr);

  return new;
}

static void *
sbrk(size_t s) {
  extern char __UCC_HEAP_START;
  static char *ptr = NULL;

  if (! ptr)
    ptr = &__UCC_HEAP_START;

  if (ptr + s >= (char *)0x400000)
    return NULL;

  ptr += s;
  return ptr - s;
}

static Header *
morecore(size_t nunits)
{
  Header *p;

  nunits = (nunits + NALLOC - 1) / NALLOC * NALLOC;
  p = (Header *)sbrk(nunits * sizeof(Header));
  if (! p)
    return NULL;
  p->size = nunits;
  _free(p + 1);
  return freep;
}


static unsigned rbuf[32], ridx = -1;

void _srand(unsigned);

int _rand(void)
{
  if (ridx >= 32) _srand(1);
  if (ridx == 31) {
    ridx = 0;
    return (rbuf[31] = rbuf[0] + rbuf[28]) >> 1;
  } else {
    int tmp = rbuf[ridx + 1] + rbuf[ridx + (ridx < 3 ? 29 : -3)];
    return (rbuf[ridx++] = tmp) >> 1;
  }
}

void _srand(unsigned seed)
{
  int i;
  unsigned tmp, lo, hi;
  const unsigned mod = 0x7fffffff;
  ridx = 2;
  rbuf[0] = seed ? seed : 1;
  for (i = 1; i < 31; ++i) {
    // rbuf[i] = (16807ll * (int)rbuf[i - 1] % mod + mod) % mod;
    tmp = rbuf[i - 1];
    if ((int)tmp < 0) tmp = -tmp;
    lo = 16807 * (tmp & 0xffff);
    hi = 16807 * (tmp >> 16);
    tmp = (lo + ((hi & 0x7fff) << 16) + (hi >> 15));
    if (tmp >= mod) tmp -= mod;
    rbuf[i] = (int)rbuf[i - 1] < 0 && tmp ? mod - tmp : tmp;
  }
  rbuf[31] = rbuf[0]; rbuf[0] = rbuf[1]; rbuf[1] = rbuf[2];
  for (i = 34; i < 344; ++i) _rand();
}


/*
  assert.h
*/

void _assert_fail(const char *expr, const char *file, int line)
{
  _printf("%s:%d: Assertion `%s' failed.\n", file, line, expr);
  _abort();
}


/*
  ctype.h
*/

int _isdigit(int c) { return '0' <= c && c <= '9'; }
int _islower(int c) { return 'a' <= c && c <= 'z'; }
int _isupper(int c) { return 'A' <= c && c <= 'Z'; }
int _isalpha(int c) { return _islower(c) || _isupper(c); }
int _isalnum(int c) { return _isalpha(c) || _isdigit(c); }
int _isspace(int c) { return c == ' ' || ('\t' <= c && c <= '\r'); }
int _tolower(int c) { return _isupper(c) ? c - 'A' + 'a' : c; }
int _toupper(int c) { return _islower(c) ? c - 'a' + 'A' : c; }

float __pow_fi(float a,int n){
  // return a^x
  float r = 1.0;
  while(n-- > 0) r *= a;
  return r;
}

/*
 * math.h
 */
/*
 * sqrt
 * fabs
 *
 */
float _sqrt(float x){
  int i;
  float xn;

  xn = x/128.0;

  for(i=0; i<10; i++){
    xn = (xn + x/xn)/2.0;
  }
  return xn;
}
float _fabs(float x){
  return x < 0 ? -x : x;
}

/*
 * kernel sin
 * kernel cos
 *
 * x is in range [-pi/4, pi/4]
 *
 */
float __kernel_sin(float x){
  float a1, a2, a3, a4;

  a1 = 0.1666667;
  a2 = 0.0500000;
  a3 = 0.0238095;
  a4 = 0.0138888;

  return x*(1-a1*x*x*(1-a2*x*x*(1-a3*x*x*(1-a4*x*x))));
}
float __kernel_cos(float x){
  float a1, a2, a3, a4;

  a1 = 0.5000000;
  a2 = 0.0833333;
  a3 = 0.0333333;
  a4 = 0.0178571;

  return (1-a1*x*x*(1-a2*x*x*(1-a3*x*x*(1-a4*x*x))));
}

/*
 * __frem_for_triangle
 *
 * i is a quotient, x = pi/4 * i + (return value)
 *
 */
float __frem_for_triangle(float x, int *i){
  float pi4;

  *i=0;
  pi4 = 0.7853981; // const value

  while(x - (*i)*pi4 >= 0) (*i) += 1;
  *i -= 1;

  return (x - (*i)*pi4);
}

/*
 * sin
 *   sin(x) = -sin(-x)
 * cos
 *   cos(x) = cos(-x)
 * tan
 *   tan(x) = tan(x + pi/2)
 */
float _sin(float x){
  int i, neg;
  float y;

  neg = x > 0 ? 1 : -1;
  x   = x > 0 ? x : -x;
  y   = __frem_for_triangle(x, &i);

  if(i%8 >= 4 && neg == 1) neg = -1;
  else if(i%8 >= 4 && neg == -1) neg = 1;

  switch(i%4){
    case 0:
      return neg * __kernel_sin(y);
    case 1:
      return neg * __kernel_cos(0.7853981 - y);
    case 2:
      return neg * __kernel_cos(y);
    default:
      return neg * __kernel_sin(0.7853981 - y);
  }
}
float _cos(float x){
  int i;
  float y;

  x = x > 0 ? x : -x;
  y = __frem_for_triangle(x, &i);

  switch(i%8){
    case 0:
      return __kernel_cos(y);
    case 1:
      return __kernel_sin(0.7853981 - y);
    case 2:
      return - __kernel_sin(y);
    case 3:
      return - __kernel_cos(0.7853981 - y);
    case 4:
      return - __kernel_cos(y);
    case 5:
      return - __kernel_sin(0.7853981 - y);
    case 6:
      return __kernel_sin(y);
    default:
      return __kernel_cos(0.7853981 - y);
  }
}
float _tan(float x){
  return _sin(x)/_cos(x);
}

float _arctan_sub(float x){
  return x*(1.0 - 0.33333333*x*x*(1.0 - 0.60*x*x*(1.0 - 2.1428571*x*x)));
}

/*
 * arctan
 *
 * arctan(x) = - arctan(-x)
 *
 * Algorithm:
 *  0 <= x <= sqrt(2) - 1:
 *    x -  1/3 x^3 + 1/5 x^5 + ...
 *  sqrt(2) - 1 < x < 1
 *    pi/4 - arctan((1-x)/(1+x))
 *  1 <= x < 1 + sqrt(2)
 *    z = 1/x
 *    arctan(x) = pi/2 - arctan(z)
 *  1 + sqrt(2) <= x
 *    z = 1/x
 *    arctan(x) = pi/2 - arctan(z)
 *
 */
float _arctan(float x){
  int sign;

  sign = x >= 0 ? 1 : -1;
  x    = x >= 0 ? x : -x;

  if(0.0 <= x && x <= 0.41421356){
    return sign * _arctan_sub(x);
  }else if(0.41421356 < x && x <= 1.0){
    float tmp;
    tmp = (1.0 - x) / (1.0 + x);
    return sign * (0.7853981 - _arctan_sub(tmp));
  }else if(1.0 <= x && x <= 2.41421356){
    float tmp;
    tmp = (x - 1.0)/(x + 1.0);
    return sign * (0.7853981 + _arctan_sub(tmp));
  }else{
    float tmp;
    tmp = 1.0/x;
    return sign * (1.5707963 - _arctan_sub(tmp));
  }
}
float _arcsin(float x){
  if(x < -1.0 || x > 1.0) return -1024; // error
  if(x == -1.0) return -1.5707963;
  if(x == 1.0) return 1.5707963;

  if(x > 0.816){
    return _arctan(x/_sqrt((1+x)*(1-x)));
  }else{
    return _arctan(x/_sqrt(1-x*x));
  }
}
float _arccos(float x){
  if(x < -1.0 || x > 1.0) return -1024; // error
  if(x == -1.0) return 3.14159265;
  if(x == 1.0) return 0.0;

  if(x > 0.816){
    return 1.5707963 - _arctan(x/_sqrt((1+x)*(1-x)));
  }else{
    return 1.5707963 - _arctan(x/_sqrt(1-x*x));
  }
}

float _exp_sub(float x){
  int i, j;
  float res, a;

  res = 0;
  for(i=0; i<10; i++){
    a=1.0;
    for(j=1; j<=i; j++){
      a *= x/(float)j;
    }
    res += a;
  }
  return res;
}
float __frem_for_exp(float x, int *i){
  float log2;

  *i=0;
  log2 = 0.693147180; // const value

  while(x - (*i)*log2 >= 0) (*i) += 1;
  *i -= 1;

  return (x - (*i)*log2);
}

/*
 * exp
 *
 * Algorithm:
 *  0 <= x < log_e(2):
 *    1 + x + x^2/2 + ...
 *  log_e(2) <= x
 *    x = m*log_e(2) + z
 *    e^x = e^z * 2^m
 *  x < 0
 *    z = -x
 *    exp(x) = 1.0/exp(z)
 *
 */
float _exp(float x){
  if(x < 0){
    return 1.0/_exp(-x);
  }else if(x < 0.693147180){
    return _exp_sub(x);
  }else{
    int d, m;
    float z;

    z = __frem_for_exp(x, &m);

    d = 1;
    while(m-- > 0) d *= 2;

    return d * _exp_sub(z);
  }
}

/*
 * log(log_e)
 *   log(x) = -1 if x <= 0
 *   generally, return nan (x < 0) and -inf (x = 0)
 *
 *  Algorithm:
 *    x = 2^m * z (1 <= z < 2)
 *    y = (z-1)/(z+1)
 *    log((1+x)/(1-x)) = 2(x + x^3/3 + ...)
 *     -> log z = log((1+y)/(1-y))
 *    log x = log z + m*log(2)
 *
 * pow
 *   use exp and log
 *   a^x = exp(x * log_e(a))
 */
float _log(float x){
  int i, m;
  float z, y, t;

  if(x <= 0)
    return -1;

  m = 0;
  z = x/__pow_fi(2.0, m);
  while(2.0 <= z){
    m++;
    z = x/__pow_fi(2.0, m);
  }

  y = (z-1)/(z+1);
  t = 0;

  for(i=0; i<10; i++){
    t += __pow_fi(y, 2*i+1)/(2*i + 1);
  }

  return (2.0*t + m*0.693147180);
}
float _pow(float a, float x){
  return _exp(x * _log(a));
}

/*
 * hyperbolic function
 *
 */
float _sinh(float x){
  return (_exp(x) - _exp(-x))/2.0;
}
float _cosh(float x){
  return (_exp(x) + _exp(-x))/2.0;
}
float _tanh(float x){
  return _sinh(x)/_cosh(x);
}

/*
 * floor
 */

float _floor(float x) {
  __asm("\
  mov r1, [rbp + 4]     \n                      \
  floor r1, r1          \n                      \
  ret                   \n                      \
");
}
