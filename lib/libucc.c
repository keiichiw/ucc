
#define NULL 0
typedef unsigned size_t;
typedef int ptrdiff_t;


/*
  Arithmetic
*/

int __mul(int a, int b) {
  int i, r;
  r = 0;
  for (i = 31; i >= 0; --i) {
    r <<= 1;
    if ((b >> i) & 1) r += a;
  }
  return r;
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


union header {
  struct {
    union header *ptr;
    size_t size;
  } s;
};

typedef union header Header;

static Header base;
static Header *freep;

static Header * _morecore(size_t);

void *
_malloc(size_t nbytes)
{
  Header *p, *prevp;
  size_t nunits;

  nunits = (nbytes + sizeof(Header) - 1) / sizeof(Header) + 1;
  if ((prevp = freep) == NULL) {  /* no free list yet */
    base.s.ptr = freep = prevp = &base;
    base.s.size = 0;
  }
  for (p = prevp->s.ptr; 1; prevp = p, p = p->s.ptr) {
    if (p->s.size >= nunits) {  /* big enough */
      if (p->s.size == nunits)  /* exactly */
        prevp->s.ptr = p->s.ptr;
      else {  /* allocate tail end */
        p->s.size -= nunits;
        p += p->s.size;
        p->s.size = nunits;
      }
      freep = prevp;
      return (void *)(p + 1);
    }
    if (p == freep)
      if ((p = _morecore(nunits)) == NULL)
        return NULL;
  }
}

void
_free(void *ap)
{
  Header *bp, *p;

  bp = (Header *)ap - 1;
  for (p = freep; !(bp > p && bp < p->s.ptr); p = p->s.ptr)
    if (p >= p->s.ptr && (bp > p || bp < p->s.ptr))
      break;

  if (bp + bp->s.size == p->s.ptr) {
    bp->s.size += p->s.ptr->s.size;
    bp->s.ptr = p->s.ptr->s.ptr;
  } else
    bp->s.ptr = p->s.ptr;
  if (p + p->s.size == bp) {
    p->s.size += bp->s.size;
    p->s.ptr = bp->s.ptr;
  } else
    p->s.ptr = bp;
  freep = p;
}

static size_t
_malloc_size(void *ap)
{
  Header *p;

  p = (Header *)ap - 1;
  return p->s.size * sizeof(Header);
}

void *
_realloc(void *ptr, size_t size)
{
  void *new;

  if (! ptr) {
    return _malloc(size);
  } else {
    if (_malloc_size(ptr) < size) {
      new = _malloc(size);
      if (! new) {
        return NULL;
      }

      _memcpy(new, ptr, _malloc_size(ptr));

      _free(ptr);
    } else {
      return ptr;
    }
  }

  return new;
}

static void *
_sbrk(size_t s) {
  extern char __UCC_HEAP_START;
  static char *ptr = 0;

  if (! ptr) {
    ptr = &__UCC_HEAP_START;
  }

  ptr += s;
  return ptr - s;
}

#define NALLOC 1024 * 4

static Header *
_morecore(size_t nu)
{
  char *cp;
  Header *up;

  if (nu < NALLOC)
    nu = NALLOC;
  cp = _sbrk((nu + 1) * sizeof (Header));
  if (cp == (char *)-1)
    return NULL;
  up = (Header *)cp;
  up->s.size = nu;
  _free((char *)(up + 1));
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
