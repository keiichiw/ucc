
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>



/* assert.h */

void __assert_fail(const char *expr, const char *file, int line)
{
  printf("%s:%d: Assertion `%s' failed.\n", file, line, expr);
  abort();
}



/* ctype.h */

int isalnum(int c) { return isalpha(c) || isdigit(c); }
int isalpha(int c) { return islower(c) || isupper(c); }
int isdigit(int c) { return '0' <= c && c <= '9'; }
int islower(int c) { return 'a' <= c && c <= 'z'; }
int isspace(int c) { return c == ' ' || ('\t' <= c && c <= '\r'); }
int isupper(int c) { return 'A' <= c && c <= 'Z'; }
int tolower(int c) { return isupper(c) ? c - 'A' + 'a' : c; }
int toupper(int c) { return islower(c) ? c - 'a' + 'A' : c; }



/* setjmp.h */

int setjmp(jmp_buf buf)
{
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

void longjmp(jmp_buf buf, int val)
{
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



/* stdio.h */

int putchar(int c)
{
  __asm("\
  mov r1, [rbp + 4] \n\
  write r1          \n\
  ret               \n\
");
}

int getchar(void)
{
  __asm("\
    read r1 \n\
    ret     \n\
");
}

static void print_int(int xx, int base, int sgn)
{
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
    putchar(buf[i]);
}

static void print_float (float f, unsigned round)
{
  int num, frac, diff, t;
  float abs = f<0 ? -f : f;
  unsigned i;
  diff = 1;
  for (i=0; i<round; i++) {
    diff *= 10;
  }
  num  = (int) abs;
  frac = (int) ((abs - num) * (float)diff);
  if (f < 0) putchar('-');
  print_int( num, 10, 1);
  putchar('.');
  t = frac;
  i=1;
  while (t>1) {
    t /= 10; ++i;
  }
  while (i<round) {
    putchar('0');++i;
  }
  print_int(frac, 10, 1);
}

int printf(const char *fmt, ...)
{
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
        putchar(c);
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
            putchar(*s);
            s+=1;
          }
        } else if(c == 'c'){
          putchar(*ap);
          ap++;
        } else if(c == '%'){
          putchar(c);
        } else {
          // Unknown % sequence.  Print it to draw attention.
          putchar('%');
          putchar(c);
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
        putchar('@');
        putchar('.');
        print_int(round, 10, 0);
        putchar(c);
        round = 4;
        state = 0;
      }
    }
  }
  return 0;
}



/* stdlib.h */

void abort(void)
{
  printf("abort!\n");
  __asm("  halt\n");
}

int abs(int n)
{
  __asm("\
  mov r1, [rbp + 4] \n\
  sar r2, r1, 31    \n\
  xor r1, r1, r2    \n\
  sub r1, r1, r2    \n\
  ret               \n\
");
}


static unsigned rbuf[32], ridx = -1;

int rand(void)
{
  if (ridx >= 32) srand(1);
  if (ridx == 31) {
    ridx = 0;
    return (rbuf[31] = rbuf[0] + rbuf[28]) >> 1;
  } else {
    int tmp = rbuf[ridx + 1] + rbuf[ridx + (ridx < 3 ? 29 : -3)];
    return (rbuf[ridx++] = tmp) >> 1;
  }
}

void srand(unsigned seed)
{
  int i;
  unsigned tmp, lo, hi;
  const unsigned mod = 0x7fffffff;
  ridx = 2;
  rbuf[0] = seed ? seed : 1;
  for (i = 1; i < 31; ++i) {
    /* rbuf[i] = (16807ll * (int)rbuf[i - 1] % mod + mod) % mod; */
    tmp = rbuf[i - 1];
    if ((int)tmp < 0) tmp = -tmp;
    lo = 16807 * (tmp & 0xffff);
    hi = 16807 * (tmp >> 16);
    tmp = (lo + ((hi & 0x7fff) << 16) + (hi >> 15));
    if (tmp >= mod) tmp -= mod;
    rbuf[i] = (int)rbuf[i - 1] < 0 && tmp ? mod - tmp : tmp;
  }
  rbuf[31] = rbuf[0]; rbuf[0] = rbuf[1]; rbuf[1] = rbuf[2];
  for (i = 34; i < 344; ++i) rand();
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

void *malloc(size_t nbytes)
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

void free(void *ap)
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

static size_t malloc_size(void *ap)
{
  Header *p;

  p = (Header *)ap - 1;
  return (p->size - 1) * sizeof(Header);
}

void *realloc(void *ptr, size_t size)
{
  void *new;

  if (! ptr)
    return malloc(size);

  if (size <= malloc_size(ptr))
    return ptr;

  new = malloc(size);
  if (! new)
    return NULL;

  memcpy(new, ptr, malloc_size(ptr));
  free(ptr);

  return new;
}

static void *sbrk(size_t s) {
  extern char __UCC_HEAP_START;
  static char *ptr = NULL;

  if (! ptr)
    ptr = &__UCC_HEAP_START;

  if (ptr + s >= (char *)0x400000)
    return NULL;

  ptr += s;
  return ptr - s;
}

static Header *morecore(size_t nunits)
{
  Header *p;

  nunits = (nunits + NALLOC - 1) / NALLOC * NALLOC;
  p = (Header *)sbrk(nunits * sizeof(Header));
  if (! p)
    return NULL;
  p->size = nunits;
  free(p + 1);
  return freep;
}



/* string.h */

size_t strlen(const char *str)
{
  size_t len = 0;

  while (str[len]) {
    ++len;
  }
  return len;
}

void *memcpy(void *dst, const void *src, size_t n)
{
  const char *s = src;
  char *d = dst;

  while (n-- > 0) {
    *d++ = *s++;
  }
  return d;
}

