
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

#define max(m, n)   ((m) > (n) ? (m) : (n))
#define min(m, n)   ((m) < (n) ? (m) : (n))



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

static FILE stdin_entity  = { getchar, NULL };
static FILE stdout_entity = { NULL, putchar };

FILE *stdin  = &stdin_entity;
FILE *stdout = &stdout_entity;
FILE *stderr = &stdout_entity;

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

int putc(int c, FILE *stream)
{
  return fputc(c, stream);
}

int fputc(int c, FILE *stream)
{
  if (! stream->write)
    return EOF;
  return stream->write(c);
}

int getc(FILE *stream)
{
  return fgetc(stream);
}

int fgetc(FILE *stream)
{
  if (! stream->read)
    return EOF;
  return stream->read();
}

int fputs(char *s, FILE *stream)
{

  while(*s != '\0') {

    if (fputc(*s, stream) == EOF)
      return EOF;

    ++s;

  }

  return 1;

}


int puts(char *s)
{
  int i = 0;

  while(*s != '\0') {

    if (putchar(*s) == EOF)
      return EOF;

    ++i; ++s;

  }

  putchar('\n');

  return i + 1;
}



char *fgets(char *s, int size, FILE *stream)
{
  int c;
  char *buf;
  buf = s;

  if (size == 0)
    return NULL;

  while (--size > 0 && (c = fgetc(stream)) != EOF) {

    if ((*buf++ = c) == '\n')
      break;

  }

  *buf = '\0';

  return (c == EOF && buf == s) ? NULL : s;

}

char *gets(char *s)
{
  int c;
  char *buf;
  buf = s;

  while ((c = getc(stdin)) != EOF && c != '\n') {

    *buf++ = c;

  }

  *buf = '\0';

  return (c == EOF && buf == s) ? NULL : s;

}

#define LEFT      (1 << 0)
#define ZEROPAD   (1 << 1)
#define SIGN      (1 << 2)
#define PLUS      (1 << 3)
#define SPACE     (1 << 4)
#define OCTPRE    (1 << 5)
#define HEXPRE    (1 << 6)
#define CAP       (1 << 7)
#define DOT       (1 << 8)
#define TRUNC     (1 << 9)

static int prints(FILE *fp, const char *str, int w, int prec, int flag)
{
  int i, len;

  if (! str)
    str = "(null)";

  len = strlen(str);;
  if (prec >= 0 && len > prec)
    len = prec;

  if (! (flag & LEFT))
    for (i = 0; i < w - len; ++i)
      fp->write(' ');

  for (i = 0; i < len; ++i)
    fp->write(str[i]);

  if (flag & LEFT)
    for (i = 0; i < w - len; ++i)
      fp->write(' ');

  return max(len, w);
}

static int printi(FILE *fp, int x, int base, int w, int prec, int flag)
{
  char buf[12], pbuf[2];
  int i, n, len = 0, plen = 0;
  unsigned u = x;

  if (prec >= 0)
    flag &= ~ZEROPAD;

  if (flag & SIGN) {
    if (x < 0) {
      u = -x;
      pbuf[plen++] = '-';
    } else if (flag & (PLUS | SPACE)) {
      pbuf[plen++] = flag & PLUS ? '+' : ' ';
    }
  }
  if (x && flag & HEXPRE) {
    pbuf[plen++] = '0';
    pbuf[plen++] = flag & CAP ? 'X' : 'x';
  }

  while (u > 0) {
    int t = u % base;
    buf[len++] = (t < 10) ? (t + '0') : (t - 10 + (flag & CAP ? 'A' : 'a'));
    u /= base;
  }
  if (x == 0 || flag & OCTPRE)
    buf[len++] = '0';

  n = max(len, prec) + plen;

  if (! (flag & (LEFT | ZEROPAD)))
    for (i = 0; i < w - n; ++i)
      fp->write(' ');

  for (i = 0; i < plen; ++i)
    fp->write(pbuf[i]);

  if (flag & ZEROPAD)
    for (i = 0; i < w - n; ++i)
      fp->write('0');

  for (i = 0; i < prec - len; ++i)
    fp->write('0');

  for (i = len - 1; i >= 0; --i)
    fp->write(buf[i]);

  if (flag & LEFT)
    for (i = 0; i < w - n; ++i)
      fp->write(' ');

  return max(w, n);
}

static float normalize(float f, int *e)
{
  static float table_p[] = { 1e1, 1e2, 1e4, 1e8, 1e16, 1e32 };
  static float table_m[] = { 1e-1, 1e-2, 1e-4, 1e-8, 1e-16, 1e-32 };
  static float table_m1[] = { 1e0, 1e-1, 1e-3, 1e-7, 1e-15, 1e-31 };

  int i;

  *e = 0;
  for (i = 5; i >= 0; --i) {
    if (f >= table_p[i]) {
      *e += 1 << i;
      f *= table_m[i];
    }
    if (f < table_m1[i]) {
      *e -= 1 << i;
      f *= table_p[i];
    }
  }

  return f;
}

static int printef(FILE *fp, float f, int w, int prec, int flag)
{
  char buf[10], pbuf[1];
  int i, n, expo = 0, len = 0, plen = 0, sp = 0;
  union { float f; unsigned i; } u;

  u.f = f;

  if (prec < 0)
    prec = 6;

  if (u.i >> 31) {
    f = -f;
    pbuf[plen++] = '-';
  } else if (flag & (PLUS | SPACE)) {
    pbuf[plen++] = flag & PLUS ? '+' : ' ';
  }

  if ((u.i >> 23 & 255) == 255) {
    int ofs = flag & CAP ? 'A' - 'a' : 0;
    sp = 1;
    flag &= ~ZEROPAD;
    if ((u.i & 0x7fffff) == 0) {
      buf[len++] = 'i' + ofs;
      buf[len++] = 'n' + ofs;
      buf[len++] = 'f' + ofs;
    } else {
      buf[len++] = 'n' + ofs;
      buf[len++] = 'a' + ofs;
      buf[len++] = 'n' + ofs;
    }
  } else if (f == 0.0) {
    buf[len++] = '0';
    buf[len++] = '.';
    /* prec >= 9 is treated as prec = 8 */
    for (i = 0; i < min(prec, 8); ++i)
      buf[len++] = '0';
  } else {
    int x = 0;
    f = normalize(f, &expo);

    len = min(prec, 8) + 2;
    for (i = 0; i < len - 1; ++i) {
      int d = f;
      x = x * 10 + d;
      f = (f - d) * 10.0;
    }
    if (f >= 5.0) ++x;

    buf[1] = '.';
    for (i = len - 1; i >= 2; --i) {
      buf[i] = x % 10 + '0';
      x /= 10;
    }
    if (x == 10) {
      ++expo;
      buf[0] = '1';
    } else {
      buf[0] = x + '0';
    }
  }

  if (flag & TRUNC)
    while (buf[len - 1] == '0')
      --len;
  if (buf[len - 1] == '.' && ~flag & DOT)
    --len;

  n = len + plen + (sp || flag & TRUNC ? 0 : max(prec - 8, 0)) + (sp ? 0 : 4);

  if (! (flag & (LEFT | ZEROPAD)))
    for (i = 0; i < w - n; ++i)
      fp->write(' ');

  for (i = 0; i < plen; ++i)
    fp->write(pbuf[i]);

  if (flag & ZEROPAD)
    for (i = 0; i < w - n; ++i)
      fp->write('0');

  for (i = 0; i < len; ++i)
    fp->write(buf[i]);

  if (! (sp || flag & TRUNC))
    for (i = 0; i < prec - 8; ++i)
      fp->write('0');

  if (! sp) {
    fp->write(flag & CAP ? 'E' : 'e');
    fp->write(expo < 0 ? '-' : '+');
    if (expo < 0) expo = -expo;
    fp->write(expo / 10 + '0');
    fp->write(expo % 10 + '0');
  }

  if (flag & LEFT)
    for (i = 0; i < w - n; ++i)
      fp->write(' ');

  return max(w, n);
}

static int printff(FILE *fp, float f, int w, int prec, int flag)
{
  static float round_table[] = {
    5e-1, 5e-2, 5e-3, 5e-4, 5e-5, 5e-6, 5e-7, 5e-8, 5e-9, 5e-10
  };

  char buf[49], pbuf[1];
  int i, n, len = 0, plen = 0, sp = 0;
  union { float f; unsigned i; } u;

  u.f = f;

  if (prec < 0)
    prec = 6;

  if (u.i >> 31) {
    f = -f;
    pbuf[plen++] = '-';
  } else if (flag & (PLUS | SPACE)) {
    pbuf[plen++] = flag & PLUS ? '+' : ' ';
  }

  if ((u.i >> 23 & 255) == 255) {
    int ofs = flag & CAP ? 'A' - 'a' : 0;
    sp = 1;
    flag &= ~ZEROPAD;
    if ((u.i & 0x7fffff) == 0) {
      buf[len++] = 'i' + ofs;
      buf[len++] = 'n' + ofs;
      buf[len++] = 'f' + ofs;
    } else {
      buf[len++] = 'n' + ofs;
      buf[len++] = 'a' + ofs;
      buf[len++] = 'n' + ofs;
    }
  } else {
    int expo;
    float g;

    /* prec >= 10 is treated as prec = 9 */
    f += round_table[min(prec, 9)];

    g = normalize(f, &expo);
    if (expo < 0)
      expo = 0;
    else
      f = g;

    for (; expo >= 0; --expo) {
      int d = f;
      buf[len++] = d + '0';
      f = (f - d) * 10.0;
    }

    buf[len++] = '.';
    for (i = 0; i < min(prec, 9); ++i) {
      int d = f;
      buf[len++] = d + '0';
      f = (f - d) * 10.0;
    }
  }

  if (flag & TRUNC)
    while (buf[len - 1] == '0')
      --len;
  if (buf[len - 1] == '.' && ~flag & DOT)
    --len;

  n = len + plen + (sp || flag & TRUNC ? 0 : max(prec - 9, 0));

  if (! (flag & (LEFT | ZEROPAD)))
    for (i = 0; i < w - n; ++i)
      fp->write(' ');

  for (i = 0; i < plen; ++i)
    fp->write(pbuf[i]);

  if (flag & ZEROPAD)
    for (i = 0; i < w - n; ++i)
      fp->write('0');

  for (i = 0; i < len; ++i)
    fp->write(buf[i]);

  if (! (sp || flag & TRUNC))
    for (i = 0; i < prec - 9; ++i)
      fp->write('0');

  if (flag & LEFT)
    for (i = 0; i < w - n; ++i)
      fp->write(' ');

  return max(w, n);
}

static int printgf(FILE *fp, float f, int w, int prec, int flag)
{
  int expo;

  if (prec < 0)
    prec = 6;
  if (prec == 0)
    prec = 1;

  normalize(f, &expo);

  if (-4 <= expo && expo < prec) {
    return printff(fp, f, w, prec - expo - 1, flag);
  } else {
    return printef(fp, f, w, prec - 1, flag);
  }
}

static char *write_string_dst;
static int write_string(int c)
{
  *write_string_dst = c;
  ++write_string_dst;
  return c;
}

int printf(const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  return vfprintf(stdout, fmt, ap);
}

int fprintf(FILE *fp, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  return vfprintf(fp, fmt, ap);
}

int sprintf(char *s, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  return vsprintf(s, fmt, ap);
}

int vprintf(const char *fmt, va_list ap)
{
  return vfprintf(stdout, fmt, ap);
}

int vfprintf(FILE *fp, const char *fmt, va_list ap)
{
  int ret = 0;

  if (! fp->write)
    return -1;

  for (; *fmt; ++fmt) {
    if (*fmt == '%') {
      int flag = 0;
      int alt = 0;
      int w = 0;
      int prec = -1;
      const char *bak = fmt++;
      if (*fmt == '%') {
        ++ret;
        fp->write('%');
        continue;
      }
      while (strchr("-+ #0", *fmt)) {
        switch (*(fmt++)) {
          case '-': flag |= LEFT; break;
          case '+': flag |= PLUS; break;
          case ' ': flag |= SPACE; break;
          case '#': alt = -1; break;
          case '0': flag |= ZEROPAD; break;
        }
      }
      if (flag & LEFT)
        flag &= ~ZEROPAD;
      if (flag & PLUS)
        flag &= ~SPACE;
      if (*fmt == '*') {
        ++fmt;
        w = va_arg(ap, int);
      } else while (isdigit(*fmt)) {
        w = w * 10 + *(fmt++) - '0';
      }
      if (*fmt == '.') {
        ++fmt;
        prec = 0;
        if (*fmt == '*') {
          ++fmt;
          prec = va_arg(ap, int);
        } else while (isdigit(*fmt)) {
          prec = prec * 10 + *(fmt++) - '0';
        }
      }
      while (strchr("hlL", *fmt))
        ++fmt;
      switch (*fmt) {
        case 'c':
          ++ret;
          fp->write(va_arg(ap, int));
          break;
        case 's':
          ret += prints(fp, va_arg(ap, char*), w, prec, flag);
          break;
        case 'd':
        case 'i':
          ret += printi(fp, va_arg(ap, int), 10, w, prec, flag | SIGN);
          break;
        case 'o':
          ret += printi(fp, va_arg(ap, int),  8, w, prec, flag | (alt & OCTPRE));
          break;
        case 'x':
          ret += printi(fp, va_arg(ap, int), 16, w, prec, flag | (alt & HEXPRE));
          break;
        case 'X':
          ret += printi(fp, va_arg(ap, int), 16, w, prec, flag | (alt & HEXPRE) | CAP);
          break;
        case 'u':
          ret += printi(fp, va_arg(ap, int), 10, w, prec, flag);
          break;
        case 'p':
          ret += printi(fp, va_arg(ap, int), 16, w, prec, flag | HEXPRE);
          break;
        case 'e':
          ret += printef(fp, va_arg(ap, double), w, prec, flag | (alt & DOT));
          break;
        case 'E':
          ret += printef(fp, va_arg(ap, double), w, prec, flag | (alt & DOT) | CAP);
          break;
        case 'f':
          ret += printff(fp, va_arg(ap, double), w, prec, flag | (alt & DOT));
          break;
        case 'F':
          ret += printff(fp, va_arg(ap, double), w, prec, flag | (alt & DOT) | CAP);
          break;
        case 'g':
          ret += printgf(fp, va_arg(ap, double), w, prec, flag | (~alt & TRUNC));
          break;
        case 'G':
          ret += printgf(fp, va_arg(ap, double), w, prec, flag | (~alt & TRUNC) | CAP);
          break;
        case 'n':
          *va_arg(ap, int*) = ret;
          break;
        default:
          ret += fmt - bak;
          for (; bak < fmt; ++bak){
            fp->write(*bak);
          }
          --fmt;
          break;
      }
    } else {
      ++ret;
      fp->write(*fmt);
    }
  }

  return ret;
}

int vsprintf(char *s, const char *fmt, va_list ap)
{
  int ret;
  FILE stream = { NULL, write_string };
  write_string_dst = s;
  ret = vfprintf(&stream, fmt, ap);
  *write_string_dst = '\0';
  return ret;
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


long strtol(const char *str, char **end, int base)
{
  long l = 0;

  if (base > 36) {
    if (end)
      *end = (char *)str;
    return 0;
  }

  if (base == 0) {
    if (*str == '0') {
      ++str;
      if (*str == 'x' || *str == 'X') {
        ++str;
        base = 16;
      } else {
        base = 8;
      }
    } else {
      base = 10;
    }
  }

  while (1) {
    char c = *str;
    long n = base;

    if (isdigit(c)) n = c - '0';
    if (islower(c)) n = c - 'a' + 10;
    if (isupper(c)) n = c - 'A' + 10;

    if (base <= n)
      break;

    ++str;
    l = l * base + n;
  }

  if (end)
    *end = (char *)str;
  return l;
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


void *calloc(size_t n, size_t size)
{
  char *ptr = malloc(n * size);

  if (! ptr)
    return NULL;

  memset(ptr, 0, n * size);

  return (void *) ptr;
}


/* string.h */

size_t strlen(const char *str)
{
  size_t len = 0;

  while (*str++) {
    ++len;
  }
  return len;
}

int strcmp(const char *l, const char *r)
{
  while (*l) {
    if (*l++ != *r++) {
      return l[-1] < r[-1] ? -1 : 1;
    }
  }
  return *r ? -1 : 0;
}

char *strchr(const char *str, int c)
{
  do {
    if (*str == c) {
      return (char *)str;
    }
  } while (*str++);
  return NULL;
}

char *strcpy(char *dst, const char *src)
{
  char *d = dst;

  while (*d++ = *src++);
  return dst;
}

void *memset(void *dst, int c, size_t n)
{
  char *d = dst;

  while (n-- > 0) {
    *d++ = c;
  }
  return dst;
}

void *memcpy(void *dst, const void *src, size_t n)
{
  const char *s = src;
  char *d = dst;

  while (n-- > 0) {
    *d++ = *s++;
  }
  return dst;
}
