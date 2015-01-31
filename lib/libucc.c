#include "intrinsics.h"

int __mul(int a, int b) {
  int i, r;
  r = 0;
  for (i = 31; i >= 0; --i) {
    r <<= 1;
    if ((b >> i) & 1) r += a;
  }
  return r;
}

void _signed_divmod(int n, int d, int *qp, int *rp) {
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

void _unsigned_divmod(unsigned n, unsigned d, unsigned *qp, unsigned *rp) {
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
  _signed_divmod(n, d, &p, &q);
  return p;
}

int __signed_mod(int n, int d) {
  int p, q;
  _signed_divmod(n, d, &p, &q);
  return q;
}

unsigned __unsigned_div(unsigned n, unsigned d) {
  unsigned p, q;
  _unsigned_divmod(n, d, &p, &q);
  return p;
}

unsigned __unsigned_mod(unsigned n, unsigned d) {
  unsigned p, q;
  _unsigned_divmod(n, d, &p, &q);
  return q;
}


void _putc(char c) {
  __gaia_write(c);
}

char _getc() {
  return __gaia_read();
}

static void _printint(int xx, int base, int sgn) {
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
    _putc(buf[i]);
}


// Print to the given fd. Only understands %d, %x, %p, %s.
int _printf(char *fmt) {
  char *s;
  int c, i, state;
  unsigned *ap;

  state = 0;
  ap = (unsigned*)(void*)&fmt + 1;
  for(i = 0; fmt[i]; i++){
    c = fmt[i] & 0xff;
    if(state == 0){
      if(c == '%'){
        state = '%';
      } else {
        _putc(c);
      }
    } else if(state == '%'){
      if(c == 'd'){
        _printint(*ap, 10, 1);
        ap++;
      } else if(c == 'x' || c == 'p'){
        _printint(*ap, 16, 0);
        ap++;
      } else if(c == 's'){
        s = (char*)*ap;
        ap++;
        if(s == 0)
          s = "(null)";
        while(*s != 0){
          _putc(*s);
          s+=1;
        }
      } else if(c == 'c'){
        _putc(*ap);
        ap++;
      } else if(c == '%'){
        _putc(c);
      } else {
        // Unknown % sequence.  Print it to draw attention.
        _putc('%');
        _putc(c);
      }
      state = 0;
    }
  }
  return 0;
}

void _abort () {
  while (1) {
    _printf("abort!\n");
  }
}

typedef int jmp_buf[4];

int setjmp (jmp_buf buf) {
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

void longjmp (jmp_buf buf, int val) {
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
