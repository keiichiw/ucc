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


void _putc (char c) {
  asm_write(c);
}

void _printint(int xx, int base, int sgn) {
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
