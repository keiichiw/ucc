#ifndef _UCC_STDARG_H
#define _UCC_STDARG_H

#define va_arg(ap, type)   (*((type *)(ap = (type *)ap + 1) - 1))
#define va_start(ap, arg)  (ap = &(arg) + 1)
#define va_end(ap)         ((void) 0)
typedef void *va_list;

#endif  /* stdarg.h */
