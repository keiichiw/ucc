/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/error.h"

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

int
xf_file_read(void *cookie, char *ptr, int size)
{
  int s = size;
  while (s-- > 0) {
    char c = getchar();
    putchar(c);
    *ptr++ = c;
  }
  return size;
}

int
xf_file_write(void *cookie, const char *ptr, int size)
{
  int s = size;
  while (s-- > 0) {
    putchar(*ptr++);
  }
  return size;
}

long
xf_file_seek(void *cookie, long pos, int whence)
{
  return 0;
}

int
xf_file_flush(void *cookie)
{
  return 0;
}

int
xf_file_close(void *cookie)
{
  return 0;
}

xFILE *
xfopen()
{
  xFILE *file;

  file = xfunopen(NULL, xf_file_read, xf_file_write, xf_file_seek, xf_file_flush, xf_file_close);
  if (! file) {
    return NULL;
  }

  return file;
}

static void *
pic_default_allocf(void *ptr, size_t size)
{
  if (size == 0) {
    if (ptr) {
      free(ptr);
    }
    return NULL;
  }
  if (ptr) {
    return realloc(ptr, size);
  } else {
    return malloc(size);
  }
}

static int
pic_default_setjmpf(void *buf)
{
  return setjmp(*(jmp_buf *)buf);
}

static void
pic_default_longjmpf(void *buf, int val)
{
  if (buf == NULL) {
    abort();
  }
  longjmp(*(jmp_buf *)buf, val);
}

/* Simple REPL program */

const char* sample_expr = "\
(letrec ((fact                                  \
          (lambda (n)                           \
            (if (= n 1)                         \
                1                               \
                (* n (fact (- n 1)))))))        \
  (fact 10))                                    \
";

#include <stdio.h>

#define NL "\r\n"

const char *msg =
NL
"  _ _ _ ____ _    ____ ____ _  _ ____    ___ ____" NL
"  | | | |___ |    |    |  | |\\/| |___     |  |  |" NL
"  |_|_| |___ |___ |___ |__| |  | |___     |  |__|" NL
"" NL
"" NL
"      .g8\"\"\"bgd       db      `7MMF\'      db" NL
"    .dP\'     `M      ;MM:       MM       ;MM:" NL
"    dM\'       `     ,V^MM.      MM      ,V^MM." NL
"    MM             ,M  `MM      MM     ,M  `MM" NL
"    MM.    `7MMF\'  AbmmmqMA     MM     AbmmmqMA" NL
"    `Mb.     MM   A'     VML    MM    A\'     VML" NL
"      `\"bmmmdPY .AMA.   .AMMA..JMML..AMA.   .AMMA." NL
NL;

void
sleep()
{
  int n = 0x100;

  while (n--)
    ;
}

void title()
{
  const char *c = msg;
  while (*c) {
    putchar(*c++);
    sleep();
  }
}

int
main()
{
  pic_state *pic;
  pic_value expr;
  xFILE *stderr = xfopen();

  title();

  pic = pic_open(pic_default_allocf, pic_default_setjmpf, pic_default_longjmpf, sizeof(jmp_buf), 0, NULL, NULL, xfopen(), xfopen(), stderr);

  while (1) {
    /* pic_try { */
      pic_printf(pic, "> ");

      expr = pic_read(pic, pic->xSTDIN);

      pic_printf(pic, "\r\n");

      if (pic_eof_p(expr))
        break;

      pic_printf(pic, "~s\r\n", pic_eval(pic, expr, pic->lib));
    /* } */
    /* pic_catch { */
    /*   pic_print_backtrace(pic, stderr); */
    /* } */
  }

  pic_close(pic);

  pic_printf(pic, "halting now...");

  return 0;
}
