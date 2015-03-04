/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

int
xf_file_read(void *cookie, char *ptr, int size)
{
  int s = size;
  while (s-- > 0) {
    *ptr++ = getchar();
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

static void
pic_default_abortf()
{
  abort();
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

int
main()
{
  pic_state *pic;
  pic_value expr;

  pic = pic_open(pic_default_allocf, pic_default_abortf, sizeof(jmp_buf), 0, NULL, NULL, xfopen(), xfopen(), xfopen());

  expr = pic_read_cstr(pic, sample_expr);

  pic_printf(pic, "~s\n", pic_eval(pic, expr, pic->lib));

  pic_close(pic);

  return 0;
}
