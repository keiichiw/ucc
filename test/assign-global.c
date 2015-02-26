/*
A 42
{1, f}
{b, a, r, {2, g}}
A 84
{1, f}
{b, a, r, {2, g}}
B 126
{1, f}
{b, a, r, {2, g}}
*/

#include <stdio.h>

char g_char;
int g_int;

struct foo {
  int a;
  char b;
} g_foo;

struct bar {
  char c, d, e;
  struct foo f;
} g_bar;


int main() {
  char c = 'A';
  int  a = 42;
  struct foo f = { 1, 'f'};
  struct bar b = {'b', 'a', 'r', {2, 'g'}};

  g_char = c;
  g_int  = a;

  g_foo  = f;
  g_bar  = b;

  printf("%c %d\n", g_char, g_int);
  printf("{%d, %c}\n", g_foo.a, g_foo.b);
  printf("{%c, %c, %c, {%d, %c}}\n",
         g_bar.c, g_bar.d, g_bar.e, g_bar.f.a, g_bar.f.b);


  g_char = g_char;
  g_int  += g_int;
  g_foo = g_foo;
  g_bar = g_bar;

  printf("%c %d\n", g_char, g_int);
  printf("{%d, %c}\n", g_foo.a, g_foo.b);
  printf("{%c, %c, %c, {%d, %c}}\n",
         g_bar.c, g_bar.d, g_bar.e, g_bar.f.a, g_bar.f.b);


  c = g_char + 1;
  a += g_int;

  f = g_foo;
  b = g_bar;

  printf("%c %d\n", c, a);
  printf("{%d, %c}\n", f.a, f.b);
  printf("{%c, %c, %c, {%d, %c}}\n",
         b.c, b.d, b.e, b.f.a, b.f.b);


  return 0;

}
