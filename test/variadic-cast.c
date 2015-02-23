/*
apple orange grape
Alice Bob Carol Dave Eva
*/
#include <stdio.h>
#include <stdarg.h>
void print_words(int num, ...) {
  va_list ap;
  int i;
  char *s;

  va_start(ap, num);

  for (i = 0; i < num; ++i) {
    s = va_arg(ap, char *);
    printf("%s", s);
    if (i != num-1)
      putchar(' ');
  }

  putchar('\n');

  va_end(ap);

}

int main() {
  double d = 5.0;

  print_words(3, "apple", "orange", "grape");

  /* implicit conversion */
  print_words(d, "Alice", "Bob", "Carol", "Dave", "Eva");

  return 0;
}
