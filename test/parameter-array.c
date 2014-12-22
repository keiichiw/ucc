/*
hello
world
hello
world
hello
world
apple
orange
grape
*/

#include "test.h"

void f1 (char s[][6]) {
  printf("%s\n", s[0]);
  printf("%s\n", s[1]);
}
void f2 (char s[2][6]) {
  printf("%s\n", s[0]);
  printf("%s\n", s[1]);
}
void g (char (*s)[6]) {
  printf("%s\n", s[0]);
  printf("%s\n", s[1]);
}
void h (char *s[], int n) {
  int i;
  for (i=0; i<n; ++i) {
    printf("%s\n", s[i]);
  }
}

int main () {
  char s[2][6] = {"hello","world"};
  char* p[3] = {"apple", "orange", "grape"};
  f1(s);
  f2(s);
  g(s);
  h(p, 3);
  return 0;
}
