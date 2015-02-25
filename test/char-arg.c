/*
a b
c d
e f
g h
*/
#include <stdio.h>

int print_chars(char a, char b)
{
  printf("%c %c\n", a, b);

  return 0;
}

int main()
{
  int i;
  char c[4][2] = { "ab", "cd", "ef", "gh"};

  for (i = 0; i < 4; ++i) {
    print_chars(c[i][0], c[i][1]);
  }

}
