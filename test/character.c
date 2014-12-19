/*
32
65
97
48
47
126
0
10
39
92
27
27
25
51
*/
#include "ucc.h"

int main() {
  print_int(' ');
  print_int('A');
  print_int('a');
  print_int('0');
  print_int('/');
  print_int('~');
  print_int('\0');
  print_int('\n');
  print_int('\'');
  print_int('\\');
  print_int('\033');
  print_int('\x1b');
  print_int('z' - 'a');
  print_int('0' + 3);
}
