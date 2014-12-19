/*
9
9
4
4
1
1
5
5
4
4
16
16
8
8
8
8
14
14
4
4
*/
#include "test.h"

#define TEST(op, op2, arg)                      \
  print_int(a op arg);                          \
  a op2 arg;                                    \
  print_int(a)

int main()
{
  int a = 3;

  TEST(*, *=, 3);
  TEST(/, /=, 2);
  TEST(%, %=, 3);
  TEST(+, +=, 4);
  TEST(-, -=, 1);
  TEST(<<, <<=, 2);
  TEST(>>, >>=, 1);
  TEST(&, &=, 24);
  TEST(|, |=, 6);
  TEST(^, ^=, 10);

  return 0;
}
