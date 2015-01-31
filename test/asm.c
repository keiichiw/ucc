/*
2
*/
int main () {
  __asm("\
  mov r1, 48        \n\
  mov r2, 2         \n\
  add r1, r1, r2    \n\
  write r1          \n\
  mov r1, 10        \n\
  write r1          \n\
");
}
