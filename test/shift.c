/*
-1
-1
-1
1073741823
*/



int main () {
  int a = -1;
  unsigned b = a;
  print_int(a);
  print_int(b);
  a>>=2;
  b>>=2;
  print_int(a);
  print_int(b);
  return 0;
}
