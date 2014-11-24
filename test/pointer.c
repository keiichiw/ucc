int main () {
  int a, *b, **c, **d, e;
  b = &a;
  c = &b;
  **c = 1;
  print_int(a);
  d = c;
  a = 2;
  print_int(**d);
  //e = *(*d); TODO
  //print_int(e);
  return 0;
}
