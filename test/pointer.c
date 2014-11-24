int main () {
  int a, *b, **c;
  b = &a;
  c = &b;
  a = 1;
  print_int(**c);
  return 0;
}
