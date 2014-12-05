/*
  0
  0
  0
  1
  2
  3
 */


int a;
struct pair {
  int x, y;
} p;

int fun () {
  a = 1;
  p.x = 2;
  p.y = 3;
}

int main () {
  print_int(a);
  print_int(p.x);
  print_int(p.y);
  fun ();
  print_int(a);
  print_int(p.x);
  print_int(p.y);
  return 0;
}
