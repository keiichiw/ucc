/*
 1
 2
 */

struct stct{
  int x[10], y;
};

int main () {
  struct pair p;
  p.x[0]=1;
  p.y=2;
  print_int(p.x[0]);
  print_int(p.y);
}
