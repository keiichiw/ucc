/*
1
2
*/

typedef int myInt;

typedef struct {
  myInt x, y;
} pair;

myInt main () {
  pair p;
  p.x = 1;
  p.y = 2;
  print_int(p.x);
  print_int(p.y);
  return 0;
}
