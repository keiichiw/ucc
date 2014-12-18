/*
0
1
2
3
4
*/

typedef int myInt;
myInt x;
typedef struct {
  myInt x, y;
} pair;
pair p;
myInt main () {
  pair q;
  x   = 0;
  p.x = 1;
  p.y = 2;
  q.x = 3;
  q.y = 4;
  print_int(x  );
  print_int(p.x);
  print_int(p.y);
  print_int(q.x);
  print_int(q.y);
}
