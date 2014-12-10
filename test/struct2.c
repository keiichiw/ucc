/*
-2
*/

struct matrix {
  int a[2], b[2];
};

int main () {
  int det;
  struct matrix mat;
  mat.a[0] = 1;
  mat.a[1] = 2;
  mat.b[0] = 3;
  mat.b[1] = 4;
  det = (mat.a[0]*mat.b[1]) - (mat.a[1]*mat.b[0]);
  print_int(det);
}
