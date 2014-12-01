/*
  Test for Array Expression
  output: 0 1 2 3 4 5 6
 */

int main () {
  int a[3],b[4];
  int i;
  for (i=0;i<3;++i) {
    a[i] = i;
  }
  for(i=0;i<4;++i){
    b[i] = i+3;
  }
  for (i=0;i<3;++i) {
    print_int(a[i]);
  }
  for(i=0;i<4;++i){
    print_int(b[i]);
  }
  return 0;
}
