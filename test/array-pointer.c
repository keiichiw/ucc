/*
0
1
2
3
4
0
1
2
3
4
*/

int main () {
  int a[5], i;
  int *p;
  p = a;
  for (i=0; i<5; i++) {
    a[i] = i;
  }
  for (i=0; i<5; i++) {
    print_int(*(a+i));
  }
  for (i=0; i<5; i++) {
    print_int(*(p+i));
  }
  return 0;
}
