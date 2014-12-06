/*
0
0
1
10
2
20
*/


int main () {
  int i;
  for (i=0;i<3;++i) {
    int j;
    j=0;
    while (1) {
      if (j==10*i) {
        break;
      }
      ++j;
    }
    print_int(i);
    print_int(j);
  }
  return 0;
}
