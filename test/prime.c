int main () {
  int i, j;
  for (i=2; i<=100; i+=1) {
    int flg;
    flg = 1;
    for (j=2; j<=(i-1); j+=1) {
      if (i%j==0) {
        flg = 0;
      }
    }
    if (flg == 1) {
      print_int(i);
    }
  }
  return 0;
}
