/*
-1
1073741823
1073741823
1073741823
1073741823
-1
1073741823
1073741823
1073741823
1073741823
*/

int main () {

  print_int((1-2)>>2);
  print_int((unsigned)(1-2)>>2);
  print_int(((unsigned)1-2)>>2);
  print_int((1-(unsigned)2)>>2);
  print_int((int)((unsigned)(1-2)>>2));

  print_int((-3+2)>>2);
  print_int((unsigned)(-3+2)>>2);
  print_int(((unsigned)-3+2)>>2);
  print_int((-3+(unsigned)2)>>2);
  print_int((int)((unsigned)(-3+2)>>2));

}
