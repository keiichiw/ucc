/*
  Test for continue statement
  output: 1 3 5 7 9 1 3 5 7 9 1 3 5 7 9
*/

int main () {
  int i;
  i=0;
  while (i<10) {
    if (i%2 == 0) {
      ++i;
      continue;
    }
    print_int(i);
    ++i;
  }
  i=0;
  do {
    if (i%2 == 0) {
      ++i;
      continue;
    }
    print_int(i);
    ++i;
  } while (i<10);

  for (i=0; i<10; ++i) {
    if (i%2 == 0) {
      continue;
    }
    print_int(i);
  }

  return 0;
}
