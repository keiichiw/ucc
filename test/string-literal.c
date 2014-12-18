/*
72
101
108
108
111
10
0
*/

int
main()
{
#ifndef __UCC__
  char a[7] = "Hello\n";
#else
  int a[7] = "Hello\n";
#endif
  int i;

  for (i = 0; i < 7; ++i) {
    print_int(a[i]);
  }
  return 0;
}
