/*
72
101
108
108
111
44
32
87
111
114
108
100
33
10
*/

int
main()
{
#ifndef __UCC__
  char *p = "Hello, World!\n";
#else
  int *p = "Hello, World!\n";
#endif
  int i = 0;

  while (p[i]) {
    print_int(p[i++]);
  }

  return 0;
}
