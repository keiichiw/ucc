/*
0
1
1
1
0
*/

int main()
{
  print_int(1 && 2 && 0 && 3);
  print_int(1 && 2 && 3);

  print_int(1 || 2 || 0);
  print_int(0 || 2 || 1);
  print_int(0 || 0 || 0);

  return 0;
}
