/*
13
*/

int add(int x, int y){
  if (x==0) {
    return y;
  } else {
    return 1+add(x-1,y);
  }
}

int main () {
  print_int(add(3,10));
}
