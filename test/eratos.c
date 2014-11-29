#define N 1000

int main () {
  int isprime[N];
  int i, n;
  for (i=0; i<=N-1; i+=1) {
    isprime[i] = 1;
  }
  isprime[0] = isprime[1] = 0;
  for (i=2; i<=N-1; i+=1) {
    if (isprime[i]==1) {
      int j;
      for (j=2; i*j <= N-1; j+=1) {
        isprime[i*j] = 0;
      }
    }
  }

  for (i=0; i<=N-1; i+=1) {
    if (isprime[i] == 1) {
      print_int(i);
    }
  }
  return 0;
}
