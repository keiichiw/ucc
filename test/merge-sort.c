/*
-8
-8
-7
-2
0
0
3
4
8
8
*/

#include "test.h"
#define N 10
int xor128();
void mergeSort (int x[], int left, int right);

int main () {
  int a[N];
  int i;
  for (i=0; i<N; i++) {
    a[i] = xor128()%N;
  }

  // mergeSort(a, 0, N);

  for (i=0; i<N;++i) {
    printf("%d\n", a[i]);
  }
  return 0;
}

int xor128(void) {
  static unsigned x = 123456789;
  static unsigned y = 362436069;
  static unsigned z = 521288629;
  static unsigned w = 88675123;
  unsigned t;

  t = x ^ (x << 11);
  x = y; y = z; z = w;
  return (int)(w = (w ^ (w >> 19)) ^ (t ^ (t >> 8)));
}

void mergeSort (int x[], int l, int r) {
  int temp[N], i, j, k;
  int mid;
  if (l >= r-1)
    return;
  mid = (l + r) / 2;
  mergeSort(x, l, mid);
  mergeSort(x, mid, r);
  for (i=l; i<mid; ++i) {
    temp[i] = x[i];
  }

  for (i=mid, j=1; i<r; ++i, ++j) {
    temp[r-j] = x[i];
  }
  i = l;
  j = r-1;
  k = l;
  while (i<=j) {
    if (temp[i] < temp[j]) {
      x[k++] = temp[i++];
    } else {
      x[k++] = temp[j--];
    }
  }
  return;
}
