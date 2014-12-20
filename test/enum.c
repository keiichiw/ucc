/*
3
0
0
2
3
0
1
2
*/

#include "test.h"

enum udon {
  KAKE, KITSUNE, TEMPURA, CURRY, CARBONARA
};

enum sushi {
  MAGURO, SALMON=2, EBI, IKA=0, WASABI, INARI
};

int main () {
  enum udon u;
  u = CURRY;
  printf("%d\n", u);
  u = KAKE;
  printf("%d\n", u);


  printf("%d\n", MAGURO);
  printf("%d\n", SALMON);
  printf("%d\n", EBI);
  printf("%d\n", IKA);
  printf("%d\n", WASABI);
  printf("%d\n", INARI);

  return 0;
}
