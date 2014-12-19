/*
10
1
11
-1
3
31
*/
#include "ucc.h"

#ifndef __UCC__
#include <stdio.h>
int print_int(int n){
  printf("%d\n", n);
}
#endif

struct pair{
  int x, y;
};

struct tag_pair{
  int tag;
  struct pair *p;
};

struct bogo_pair{
  int bogo;
  struct tag_pair *q;
};

int read_struct(struct pair *p, int none, struct bogo_pair *bp){
  print_int(none);

  print_int(p->x);
  print_int(p->y);

  print_int(bp->bogo);
  print_int(bp->q->p->x);
  print_int(((bp->q)->p)->y);

  return 0;
}

int main(){
  struct pair p0, p1;
  struct tag_pair tp0;
  struct bogo_pair bg0;

  p0.x = 1;
  p0.y = 11;
  p1.x = 3;
  p1.y = 31;

  tp0.tag = 55;
  tp0.p = &p1;

  bg0.bogo = -1;
  bg0.q = &tp0;

  read_struct(&p0,10,&bg0);

  return 0;
}
