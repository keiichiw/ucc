/*
0
1
2
3
4
4
3
2
1
0
*/

#include "test.h"

struct list {
  int car;
  struct list *cdr;
} heap[1024];

int i;

struct list *alloc() {
  return heap + i++;
}

struct list *cons(int car, struct list *cdr) {
  struct list *x;

  x = alloc();
  x->car = car;
  x->cdr = cdr;
  return x;
}

struct list *rev(struct list *list, struct list *acc) {
  if (list == 0) {
    return acc;
  }
  else {
    struct list *tail = alloc();

    tail->car = list->car;
    tail->cdr = acc;
    return rev(list->cdr, tail);
  }
}

void print_list(struct list *list) {
  if (list == 0)
    return;
  print_int(list->car);
  print_list(list->cdr);
}

/* list of [s,e) */
struct list *iota(int s, int e) {
  if (s == e) {
    return 0;
  } else {
    return cons(s, iota(s + 1, e));
  }
}

int main()
{
  struct list *e;

  e = iota(0, 5);

  print_list(e);

  e = rev(e, 0);

  print_list(e);
}
