/*
1
1
1
1
1
1
1
1
1
1
1
0
*/

#include "test.h"

typedef struct {
    unsigned d[4];
} ull;

void assign(ull *n, ull *x)
{
    n->d[0] = x->d[0];
    n->d[1] = x->d[1];
    n->d[2] = x->d[2];
    n->d[3] = x->d[3];
}

void assign_digit(ull *n, unsigned x)
{
    n->d[0] = x;
    n->d[1] = n->d[2] = n->d[3] = 0;
}

int cmp(ull *n, ull *m)
{
    int i;
    for (i = 3; i >= 0; --i)
        if (n->d[i] != m->d[i])
            return n->d[i] > m->d[i] ? 1 : -1;
    return 0;
}

int cmp_digit(ull *n, unsigned x)
{
    if (n->d[3] || n->d[2] || n->d[1] || n->d[0] > x)
        return 1;
    return n->d[0] == x ? 0 : -1;
}

int test_bit(ull *n, int b)
{
    return (n->d[(b >> 4) & 3] >> (b & 15)) & 1;
}

void add(ull *n, ull *x, ull *y)
{
    int i, tmp = 0;
    for (i = 0; i < 4; ++i) {
        tmp += x->d[i] + y->d[i];
        n->d[i] = tmp & 0xffff;
        tmp >>= 16;
    }
}

void sub(ull *n, ull *x, ull *y)
{
    int i, tmp = 0;
    for (i = 0; i < 4; ++i) {
        tmp += x->d[i] - y->d[i];
        n->d[i] = tmp & 0xffff;
        tmp = -(tmp < 0);
    }
}

void sub_digit(ull *n, ull *x, unsigned y)
{
    int i, carry;
    n->d[0] = x->d[0] - y;
    for (i = 1; i < 4; ++i) {
        carry = n->d[i - 1] >= 0x10000;
        if (carry) n->d[i - 1] += 0x10000;
        n->d[i] = x->d[i] - carry;
    }
}

void modmul(ull *n, ull *x, ull *y, ull *mod)
{
    int i;
    ull res, tmp;
    assign_digit(&res, 0);
    assign(&tmp, x);
    for (i = 0; i < 64; ++i) {
        if (test_bit(y, i)) {
            add(&res, &res, &tmp);
            if (cmp(&res, mod) > 0)
                sub(&res, &res, mod);
        }
        add(&tmp, &tmp, &tmp);
        if (cmp(&tmp, mod) > 0)
            sub(&tmp, &tmp, mod);
    }
    assign(n, &res);
}

void modpow(ull *n, ull *x, ull *y, ull *mod)
{
    int i;
    ull res, tmp;
    assign_digit(&res, 1);
    assign(&tmp, x);
    for (i = 0; i < 64; ++i, modmul(&tmp, &tmp, &tmp, mod))
        if (test_bit(y, i)) modmul(&res, &res, &tmp, mod);
    assign(n, &res);
}

void shift_right(ull *n, ull *x, int y)
{
    int i;
    int a = (y >> 4) & 3, b = y & 15;
    for (i = 0; i < 4 - a; ++i)
        n->d[i] = x->d[i + a] >> b;
    for (i = 0; i < 3 - a; ++i)
        n->d[i] |= (x->d[i + a + 1] << (16 - b)) & 0xffff;
}

int miller_rabin(ull *n, int base)
{
    int i, s;
    ull d, x, nm1;
    if (cmp_digit(n, 2) < 0 || test_bit(n, 0) == 0)
        return cmp_digit(n, 2) == 0;
    for (s = 1; test_bit(n, s) == 0; ++s);
    shift_right(&d, n, s);
    sub_digit(&nm1, n, 1);
    assign_digit(&x, base);
    modpow(&x, &x, &d, n);
    if (cmp_digit(&x, 1) == 0 || cmp(&x, &nm1) == 0)
        return 1;
    for (i = 0; i < s - 1; ++i) {
        modmul(&x, &x, &x, n);
        if (cmp(&x, &nm1) == 0) return 1;
    }
    return 0;
}

int main()
{
    ull n;

    /* assign 3825123056546413051 */
    n.d[0] = 63995;
    n.d[1] = 20378;
    n.d[2] = 37159;
    n.d[3] = 13589;

    print_int(miller_rabin(&n,  2));
    print_int(miller_rabin(&n,  3));
    print_int(miller_rabin(&n,  5));
    print_int(miller_rabin(&n,  7));
    print_int(miller_rabin(&n, 11));
    print_int(miller_rabin(&n, 13));
    print_int(miller_rabin(&n, 17));
    print_int(miller_rabin(&n, 19));
    print_int(miller_rabin(&n, 23));
    print_int(miller_rabin(&n, 29));
    print_int(miller_rabin(&n, 31));
    print_int(miller_rabin(&n, 37));
}
