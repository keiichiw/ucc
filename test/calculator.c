/*
100
100
2014
2015
*/

#include "test.h"

#define BUF_SIZE 256

int pos;
char str[BUF_SIZE];
int is_error;

int isdigit(char c)
{
    return '0' <= c && c <= '9';
}

int parse0()
{
    int res = 0;
    if (!isdigit(str[pos])) is_error = 1;
    while (isdigit(str[pos]))
        res = res * 10 + (str[pos++] - '0');
    return res;
}

int parse4();
int parse1()
{
    if (str[pos] == '(') {
        int res;
        ++pos;
        res = parse4();
        if (str[pos++] != ')') is_error = 1;
        return res;
    }
    return parse0();
}

int parse2()
{
    if (str[pos] == '-') {
        ++pos;
        return -parse1();
    }
    return parse1();
}

int parse3()
{
    int res = parse2();
    while (str[pos] == '*' || str[pos] == '/') {
        char op = str[pos++];
        int tmp = parse2();
        op == '*' ? (res *= tmp) : (res /= tmp);
    }
    return res;
}

int parse4()
{
    int res = parse3();
    while (str[pos] == '+' || str[pos] == '-') {
        char op = str[pos++];
        int tmp = parse3();
        op == '+' ? (res += tmp) : (res -= tmp);
    }
    return res;
}

int parse(char *p)
{
    int i, sz = 0;
    int res;
    pos = is_error = 0;
    for (i = 0; p[i] && sz < BUF_SIZE; ++i)
        if (p[i] != ' ') str[sz++] = p[i];
    if (sz == BUF_SIZE) {
        is_error = 1;
        return 0;
    }
    str[sz] = '$';
    res = parse4();
    if (pos != sz) is_error = 1;
    return res;
}

void calc(char *p)
{
    int res = parse(p);
    if (is_error)
        printf("parse error\n");
    else
        printf("%d\n", res);
}

int main()
{
    calc("123 - 45 - 67 + 89");
    calc("-12 + 3 * 4 * 5 + 6 * 78 / 9");
    calc("-1 + 2 + 3 * (-4 * 5 * (6 * -7 + 8) - 9)");
    calc("((1 + 2) / 3 + 4 * (56 + 7)) * 8 - 9");
}
