/*
1
2
0
4
5
6
7
0
0
10
11
12
13
0
0
16
17
18
19
20
21
22
0
0
25
0
0
*/
#include "test.h"
int main() {
    int i, j, k;
    int a[3][3][3] = {
        { { 1, { 2 } }, 4, 5, 6, { 7 } },
        10, 11, 12, { 13 }, 16, 17, 18,
        19, { 20 }, { 21 }, { 22 }, 25
    };
    for (i = 0; i < 3; ++i)
        for (j = 0; j < 3; ++j)
            for (k = 0; k < 3; ++k)
                print_int(a[i][j][k]);
}
