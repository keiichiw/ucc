/*
0
3
-1
0
1
2
5
abcde
aaaae
abcae
*/

#include <string.h>
#include <stdio.h>

int main()
{
    char *s = "abcde";
    char a[10];

    printf("%d\n", strlen(""));
    printf("%d\n", strlen("abc"));
    printf("%d\n", strcmp("abc", "abcd"));
    printf("%d\n", strcmp("abcd", "abcd"));
    printf("%d\n", strcmp("abcde", "abcd"));
    printf("%d\n", strchr(s, 'c') - s);
    printf("%d\n", strchr(s, '\0') - s);

    strcpy(a, s);
    printf("%s\n", a);

    memset(a, 'a', 4);
    printf("%s\n", a);

    memcpy(a, s, 3);
    printf("%s\n", a);

    return 0;
}
