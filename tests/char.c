#include <stdio.h>
#include <stdlib.h>

int mcmp(char *a, char *b, int n)
{
    int ret;
    while (n--) {
        ret = *a++ - *b++;
        if (ret)
            return ret;
    }
    return 0;
}

void mcpy(char *a, char *b, int n)
{
    char *dst;
    dst = a;
    while (n--)
        *a++ = *b++;
}

int main()
{
    char *p;
    int v;
    p = malloc(128);
    mcpy(p, "hello world", 12);
    printf("%s\n", p);
    printf("memcmp = %d\n", mcmp(p, "hello world", 12));
    printf("memcmp = %d\n", mcmp(p, "hello xorld", 12));
    printf("memcmp = %d\n", mcmp(p, "hello yorld", 12));
    p[0] = -1;
    v = p[0];
    printf("%x %d %d %x\n", p[0], p[0], v, p[1]);
    printf("\0"); /* shall be nothing generated */

    return 0;
}
