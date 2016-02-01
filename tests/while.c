#include <stdio.h>

int fact(int n)
{
    int r;
    r = 1;
    while (n > 0) {
        r = r * n;
        printf("n = %d, r = %d\n", n, r);
        --n;
    }
    return r;
}

int main(int argc, char **argv)
{
    printf("%d\n", fact(8));
    return 0;
}
