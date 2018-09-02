#include <stdio.h>
#include <stdlib.h>

int my_atoi(char *s)
{
    int res;
    res = 0;
    while (*s) {
        if (*s < '0' || '9' < *s)
            return 0;
        res = res * 10 + (*s - '0');
        ++s;
    }
    return res;
}

int fib(int n)
{
    if (n < 2)
        return 1;
    return fib(n - 1) + fib(n - 2);
}

int main(int argc, char **argv)
{
    int n;
    if (argc < 2) {
        printf("Usage: %s <number>\n", argv[0]);
        exit(1);
    }

    n = my_atoi(argv[1]);
    printf("%d\n", fib(n));

    return 0;
}
