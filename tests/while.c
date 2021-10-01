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

int filteradd(char *data)
{
    int sum = 0;
    do {
        if (*data == '*')
            break;
        if (*data < '0' || *data > '9')
            continue;
        sum += *data - '0';
    } while (*++data != 0);

    return sum;
}

int main(int argc, char **argv)
{
    printf("%d\n", fact(8));
    printf("\n%d\n", filteradd("445h5h5g*45hb7b4g5"));

    return 0;
}
