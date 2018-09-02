#include <stdio.h>

int main()
{
    int a;
    a = 256;
    while (a++ < 512)
        printf("a = %d\n", a);

    return 0;
}
