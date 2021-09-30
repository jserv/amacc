#include <stdio.h>

int main(int argc, char **argv)
{
    int i, j;

    j = 10;

    for (i = 0, printf("let's loop\n"); i < j; i++, printf("loops again\n"))
        printf("loop %d\n", i);

    printf("nested loop\n");
    for (i = 1; i < 10; i++) {
        for (j = 1; j < 10; j++) {
            printf("%d * %d = %d\t", i, j, i * j);
        }
        printf("\n");
    }

    printf("\n");
    for (i = 1; i <= 5; ++i) {
        for (j = 1; j <= 5; ++j) {
            if (j > i)
                break;
            printf("* ");
        }
        printf("\n");
    }
    printf("\n");

    printf("\n");
    for (i = 1; i <= 30; ++i) {
        if (i > 10 && i < 20)
            continue;
        printf("%d ", i);
    }
    printf("\n");

    return 0;
}
