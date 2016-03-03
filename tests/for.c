#include <stdio.h>

int main(int argc, char **argv)
{
        int i, j;

        j = 10;

        for (i = 0, printf("let's loop\n"); i < j; i++, printf("loops again\n"))
                printf("loop %d\n", i);

        return 0;
}
