#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    if (argc == 1) {
        printf("more arguments are required\n");
        exit(1);
    }
    printf("argc = %d\n", argc);

    return 0;
}
