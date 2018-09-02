#include <stdio.h>

int main(int argc, char **argv)
{
    if (argc == 1) {
        printf("more arguments are required\n");
    } else {
        printf("argc = %d\n", argc);
    }
    return 0;
}
