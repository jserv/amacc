#include <stdio.h>

int main(int argc, char **argv)
{
    switch (argc) {
    case 1:
        printf("No arguments\n");
        return 0;
    case 2:
        printf("arg = %s\n", argv[1]);
        break;
    default:
        printf("More than 1 argument\n");
        break;
    }
    return 0;
}
