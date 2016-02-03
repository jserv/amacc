#include <stdio.h>

int main(int argc, char **argv)
{
    int i;
    i = 0;
    while (i < 10) {
        printf("iteration %d\n", i);
        if (i == 5) {
            printf("break while-loop\n");
            break;
        }
        i++;
    }
    return 0;
}
