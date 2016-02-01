#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    char *p;
    int fd, readsz;
    ++argv;

    if (!(p = malloc(1024 * 16))) { printf("failed to malloc memory\n"); exit(1); }

    printf("opening %s...\n", *argv);
    fd = open(*argv, 0);

    readsz = read(fd, p, 1024 * 16);
    printf("read %d bytes\nContents:\n______________________________\n%s", readsz, p);
    printf("\n_______________________________\n");

    exit(0);
}
