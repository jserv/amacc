#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char **argv)
{
    char *p;
    int fd, readsz;

    if (!(p = malloc(1024 * 16))) {
        printf("failed to malloc memory\n");
        exit(1);
    }

    fd = open("amacc.c", 0);

    readsz = read(fd, p, 1024 * 16);
    printf("read %d bytes\nContents:\n______________________________\n%s",
           readsz, p);
    printf("\n_______________________________\n");

    return 0;
}
