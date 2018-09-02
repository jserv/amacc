#include <stdio.h>

struct foo {
    char *p;
    char c;
    char *pad;
    int x;
};

int main(int argc, char **argv)
{
    struct foo bar, *p;
    bar.x = 1;
    p = &bar;

    // FIXME: 32-bit only
    if (*(int *) ((void *) p + sizeof(struct foo) - 4) != bar.x)
        return -1;

    printf("%d\n", sizeof(struct foo));
    return 0;
}
