#include <stdio.h>
#include <stdlib.h>
int len;
struct foo {
    char *p;
    char c;
    char *pad;
    int x;
} * d;

char *str;

int main(int argc, char **argv)
{
    struct foo bar, *p;
    struct foo *ptr;
    char c;
    int i;
    str = "I am a String!\n";
    len = 10;
    bar.x = 1;
    p = &bar;
    p->c = 'a';
    c = '1';

    // FIXME: 32-bit only
    if (*(int *) ((void *) p + sizeof(struct foo) - 4) != bar.x)
        exit(-1);

    printf("%zu\n", sizeof(struct foo));
    printf("%c\n", bar.c);

    d = malloc(sizeof(struct foo) * len);
    ptr = d;
    for (i = 0; i < len; ++i) {
        d->p = "one";
        d->pad = str;
        d->x = i;
        d->c = c;
        ++d;
        ++c;
    }

    for (i = 0; i < len; ++i) {
        printf("%d------------\n", i);
        printf("%s\n", ptr->p);
        printf("%s\n", ptr->pad);
        printf("%d\n", ptr->x);
        printf("%c\n", ptr->c);
        printf("--------------\n");
        ++ptr;
    }
    return 0;
}
