#include <stdio.h>
#include <stdlib.h>
int len;
struct foo {
    char* p;
    char c;
    char* pad;
    int x;
} * d;

char* str;

int main(int argc, char** argv)
{
    struct foo bar, *ptr;
    char c = '1';
    int i;
    str = "I am a String!\n";
    len = 10;
    bar.x = 1;
    struct foo* p = &bar;
    p->c = 'a';

    // FIXME: 32-bit only
    if (*(int*)((void*)p + sizeof(struct foo) - 4) != bar.x)
        exit(-1);

    printf("%zu\n", sizeof(struct foo));
    printf("%c\n", bar.c);

    d = malloc(sizeof(struct foo) * len);
    ptr = d;
    for (i = 0; i < len; ++i) {
        ptr->p = "one";
        ptr->pad = str;
        ptr->x = i;
        ptr->c = c;
        ++ptr;
        ++c;
    }

    ptr = d;
    for (i = 0; i < len; ++i) {
        printf("%d------------\n", i);
        printf("%s\n", ptr->p);
        printf("%s\n", ptr->pad);
        printf("%d\n", ptr->x);
        printf("%c\n", ptr->c);
        printf("--------------\n");
        ++ptr;
    }

    ptr = d;
    for (i = 0; i < len; ++i) {
        printf("%d------------\n", i);
        printf("%s\n", (*ptr).p);
        printf("%s\n", (*ptr).pad);
        printf("%d\n", (*ptr).x);
        printf("%c\n", (*ptr).c);
        printf("--------------\n");
        ++ptr;
    }

    return 0;
}
