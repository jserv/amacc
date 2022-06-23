#include <stdlib.h>
#include <stdio.h>

int assert_eq(int a, int b)
{
    if (a != b) {
        printf("Assertion: %d != %d\n", a, b);
        exit(1);
    }
    return 0;
}

int main()
{
    int i;
    int *s, *e, v;
    int *data;
    struct abc_s { int a, b, c; } *sptr;

    s = (int *) 0xbebebeb0;
    e = (int *) 0xbebebeb4;
    v = e - s;
    if (v == 1)
        printf("passed\n");
    else
        printf("failed, e - s = %x\n", v);
    v = (int) (e - 1);
    if (v == (int) s)
        printf("passed\n");
    else
        printf("failed, e - s = %x\n", v);

    data = (int *) malloc(sizeof(int) * 10);
    sptr = (struct abc_s *) malloc(sizeof(struct abc_s) * 10);

    assert_eq(&sptr[5] - &sptr[2], 3);
    assert_eq((int) (&sptr[5] - 3), (int) &sptr[2]);
    assert_eq((int) &sptr[5], (int) (sptr + 5));
    assert_eq((int) &sptr[5], (int) (5 + sptr));

    for (i = 0; i < 10; ++i) data[i] = i;

    s = data; e = &data[9];
    for (i = 0; i < 10; ++i) {
       assert_eq(s[i], *(s + i));
       assert_eq(e[-i], *(e - i));
    }

    free(sptr); free(data);

    return 0;
}
