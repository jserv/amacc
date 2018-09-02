#include <stdio.h>
#include <stdlib.h>

int assert_eq(int a, int b)
{
    if (a != b) {
        printf("Assertion: %d != %d\n", a, b);
        exit(1);
    }
}

int main(int argc, char **argv)
{
    /* Value test */
    int a;
    a = 1;
    a += 101;
    assert_eq(a, 102);

    a = 10;
    a -= 101;
    assert_eq(a, -91);

    a = 10;
    a *= 101;
    assert_eq(a, 1010);

    /* precedence test */
    a = 1;
    a += 3 * 4;
    assert_eq(a, 13);

    a = 1;
    a -= 3 * 4;
    assert_eq(a, -11);

    a = 2;
    a *= 3 * 4;
    assert_eq(a, 24);

    return 0;
}
