#include <stdio.h>
#include <stdlib.h>

int assert_eq(int a, int b)
{
    if (a != b) {
        printf("Assertion: %d != %d\n", a, b);
        exit(1);
    }
    return 0;
}

int main(int argc, char **argv)
{
    /* Value test */
    int a, b, c;
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

    a = 10;
    a /= 5;
    assert_eq(a, 2);

    a = 4;
    a %= 3;
    assert_eq(a, 1);

    a = 1;
    a <<= 2;
    assert_eq(a, 4);

    a = 4;
    a >>= 2;
    assert_eq(a, 1);

    a = 17;
    a |= 14;
    assert_eq(a, 31);

    /* precedence test */
    a = 0xff;
    b = 1;
    a ^= b | 2;
    assert_eq(a, 0xfc);

    a = 17;
    a &= 7;
    assert_eq(a, 1);

    /* comma operator tests */
    a = 0;
    b = 10;
    a++, b++;
    assert_eq(a, 1);
    assert_eq(b, 11);

    c = (++a, ++b);
    assert_eq(c, 12);

    return 0;
}
