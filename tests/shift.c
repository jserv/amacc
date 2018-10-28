#include <stdio.h>

int main()
{
    int a, b, c;
    printf("1 << 0 = %x\n", 1 << 0);
    printf("1 << 2 = %x\n", 1 << 2);
    printf("0 << 4 = %x\n", 0 << 4);
    printf("1 << 31 = %x\n", 1 << 31);
    printf("1 << 32 = %x\n", 1 << 32);
    printf("4 << -1 = %x\n", 4 << -1);
    printf("4 >> -1 = %x\n", 4 >> -1);
    printf("-1 << 1 = %x\n", -1 << 1);
    printf("-1 << 0 = %x\n", -1 << 0);

    printf("4 >> 1 = %x\n", 4 >> 1);
    printf("4 >> 5 = %x\n", 4 >> 5);
    printf("0x80000000 >> 31 = %x\n", (int) 0x80000000 >> 31);
    printf("-1 >> 2 = %x\n", -1 >> 2);
    b = 0xbef6d568;
    c = 0xbef6d56a;
    printf("%d - %d = %d(%x)\n", b, c, b - c, b - c);
    a = (b << 8) | 12;
    c = (a >> 8) | ((int) b & 0xff000000);
    printf("a = %x, b = %x, c = %x\n", a, b, c);
    a = ((b & 0x007fffff) << 8) | 12;
    c = (a >> 8) | ((int) b & 0xff800000);
    printf("a = %x, b = %x, c = %x\n", a, b, c);

    return 0;
}
