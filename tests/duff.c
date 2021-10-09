void copy(char *to, char *from, int count)
{
    int n = (count + 7) >> 3;
    switch (count & 7) {
    case 0:
        do {
            *to++ = *from++;
        case 7:
            *to++ = *from++;
        case 6:
            *to++ = *from++;
        case 5:
            *to++ = *from++;
        case 4:
            *to++ = *from++;
        case 3:
            *to++ = *from++;
        case 2:
            *to++ = *from++;
        case 1:
            *to++ = *from++;
        } while (--n > 0);
    }
}

void fastcopy(char *to, char *from, int count)
{
    int n = (count + 7) >> 3;

    switch (count & 7) {
    case 7:
        goto r7;
    case 6:
        goto r6;
    case 5:
        goto r5;
    case 4:
        goto r4;
    case 3:
        goto r3;
    case 2:
        goto r2;
    case 1:
        goto r1;
    }

    do {
        *to++ = *from++;
    r7:
        *to++ = *from++;
    r6:
        *to++ = *from++;
    r5:
        *to++ = *from++;
    r4:
        *to++ = *from++;
    r3:
        *to++ = *from++;
    r2:
        *to++ = *from++;
    r1:
        *to++ = *from++;
    } while (--n > 0);
}

int main()
{
    char *message = "This is a test of duff's device\n";
    char *output = malloc(64);
    char *output2 = malloc(64);

    copy(output, message, 33);
    printf(output);

    fastcopy(output2, message, 33);
    printf(output2);

    free(output2);
    free(output);

    return 0;
}
