void copy(char *to, char *from, int count)
{
    int n = (count + 7) >> 3;
    switch (count & 7) {
    case 0: do { *to++ = *from++;
    case 7:      *to++ = *from++;
    case 6:      *to++ = *from++;
    case 5:      *to++ = *from++;
    case 4:      *to++ = *from++;
    case 3:      *to++ = *from++;
    case 2:      *to++ = *from++;
    case 1:      *to++ = *from++;
            } while (--n > 0);
    }
}

int main()
{
    char *message = "This is a test of duff's device\n";
    char *output  = (char *) malloc(64);

    copy(output, message, 33);
    printf(output);

    free(output);

    return 0 ;
}
