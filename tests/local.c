int main(void)
{
    int n = 10;
    int *c = &n;
    printf("%d\n", n);
    printf("%d\n", *c);
    char cc = 'a';
    char *ptr = &cc;
    printf("%c\n", *ptr);
    return 0;
}
