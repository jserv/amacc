struct Value {
    int r;
    int i;
};

int main(void)
{
    int n = 10;
    int *c = &n;
    printf("%d\n", n);
    printf("%d\n", *c);
    char cc = 'a';
    char *ptr = &cc;
    printf("%c\n", *ptr);
    struct Value a;
    a.r = 10;
    a.i = 20;
    struct Value *v = &a;
    printf("%d %d", v->r, v->i);
    return 0;
}
