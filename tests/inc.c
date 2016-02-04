int inc(int x)
{
    return x + 1;
}

int add2(int x)
{
    return inc(inc(x));
}

int main(int argc, char **argv)
{
    return add2(argc) - 4;
}
