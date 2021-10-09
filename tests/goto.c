// Finite state machine

// $*      -> AB*
// [^$]*   -> AC*
// [^$]$$  -> ACD
// $[^$]$$ -> ABCD

int main()
{
    char *data = "$$$$7o$n*r*0rj$o*$c0*d**dj0$$gbwj0";

A:
    printf("A");
    if (*data++ != '$')
        goto C;

B:
    printf("B");
    if (*data++ == '$')
        goto B;

C:
    printf("C");
    if (*data++ != '$')
        goto C;

D:
    printf("D");
    if (*data++ != '$')
        goto C;

    printf("\n");

    return 0;
}
