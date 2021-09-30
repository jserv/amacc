#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

int main(int ac, char **av)
{
    char *jitmem;
    int *je, var;

    jitmem = mmap(0, 256, 7, 0x22, -1, 0);
    je = (int *) jitmem;
    *je++ = 0xe59f000c;  // ldr r0, [pc, #12]
    *je++ = 0xe5901000;  // ldr r1, [r0]
    *je++ = 0xe2811009;  // add r1, r1, #9
    *je++ = 0xe5801000;  // str r1, [r0]
    *je++ = 0xe1a0f00e;  // mov pc, lr
    *je = (int) &var;
    __clear_cache(jitmem, je);

    var = ac;
    bsearch(&av, av, 1, 1, (void *) jitmem);
    printf("ac = %d, var = %d\n", ac, var);
    return 0;
}
