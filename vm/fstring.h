#ifndef FSTRING_H
#define FSTRING_H

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <ctype.h>

int fstring(char* str, size_t size, char* fmt, intptr_t args[], int nu);

#endif /* end of include guard: FSTRING_H */
