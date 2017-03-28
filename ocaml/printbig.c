/*
 *  A function illustrating how to link C code to code generated from LLVM 
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int printbig(int c)
{
    printf("%d\n", c);
}

