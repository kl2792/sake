#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int (*print)(const char * restrict, ...) = printf;

/*int print(const char * restrict format, int arg) {
	printf(format, arg);
}*/
