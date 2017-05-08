#include <stdio.h>
#include <stdlib.h>
#include "test_unreachableTL.h"

int main() {
	struct test_unreachableTL_input in;
	struct test_unreachableTL_state s;
	struct test_unreachableTL_output o;

	test_unreachableTL_tick(&s, NULL, NULL);

        char *input = "11101010101";
        char temp[1];

        while(*input) {
                temp[0] = input[0];
                in.inOne = atoi(temp);
                test_unreachableTL_tick(&s, &in, &o);
                printf("Light color: %c\n", o.outOne);
                input++;
        }

	return 0;
}
