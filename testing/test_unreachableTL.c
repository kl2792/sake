#include <stdio.h>
#include "test_unreachableTL.h"

int main() {
	struct test_unreachableTL_input in;
	struct test_unreachableTL_state s;
	struct test_unreachableTL_output o;

	test_unreachableTL_tick(&s, NULL, NULL);

        char *input = "11101010101";

        while(*input) {
                in.inOne = *input;
                test_unreachableTL_tick(&s, &in, &o);
                printf("%c\n", o.outOne);
                input++;
        }

	return 0;
}
