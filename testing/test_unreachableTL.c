#include <stdio.h>
#include "test_unreachableTL.h"

int main() {
	struct test_unreachableTL_input i;
	struct test_unreachableTL_state s;
	struct test_unreachableTL_output o;

	test_unreachableTL_tick(&s, NULL, NULL);

        char *input = "11101010101";

        while(*input) {
                input.inOne = *input;
                test_unreachableTL_tick(&s, &i, &o);
                printf("%c\n", o.outOne);
        }

	return 0;
}
