#include <stdio.h>
#include "test_hogTL.h"

int main() {
	struct test_hogTL_input i;
	struct test_hogTL_state s;
	struct test_hogTL_output o;

	test_hogTL_tick(&s, NULL, NULL); 

        char *inputOne = "11110000000011111000";
        char *inputTwo = "10000011110000011100";

        while (*inputOne) {
            i.inOne = *inputOne;
            i.inTwo = *inputTwo;

            test_trafficLight_tick(&s, &i, &o);

            printf("TL 1: %c\n", o.outOne);
            printf("TL 2: %c\n", o.outTwo);

            inputOne++;
            inputTwo++;
        }

	return 0;
}
