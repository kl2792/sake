#include <stdio.h>
#include <stdlib.h>
#include "test_hogTL.h"

int main() {
	struct test_hogTL_input i;
	struct test_hogTL_state s;
	struct test_hogTL_output o;

	test_hogTL_tick(&s, NULL, NULL); 

        char *inputOne = "11110000000011111000";
        char *inputTwo = "10000011110000011100";

        char temp[1];

        while (*inputOne) {
             
            temp[0] = inputOne[0];
            i.inOne = atoi(temp);
            temp[0] = inputTwo[0];
            i.inTwo = atoi(temp);

            test_hogTL_tick(&s, &i, &o);

            printf("TL 1: %c\n", o.outOne);
            printf("TL 2: %c\n", o.outTwo);

            inputOne++;
            inputTwo++;
        }

	return 0;
}
