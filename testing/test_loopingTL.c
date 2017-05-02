#include <stdio.h>
#include "test_loopingTL.h"

int main() {
	struct test_loopingTL_input i;
	struct test_loopingTL_state s;
	struct test_loopingTL_output o;

	test_loopingTL_tick(&s, NULL, NULL); 
        
        char *inputTwo = "00001001110000000000000000111100";
                
        while (*inputOne) {
            i.inTwo = *inputTwo;

            test_loopingTL_tick(&s, &i, &o); 

            printf("TL 1: %c\n", o.outOne);
            printf("TL 2: %c\n", o.outTwo);

            inputOne++;
            inputTwo++;
        }

	return 0;
}
