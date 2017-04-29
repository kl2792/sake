#include <stdio.h>
#include "test_brokenTL.h"

int main() {
	struct test_brokenTL_input i;
	struct test_brokenTL_state s;
	struct test_brokenTL_output o;

	test_brokenTL_tick(&s, NULL, NULL);
        
        char *inputOne = "1111001101";                
        char *inputTwo = "1110011011";
                         
        while (*inputOne) {
            input.inOne = *inputOne;
            input.inTwo = *inputTwo;
            
            test_brokenTL_tick(&s, &i, &o);

            printf("TL 1: %c\n", o.outOne);
            printf("TL 2: %c\n", o.outTwo);

            inputOne++;
            inputTwo++:  
        }

	return 0;
}
