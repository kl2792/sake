#include <stdio.h>
#include <stdlib.h>
#include "test_brokenTL.h"

int main() {
	struct test_brokenTL_input i;
	struct test_brokenTL_state s;
	struct test_brokenTL_output o;

	test_brokenTL_tick(&s, NULL, NULL);
        
        char *inputOne = "1111001101";                
        char *inputTwo = "1110011011";
            
        char temp[1];

        while (*inputOne) {
            
            temp[0] = inputOne[0];
            i.inOne = atoi(temp);
            temp[0] = inputTwo[0];
            i.inTwo = atoi(temp);
            
            test_brokenTL_tick(&s, &i, &o);

            printf("TL 1: %c\n", o.outOne);
            printf("TL 2: %c\n", o.outTwo);

            inputOne++;
            inputTwo++;
        }

	return 0;
}
