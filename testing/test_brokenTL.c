#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "test_brokenTL.h"

int main() {
	struct test_brokenTL_input i;
	struct test_brokenTL_state s;
	struct test_brokenTL_output o;

	test_brokenTL_tick(&s, NULL, NULL);
        
        char *inputOne = "1111001101";                
        char *inputTwo = "1110011011";
            
        char temp[1];
        int count = 0;

        while (*inputOne) {
            if (count != 0) {
                sleep(1);
            }
            
            temp[0] = inputOne[0];
            i.inOne = atoi(temp);
            temp[0] = inputTwo[0];
            i.inTwo = atoi(temp);
            
            test_brokenTL_tick(&s, &i, &o);

            printf("TL 1: %c\t\t", o.outOne);
            printf("TL 2: %c\n", o.outTwo);

            inputOne++;
            inputTwo++;

            count = 1;
        }

	return 0;
}
