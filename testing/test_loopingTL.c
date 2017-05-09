#include <stdio.h>
#include <stdlib.h>
#include "test_loopingTL.h"

int main() {
	struct test_loopingTL_input i;
	struct test_loopingTL_state s;
	struct test_loopingTL_output o;

	test_loopingTL_tick(&s, NULL, NULL); 
        
        char *inputTwo = "000000000000010100";
        //char *inputTwo = "00001001110000000000000000111100";
               
        char temp[1];

        while (*inputTwo) {
            temp[0] = inputTwo[0];
            i.inTwo = atoi(temp);

            test_loopingTL_tick(&s, &i, &o); 

            printf("TL 1: %c\n", o.outOne);
            printf("TL 2: %c\n", o.outTwo);

            inputTwo++;
        }

	return 0;
}
