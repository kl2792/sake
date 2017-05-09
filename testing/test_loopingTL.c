#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "test_loopingTL.h"

int main() {
	struct test_loopingTL_input i;
	struct test_loopingTL_state s;
	struct test_loopingTL_output o;

	test_loopingTL_tick(&s, NULL, NULL); 
        
        char *inputTwo = "00001001110000000000000000111100";
               
        char temp[1];
        int count = 0;

        while (*inputTwo) {
            if (count != 0) {
                sleep(1);
            }

            temp[0] = inputTwo[0];
            i.inTwo = atoi(temp);

            test_loopingTL_tick(&s, &i, &o); 

            printf("TL 1: %c\t\t", o.outOne);
            printf("TL 2: %c\n", o.outTwo);

            inputTwo++;
            count = 1;
        }

	return 0;
}
