#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "test_hogTL.h"

int main() {
	struct test_hogTL_input i;
	struct test_hogTL_state s;
	struct test_hogTL_output o;

	test_hogTL_tick(&s, NULL, NULL); 

        char *inputOne = "11110000000011111000";
        char *inputTwo = "10000011110000011100";

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

            test_hogTL_tick(&s, &i, &o);

            printf("TL 1: %c\t\t", o.outOne);
            printf("TL 2: %c\n", o.outTwo);

            inputOne++;
            inputTwo++;
            
            count = 1;
        }

	return 0;
}
