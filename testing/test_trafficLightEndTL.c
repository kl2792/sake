#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "test_trafficLightEndTL.h"

int main() {
	struct test_trafficLightEndTL_input in;
	struct test_trafficLightEndTL_state s;
	struct test_trafficLightEndTL_output o;

	test_trafficLightEndTL_tick(&s, NULL, NULL); 

        char *input = "00011111000";
             
        char temp[1];
        int count = 0;
                
        while (*input) {
           
            if (count != 0) {
                sleep(1);
            }

            temp[0] = input[0];
            in.i = atoi(temp);

            if (test_trafficLightEndTL_tick(&s, &in, &o) == NULL) {
                 printf("Ended traffic light sequence");
                 break;
            }

            printf("Light color: %c\n", o.out);

            input++;
            count = 1;
        }

	return 0;
}
