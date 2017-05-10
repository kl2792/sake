#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "test_trafficLightTL.h"

int main() {
	struct test_trafficLightTL_input in;
	struct test_trafficLightTL_state s;
	struct test_trafficLightTL_output o;

	test_trafficLightTL_tick(&s, NULL, NULL); 

        char *input = "00011111000";
             
        char temp[1];
        int count = 0;

        while (*input) {
          
            if (count != 0) {
                sleep(1);
            }

            temp[0] = input[0];
            in.i = atoi(temp);

            test_trafficLightTL_tick(&s, &in, &o);

            printf("Light color: %c\n", o.out);

            input++;
            count = 1;
        }

	return 0;
}
