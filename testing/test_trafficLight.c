#include <stdio.h>
#include <stdlib.h>
#include "test_trafficLight.h"

int main() {
	struct test_trafficLight_input in;
	struct test_trafficLight_state s;
	struct test_trafficLight_output o;

	test_trafficLight_tick(&s, NULL, NULL); 

        char *input = "00011111000";
             
        char temp[1];

        while (*input) {
            temp[0] = input[0];
            in.i = atoi(temp);

            test_trafficLight_tick(&s, &in, &o);

            printf("%c\n", o.out);

            input++;
        }

	return 0;
}
