#include <stdio.h>
#include "test_trafficLight.h"

int main() {
	struct test_trafficLight_input in;
	struct test_trafficLight_state s;
	struct test_trafficLight_output o;

	test_trafficLight_tick(&s, NULL, NULL); 

        char *input = "00011111000";
              
        while (*input) {
            in.i = *input;

            test_trafficLight_tick(&s, &in, &o);

            printf("%c\n", o.out);

            input++;
        }

	return 0;
}
