#include <stdio.h>
#include "test_trafficLight.h"

int main() {
	struct test_trafficLight_input i;
	struct test_trafficLight_state s;
	struct test_trafficLight_output o;

	test_trafficLight_tick(&s, NULL, NULL); 

        char *input = "00011111000";
              
        while (*input) {
        }


           //         = "rrrgggggyrr



	test_trafficLight_tick(&s, &i, NULL); 

	return 0;
}
