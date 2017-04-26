#include <stdio.h>
#include "test_trafficLight.h"

int main() {
	struct test_trafficLight_input i;
	struct test_trafficLight_state s;

	test_trafficLight_tick(&s, NULL, NULL); 
	test_trafficLight_tick(&s, &i, NULL); 

	return 0;
}
