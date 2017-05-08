#include <stdio.h>
#include "test_switch1.h"

int main() {
	struct test_switch1_input i;
	struct test_switch1_state s;

	test_switch1_tick(&s, NULL, NULL); 
	test_switch1_tick(&s, &i, NULL); 

	return 0;
}
