#include <stdio.h>
#include "test_switch.h"

int main() {
	struct test_switch_input i;
	struct test_switch_state s;

	test_switch_tick(&s, NULL, NULL); 
	test_switch_tick(&s, &i, NULL); 

	return 0;
}
