#include <stdio.h>
#include "test_multiSwitch1.h"

int main() {
	struct test_multiSwitch1_input i;
	struct test_multiSwitch1_state s;

	test_multiSwitch1_tick(&s, NULL, NULL); 
	test_multiSwitch1_tick(&s, &i, NULL); 

	return 0;
}
