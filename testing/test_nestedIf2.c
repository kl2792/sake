#include <stdio.h>
#include "test_nestedIf2.h"

int main() {
	struct test_nestedIf2_input i;
	struct test_nestedIf2_state s;

	test_nestedIf2_tick(&s, NULL, NULL); 
	test_nestedIf2_tick(&s, &i, NULL); 

	return 0;
}
