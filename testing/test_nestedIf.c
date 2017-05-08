#include <stdio.h>
#include "test_nestedIf.h"

int main() {
	struct test_nestedIf_input i;
	struct test_nestedIf_state s;

	test_nestedIf_tick(&s, NULL, NULL); 
	test_nestedIf_tick(&s, &i, NULL); 

	return 0;
}
