#include <stdio.h>
#include "test_nestedFor.h"

int main() {
	struct test_nestedFor_input i;
	struct test_nestedFor_state s;

	test_nestedFor_tick(&s, NULL, NULL); 
	test_nestedFor_tick(&s, &i, NULL); 

	return 0;
}
