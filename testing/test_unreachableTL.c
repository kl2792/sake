#include <stdio.h>
#include "test_unreachableTL.h"

int main() {
	struct test_unreachableTL_input i;
	struct test_unreachableTL_state s;

	test_unreachableTL_tick(&s, NULL, NULL); 
	test_unreachableTL_tick(&s, &i, NULL); 

	return 0;
}
