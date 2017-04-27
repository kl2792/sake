#include <stdio.h>
#include "test_loopingTL.h"

int main() {
	struct test_loopingTL_input i;
	struct test_loopingTL_state s;

	test_loopingTL_tick(&s, NULL, NULL); 
	test_loopingTL_tick(&s, &i, NULL); 

	return 0;
}
