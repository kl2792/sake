#include <stdio.h>
#include "test_brokenTL.h"

int main() {
	struct test_brokenTL_input i;
	struct test_brokenTL_state s;

	test_brokenTL_tick(&s, NULL, NULL); 
	test_brokenTL_tick(&s, &i, NULL); 

	return 0;
}
