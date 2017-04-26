#include <stdio.h>
#include "test_for.h"

int main() {
	struct test_for_input i;
	struct test_for_state s;

	test_for_tick(&s, NULL, NULL); 
	test_for_tick(&s, &i, NULL); 

	return 0;
}
