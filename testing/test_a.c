#include <stdio.h>
#include "test_a.h"

int main() {
	struct test_a_input i;
	struct test_a_state s;

	test_a_tick(&s, NULL, NULL); 
	test_a_tick(&s, &i, NULL); 

	return 0;
}
