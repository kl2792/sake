#include <stdio.h>
#include "test_42.h"

int main() {
	struct test_42_input i;
	struct test_42_state s;

	test_42_tick(&s, NULL, NULL); 
	test_42_tick(&s, &i, NULL); 

	return 0;
}
