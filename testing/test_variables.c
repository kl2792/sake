#include <stdio.h>
#include "test_variables.h"

int main() {
	struct test_variables_input i;
	struct test_variables_state s;

	test_variables_tick(&s, NULL, NULL); 
	test_variables_tick(&s, &i, NULL); 

	return 0;
}
