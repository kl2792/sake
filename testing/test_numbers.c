#include <stdio.h>
#include "test_numbers.h"

int main() {
	struct test_numbers_input i;
	struct test_numbers_state s;

	test_numbers_tick(&s, NULL, NULL); 
	test_numbers_tick(&s, &i, NULL); 

	return 0;
}
