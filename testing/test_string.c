#include <stdio.h>
#include "test_string.h"

int main() {
	struct test_string_input i;
	struct test_string_state s;

	test_string_tick(&s, NULL, NULL); 
	test_string_tick(&s, &i, NULL); 

	return 0;
}
