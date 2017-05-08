#include <stdio.h>
#include "test_header.h"

int main() {
	struct test_header_input i;
	struct test_header_state s;

	test_header_tick(&s, NULL, NULL); 
	test_header_tick(&s, &i, NULL); 

	return 0;
}
