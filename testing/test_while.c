#include <stdio.h>
#include "test_while.h"

int main() {
	struct test_while_input i;
	struct test_while_state s;

	test_while_tick(&s, NULL, NULL); 
	test_while_tick(&s, &i, NULL); 

	return 0;
}
