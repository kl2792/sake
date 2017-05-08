#include <stdio.h>
#include "test_printing.h"

int main() {
	struct test_printing_input i;
	struct test_printing_state s;

	test_printing_tick(&s, NULL, NULL); 
	test_printing_tick(&s, &i, NULL); 

	return 0;
}
