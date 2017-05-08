#include <stdio.h>
#include "test_char.h"

int main() {
	struct test_char_input i;
	struct test_char_state s;

	test_char_tick(&s, NULL, NULL); 
	test_char_tick(&s, &i, NULL); 

	return 0;
}
