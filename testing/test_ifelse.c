#include <stdio.h>
#include "test_ifelse.h"

int main() {
	struct test_ifelse_input i;
	struct test_ifelse_state s;

	test_ifelse_tick(&s, NULL, NULL); 
	test_ifelse_tick(&s, &i, NULL); 

	return 0;
}
