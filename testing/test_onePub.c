#include <stdio.h>
#include "test_onePub.h"

int main() {
	struct test_onePub_input i;
	struct test_onePub_state s;

	test_onePub_tick(&s, NULL, NULL); 
	test_onePub_tick(&s, &i, NULL); 

	return 0;
}
