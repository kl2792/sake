#include <stdio.h>
#include "test_emptyfsm.h"

int main() {
	struct test_emptyfsm_input i;
	struct test_emptyfsm_state s;

	test_emptyfsm_tick(&s, NULL, NULL); 
	test_emptyfsm_tick(&s, &i, NULL); 

	return 0;
}
