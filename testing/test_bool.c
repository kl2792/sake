#include <stdio.h>
#include "test_bool.h"

int main() {
	struct test_bool_input i;
	struct test_bool_state s;

	test_bool_tick(&s, NULL, NULL); 

        i.i = 1;

        test_bool_tick(&s, &i, NULL); 

	return 0;
}
