#include <stdio.h>
#include "test_nestedFor.h"

int main() {
	struct test_nestedFor_input i;
	struct test_nestedFor_state s;
        struct test_nestedFor_output o;

	test_nestedFor_tick(&s, NULL, NULL); 

	test_nestedFor_tick(&s, &i, &o); 

        printf("%d", o.q);

	return 0;
}
