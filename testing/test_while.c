#include <stdio.h>
#include "test_while.h"

int main() {
	struct test_while_input i;
	struct test_while_state s;
	struct test_while_output o;

	test_while_tick(&s, NULL, NULL); 
	test_while_tick(&s, &i, &o); 
        printf("%d", o.o);
	return 0;
}
