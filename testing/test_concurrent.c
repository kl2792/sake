#include <stdio.h>
#include "test_concurrent.h"

int main() {
	struct test_concurrent_input i;
	struct test_concurrent_state s;

	test_concurrent_tick(&s, NULL, NULL); 

        i.i = 42; 
        test_concurrent_tick(&s, &i, NULL); 

        i.i = 2;
        test_concurrent_tick(&s, &i, NULL); 

        i.i = 42;
        test_concurrent_tick(&s, &i, NULL); 

	return 0;
}
