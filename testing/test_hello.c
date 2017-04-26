#include <stdio.h>
#include "test_hello.h"

int main() {
	struct test_hello_input i;
	struct test_hello_state s;

	test_hello_tick(&s, NULL, NULL); 
	test_hello_tick(&s, &i, NULL); 

	return 0;
}
