#include <stdio.h>
#include "test_block.h"

int main() {
	struct test_block_input i;
	struct test_block_state s;

	test_block_tick(&s, NULL, NULL); 
	test_block_tick(&s, &i, NULL); 

	return 0;
}
