#include <stdio.h>
#include "test_comment.h"

int main() {
	struct test_comment_input i;
	struct test_comment_state s;

	test_comment_tick(&s, NULL, NULL); 
	test_comment_tick(&s, &i, NULL); 

	return 0;
}
