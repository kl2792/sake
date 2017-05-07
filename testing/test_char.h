#ifndef __test_char_H__
#define __test_char_H__





struct test_char_input {
};

struct test_char_output {
};

struct test_char_state {
	int _running;
	int ekas;
};

int test_char_tick(struct test_char_state *, struct test_char_input *, struct test_char_output *);

#endif
