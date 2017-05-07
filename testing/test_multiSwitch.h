#ifndef __test_multiSwitch_H__
#define __test_multiSwitch_H__





struct test_multiSwitch_input {
	int i;
};

struct test_multiSwitch_output {
	int out;
};

struct test_multiSwitch_state {
	int _running;
	int switchCase;
};

int test_multiSwitch_tick(struct test_multiSwitch_state *, struct test_multiSwitch_input *, struct test_multiSwitch_output *);

#endif
