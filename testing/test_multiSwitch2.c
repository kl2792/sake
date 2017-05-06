#include <stdio.h>
#include "test_multiSwitch.h"

int main() {
	struct test_multiSwitch_input i;
	struct test_multiSwitch_state s;
        struct test_multiSwitch_output o;

	test_multiSwitch_tick(&s, NULL, NULL); 

        i.i = 0;
        test_multiSwitch_tick(&s, &i, &o); 
        printf("%d\n", o.out);
                
        i.i = 1;
        test_multiSwitch_tick(&s, &i, &o); 
        printf("%d\n", o.out);
                
	return 0;
}
