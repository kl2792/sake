#include <stdio.h>
#include "test_multiSwitch2.h"

int main() {
	struct test_multiSwitch2_input i;
	struct test_multiSwitch2_state s;
        struct test_multiSwitch2_output o;

	test_multiSwitch2_tick(&s, NULL, NULL); 

        i.i = 0;
        test_multiSwitch2_tick(&s, &i, &o); 
        printf("%d\n", o.out);
                
        i.i = 1;
        test_multiSwitch2_tick(&s, &i, &o); 
        printf("%d\n", o.out);
                
	return 0;
}
