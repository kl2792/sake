#include <stdio.h>
#include "test_switch.h"

int main() {
	struct test_switch_input i;
	struct test_switch_state s;
        struct test_switch_output o;

	test_switch_tick(&s, NULL, NULL); 

        i.i = 0;
        test_switch_tick(&s, &i, &o); 
        printf("%d\n", o.o);
                
        i.i = 1;
        test_switch_tick(&s, &i, &o); 
        printf("%d\n", o.o);
                
	return 0;
}
