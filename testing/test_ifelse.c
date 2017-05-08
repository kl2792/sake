#include <stdio.h>
#include "test_ifelse.h"

int main() {

    struct test_ifelse_input i;
    struct test_ifelse_state s;
    struct test_ifelse_output o;

    test_ifelse_tick(&s, NULL, NULL); 

    i.v = 5;
    test_ifelse_tick(&s, &i, &o); 
    printf("%d\n", o.p);
  
    i.v = 1233;
    test_ifelse_tick(&s, &i, &o); 
    printf("%d\n", o.p);

    i.v = 123;
    test_ifelse_tick(&s, &i, &o); 
    printf("%d\n", o.p);

    i.v = 7;
    test_ifelse_tick(&s, &i, &o); 
    printf("%d\n", o.p);

    i.v = 56;
    test_ifelse_tick(&s, &i, &o); 
    printf("%d\n", o.p);

    return 0;
}
