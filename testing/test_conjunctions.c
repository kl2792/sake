#include <stdio.h>
#include "test_conjunctions.h"

int main() {

    struct test_conjunctions_input i;
    struct test_conjunctions_state s;
    struct test_conjunctions_output o;

    test_conjunctions_tick(&s, NULL, NULL); 

    i.v = 5;
    test_conjunctions_tick(&s, &i, &o); 
    printf("%d\n", o.p);
  
    i.v = 1233;
    test_conjunctions_tick(&s, &i, &o); 
    printf("%d\n", o.p);

    i.v = 123;
    test_conjunctions_tick(&s, &i, &o); 
    printf("%d\n", o.p);

    i.v = 7;
    test_conjunctions_tick(&s, &i, &o); 
    printf("%d\n", o.p);

    i.v = 56;
    test_conjunctions_tick(&s, &i, &o); 
    printf("%d\n", o.p);

    return 0;
}
