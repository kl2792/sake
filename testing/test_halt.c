#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test_halt.h"

int main(){
    
    struct test_halt_state s; 
    struct test_halt_input i; 
   
    test_halt_tick(&s, NULL, NULL); 

    i.p = 0; // print nothing
    test_halt_tick(&s, &i, NULL); 

    i.p = 1; // print hello
    test_halt_tick(&s, &i, NULL); 

    i.p = 0;
    i.q = "Emma"; //print nothing
    test_halt_tick(&s, &i, NULL); 

    i.p = 1;
    i.q = "Emma"; //print Emma's world
    test_halt_tick(&s, &i, NULL)

    if (test_halt_tick(&s, &i, NULL) == NULL) {
       printf("And we halted");
    } 
    
    return 0;
}
