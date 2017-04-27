#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test_fsmhello.h"

int main(){
    
    struct test_fsmhello_state s; 
    struct test_fsmhello_input i; 
   
    test_fsmhello_tick(&s, NULL, NULL); 

    i.p = 0;
    test_fsmhello_tick(&s, &i, NULL); 

    i.p = 1;
    test_fsmhello_tick(&s, &i, NULL); 

    i.p = 0;
    i.q = "Emma";
    test_fsmhello_tick(&s, &i, NULL); 

    i.p = 1;
    test_fsmhello_tick(&s, &i, NULL); 
    test_fsmhello_tick(&s, &i, NULL); 

    return 0;
}
