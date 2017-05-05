#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test_fsmhello.h"

int main(){
    
    struct test_fsmhello_state s; 
    struct test_fsmhello_input i; 
   
    test_fsmhello_tick(&s, NULL, NULL); 

    i.p = 0; // print nothing
    test_fsmhello_tick(&s, &i, NULL); 

    i.p = 1; // print hello
    test_fsmhello_tick(&s, &i, NULL); 

    i.p = 0;
    i.q = "Emma"; //print nothing
    test_fsmhello_tick(&s, &i, NULL); 

    i.p = 1;
    i.q = "Emma"; //print Emma's world 
    test_fsmhello_tick(&s, &i, NULL); 

    // print hello b/c p is still 1
    test_fsmhello_tick(&s, &i, NULL); 

    return 0;
}
