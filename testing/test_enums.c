#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test_enums.h"

int main(){
    
    struct test_enums_state s; 
    struct test_enums_input i; 
   
    test_enums_tick(&s, NULL, NULL); 

    i.p = 0; // print nothing
    test_enums_tick(&s, &i, NULL); 

    i.p = 1; // print hello
    test_enums_tick(&s, &i, NULL); 

    i.p = 0;
    i.q = "Emma"; //print nothing
    test_enums_tick(&s, &i, NULL); 

    i.p = 1;
    i.q = "Emma"; //print Emma's world 
    test_enums_tick(&s, &i, NULL); 

    // print hello b/c p is still 1
    test_enums_tick(&s, &i, NULL); 

    return 0;
}
