#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test_fsmhello.h"

int main(){
    
    struct test_fsmhello_state hello_fsm; 

    struct test_fsmhello_input current_input; 

    struct test_fsmhello_output current_output;

    // Set the initial state struct to initial values 
    //test-fsmhello_tick(hello_fsm, NULL, current_output);

    // Take one input at a time from the user
    char input[500];
    //printf("Please enter an input stream of 0s and 1s. (max. 50): ");

    /*
    // RUN THE SIMULATION MULTIPLE TIMES 
    while (fgets(input, sizeof(input), stdin) != NULL) {

        int length = strlen(input);
        printf("%d\n", length);
        
        if (length > 50) {
            fprintf(stderr, "ERROR: input stream is too long\n");
            printf("Please enter an input stream of 0s and 1s. (max. 50): ");
            continue;
        }

        int i;
        for (i = 0; i < length-1; i = i+2) { 
            char curr_char1 = input[i];
            char curr_char2 = input[i+1];
            int curr_int1 = atoi(&curr_char1);
            int curr_int2 = atoi(&curr_char2);

            // setting new input 
            current_input.i = curr_int1;
            current_input.o = curr_int2;

            // call tick on input 
            test_fsmhello_tick(&hello_fsm, &current_input, &current_output);

            // interpret the output and print it to the screen
            printf("%d", current_output.k);        
            printf("%d", current_output.l);        
        }

        //reset the machine
        //test-fsmhello_tick(hello_fsm, NULL, current_output);

        printf("\nPlease enter an input stream of 0s and 1s. (max. 50): ");
    }
    */
    return 0;
}
