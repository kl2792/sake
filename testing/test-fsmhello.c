#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../ocaml/test-fsmhello.h"

int main(){
    
    struct test-fsmhello_state hello_fsm; 

    struct test-fsmhello_input current_input; 

    struct test-fsmhello_output current_output;

    // Set the initial state struct to initial values 
    //test-fsmhello_tick(hello_fsm, NULL, current_output);

    // Take one input at a time from the user
    char input[500];
    printf("Please enter an input stream of 0s and 1s. (max. 50): ");

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
        for (i = 0; i < length-1; i++) { 
            char curr_char = input[i];
            int curr_int = atoi(&curr_char);

            // setting new input 
            current_input.i = curr_int;

            // call tick on input 
            test-fsmhello_tick(&hello_fsm, &current_input, &current_output);

            // interpret the output and print it to the screen
            printf("%d", current_output.o);        
        }

        //reset the machine
        //test-fsmhello_tick(hello_fsm, NULL, current_output);

        printf("\nPlease enter an input stream of 0s and 1s. (max. 50): ");
    }
}
