#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hello.h"

int main(){
    
    struct name_state *hello_fsm = malloc(sizeof(struct name_state));
    if (hello_fsm == NULL) {
        perror("malloc returned null");
        exit(1);
    }

    struct name_input *current_input = malloc(sizeof(struct name_input));
    if (current_input == NULL) {
        perror("malloc returned null");
        free(hello_fsm);
        exit(1);
    }

    struct name_output *current_output = malloc(sizeof(struct name_output));
    if (current_output == NULL) {
        perror("malloc returned null");
        free(hello_fsm);
        free(current_input);
        exit(1);
    }

    // Set the initial state struct to initial values 
    name_tick(hello_fsm, NULL, current_output);

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
            current_input->i = curr_int;

            // call tick on input 
            tick(hello_fsm, current_input, current_output);

            // interpret the output and print it to the screen
            printf("%d", current_output->o);        
        }

        //reset the machine
        tick(hello_fsm, NULL, current_output);

        printf("\nPlease enter an input stream of 0s and 1s. (max. 50): ");
    }
}
