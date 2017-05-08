#include <stdio.h>
#include "adventure.h"

int main() {

    struct adventure_input input;
    struct adventure_output output;
    struct adventure_state states;

    adventure_tick(&states, NULL, NULL);
    output.result = 0;

    char user_input[50];
    printf("\033[H\033[J");
  
    while (output.result == 0) {
        printf("Press s to start the adventure: ");
        scanf("%s", (user_input));
        input.decision = *user_input;
        printf("\n");
        printf("\033[H\033[J");
        adventure_tick(&states, &input, &output);
    }

    while (output.result == 1) {
        input.decision = '(';
        if (adventure_tick(&states, &input, &output) == NULL) {
            break;
        } 
        printf("Press the corresponding letter on your keyboard to make your choice: ");
        scanf("%s", (user_input));
        input.decision = *user_input;
        printf("\n");
        printf("\033[H\033[J");
        adventure_tick(&states, &input, &output); 
    }
    
    
    printf("\nThanks for playing :) This story was brought to you by SAKE: Don't drink if you underage.");
}
