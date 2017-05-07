#include <stdio.h>
#include "adventure.h"

int main() {

    struct adventure_input input;
    struct adventure_output output;
    struct adventure_state states;

    adventure_tick(&states, NULL, NULL);
    // TODO run tick with null values to do the reset
    output.result = 0;

    char user_input[50];

    while (output.result == 0) {
        printf("Press s to start the adventure: ");
        scanf("%s", (user_input));
        input.decision = *user_input;
        printf("\n");
        adventure_tick(&states, &input, &output);
    }

    while (output.result == 1) {
        printf("Press the corresponding letter on your keyboard to make your choice: ");
        scanf("%s", (user_input));
        input.decision = *user_input;
        printf("\n");
        adventure_tick(&states, &input, &output);
    }

    if (output.result == 2) {
        printf("\nThanks for playing :) This story was brought to you by SAKE: Don't drink if you underage.");
    }


}
