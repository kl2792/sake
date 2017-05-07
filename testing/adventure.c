#include <stdio.h>
#include "adventure.h"

int main() {

    struct adventure_input input;
    struct adventure_output output;
    struct adventure_state states;

    // TODO run tick with null values to do the reset
    output.result = 0;

    char user_input[50];

    while (output.result == 0) {
        printf("Press s to start the adventure: ");
        scanf("%s", user_input);
        input.decision = user_input[0];
        printf("user input: %c\n", input.decision);
        adventure_tick(&states, &input, &output);
        printf("after tick\n\n");
    }

    printf("After first state moving onto main loop");

    while (output.result == 1) {
        printf("Press the corresponding letter on your keyboard to make your choice: ");
        scanf("%s", (user_input));
        input.decision = *user_input;


        //scanf("%c", &(input.decision));
        adventure_tick(&states, &input, &output);
    }

    if (output.result == 2) {
        printf("Thanks for playing :)");
    }


}
