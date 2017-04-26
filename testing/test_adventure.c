#include <stdio.h>
#include "test_adventure.h"

int main() {

    struct test_adventure_input input;
    struct test_adventure_output output;
    struct test_adventure_state states;

    // TODO run tick with null values to do the reset
    output.result = 0;

    while (output.result == 0) {
        printf("Press s to start the adventure");
        scanf("%c", &(input.decision));
        test_adventure_tick(&states, &input, &output);
    }

    while (output.result == 1) {
        printf("Press the corresponding letter on your keyboard to make your choice: ");
        scanf("%c", &(input.decision));
        test_adventure_tick(&states, &input, &output);
    }

    if (output.result == 2) {
        printf("Thanks for playing :)"):
    }


}
