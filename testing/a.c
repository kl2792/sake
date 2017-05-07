#include <stdio.h>
#include "test_a.h"

int main() {

    struct test_a_input input;
    struct test_a_output output;
    struct test_a_state states;

    // TODO run tick with null values to do the reset
    output.result = 0;

    while (output.result == 0) {
        printf("Press s to start the adventure");
        scanf("%c", &(input.decision));
        test_a_tick(&states, &input, &output);
    }

    while (output.result == 1) {
        printf("Press the corresponding letter on your keyboard to make your choice: ");
        scanf("%c", &(input.decision));
        test_a_tick(&states, &input, &output);
    }

    if (output.result == 2) {
        printf("Thanks for playing :)"):
    }


}
