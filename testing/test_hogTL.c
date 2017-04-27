#include <stdio.h>
#include "test_trafficLights.h"

int main() {
    
    struct test_hogTL_input input;
    struct test_hogTL_output output;    
    struct test_hogTL_state states;

    // TODO run tick with null values to do the reset
    //     output.result = 0;
           
    test_hogTL_tick(&states, NULL, &output);

    input.inOne = 1;
    input.inTwo = 0;

    test_hogTL_tick(&states, &input, &output);


    printf("output 1: %c\n", output.outOne);
    printf("output 2: %c\n", output.outTwo);

    //input[int inOne, int inTwo]
    //output[char outOne, char outTwo] 

     

}
