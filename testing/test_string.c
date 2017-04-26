#include <stdio.h>
#include "test_string.h"

int main() {
    struct test_string_input input;
    struct test_string_output output;
    struct test_string_state states;
    
    test_string_tick(&states, &input, &output);
}
