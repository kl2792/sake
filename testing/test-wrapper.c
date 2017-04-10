#include <stdio.h>
#include "test-wrapper.h"

int main() {
    struct test-wrapper_input i;
    struct test-wrapper_output p;
    struct test-wrapper_state q;
    
    test-wrapper_tick(&q, &i, &p);

    return 0;
}
