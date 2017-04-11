#include <stdio.h>
#include "test_hello.h"

int main() {
    struct test_hello_input i;
    struct test_hello_output p;
    struct test_hello_state q;
    
    test_hello_tick(&q, &i, &p);

    return 0;
}
