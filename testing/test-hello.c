#include <stdio.h>
#include "../ocaml/test-hello.h"

int main() {
    struct test-hello_input i;
    struct test-hello_output p;
    struct test-hello_state q;
    
    test-hello_tick(&q, &i, &p);

    return 0;
}
