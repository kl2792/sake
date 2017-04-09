#include <stdio.h>
#include "../ocaml/test-42.h"

int main() {
    struct test-42_input i;
    struct test-42_output p;
    struct test-42_state q;
    
    test-42_tick(&q, &i, &p);

    return 0;
}
