#include <stdio.h>
#include "../wrapper.h"

int main() {
    struct wrapper_input i;
    struct wrapper_output p;
    struct wrapper_state q;
    
    wrapper_tick(&q, &i, &p);

    return 0;
}
