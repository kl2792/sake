#include <stdio.h>
#include "test_nestedWhile.h"
int main() {
struct test_nestedWhile_input i;
struct test_nestedWhile_state s;
test_nestedWhile_tick(&s, NULL, NULL); 
test_nestedWhile_tick(&s, &i, NULL); 
return 0;}
