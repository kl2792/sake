#include <stdio.h>
#include <assert.h>
#include "bitvector.h"

int main() {
	DECLARE_BITVECTOR(v, 42);
	DECLARE_BITMATRIX(m, 3, 24);

	assert(TRYBIT(v, 0) == TRYBIT(v, 23));
	assert(TRYBIT(v, 0) == 0);

	SETBIT(v, 13);
	assert(TRYBIT(v, 14) == 0);
	assert(TRYBIT(v, 12) == 0);
	assert(TRYBIT(v, 13) == 1);
}
