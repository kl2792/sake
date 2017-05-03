#include <stdio.h>
#include "queue.h"

int main() {
	DEFINE_QUEUE(my_queue);
	struct data d = { .filler = 0 };

	printf("initially\n");
	printf("d: %d\nlen: %u\n\n", d.filler, queue_len(&my_queue));

	queue_put(&my_queue, &d);
	printf("d: %d\nlen: %u\n\n", d.filler, queue_len(&my_queue));

	d.filler = 12;
	queue_put(&my_queue, &d);	queue_put(&my_queue, &d);	queue_put(&my_queue, &d);
	printf("d: %d\nlen: %u\n\n", d.filler, queue_len(&my_queue));

	d.filler = 13;
	printf("d: %d\nlen: %u\n\n", d.filler, queue_len(&my_queue));

	queue_get(&my_queue, &d);
	printf("d: %d\nlen: %u\n\n", d.filler, queue_len(&my_queue));

	queue_del(&my_queue);
	printf("d: %d\nlen: %u\n\n", d.filler, queue_len(&my_queue));
}
