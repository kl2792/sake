/* Usage:
 * DEFINE_QUEUE(q);
 * struct data data;
 * ...
 * queue_put(&q, &data); // add to end of queue
 * ...
 * queue_get(&q, &data); // remove from beginning of queue; write to data var
 * ...
 * queue_len(&q); // get length of queue
 * ...
 * queue_destroy(&q); // free all queue elements, reinitialize
 *
 * Note: the struct data type can be redefined to meet user needs.
 * Author: Kai-Zhan Lee
 */

#ifndef __QUEUE_H__
#define __QUEUE_H__

#define DEFINE_QUEUE(name) struct node name = { \
	.next = &name, \
	.prev = &name, \
}

#include <assert.h>
#include <stdlib.h>
#include <string.h>

struct data {
	int filler; // To be defined
};
struct node {
	struct node *next, *prev;
	struct data data;
};

// Queue API
int queue_put(struct node *head, const struct data *data);
int queue_get(struct node *head, struct data *data);
int queue_len(struct node *head);
void queue_del(struct node *head);

#endif
