#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "queue.h"

// Print out node's contents, excluding data
static void print_node(const char *name, struct node *n) {
	printf("%s: %p %p %p\n", name, n->prev, n, n->next);
}

// Add node
static void node_add(struct node *n, struct node *head) {
	n->next = head->next;
	n->prev = head;
	head->next = head->next->prev = n;
};

// Remove node
static void node_del(struct node *n) {
	n->next->prev = n->prev;
	n->prev->next = n->next;
	free(n);
}

// Check if queue empty
static int queue_empty(struct node *head) {
	if (head->next == head) assert(head->prev == head);
	return head == head->next;
}

// Append to queue; return 0 if no memory
int queue_put(struct node *head, const struct data *data) {
	struct node *new = malloc(sizeof(*new));
	if (!new) return 0;
	node_add(new, head);
	memcpy(&new->data, data, sizeof(*data));
	return 1;
}

// Remove from start of queue, write to data; return 0 if queue empty
int queue_get(struct node *head, struct data *data) {
	if (queue_empty(head)) return 0;
	memcpy(data, &head->next->data, sizeof(*data));
	node_del(head->next);
	return 1;
}

// Return length
int queue_len(struct node *head) {
	int count = 0;

	for (struct node *n = head->next; n != head; n = n->next) {
		count++;
		//fprintf(stderr, "%p %p %p %d\n", n, n->next, n->prev, count);
	}
	//fprintf(stderr, "done\n");
	return count;
}

// Destroy queue
void queue_del(struct node *head) {
	struct node *n, *nxt;
	for (n = head->next, nxt = n->next; n != head; n = nxt, nxt = n->next)
		node_del(n);
	head->next = head->prev = head; // reinitialize head
}
