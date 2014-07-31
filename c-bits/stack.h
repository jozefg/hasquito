#ifndef STACK_H
#define STACK_H

typedef struct stack_impl *stack;
typedef void *object;

void   push(stack, object);
object pop (stack);
stack  empty();
void   free_stack(stack);
size_t size(stack);

#endif
