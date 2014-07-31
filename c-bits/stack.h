#ifndef STACK_H
#define STACK_H

typedef struct stack_impl *stack;
typedef void *object;

void   push(stack, object);
object pop (stack);
stack  empty();
size_t size(stack);

#endif
