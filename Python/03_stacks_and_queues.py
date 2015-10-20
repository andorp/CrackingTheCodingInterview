from array import array

class Node:
    def __init__(self, d):
        self.data = d
        self.next = None


class Stack:
    def __init__(self):
        self.top = None

    def is_empty(self):
        return bool(self.top)

    def push(self, data):
        new_top = Node(data)
        new_top.next = self.top
        self.top = new_top
    
    def pop(self):
        if self.top:
            data = self.top.data
            self.top = self.top.next
            return data
        else:
            return None

    def peek(self):
        if self.top:
            return self.top.data
        else:
            return None


class Queue:
    def __init__(self):
        self.first = None
        self.last = None

    def enqueue(self, data):
        node = Node(data)
        if self.first is None:
            self.first = node
            self.last = self.first
        else:
            self.last.next = node
            self.last = node

    def dequeue(self):
        if self.first:
            item = self.first.data
            self.first = self.first.next
            if not self.first:
                self.last = None
            return item
        else:
            return None

# Exercise 3.1
# ============
# Describe how you could use a single array to implement three stacks

EMPTY_POINTER = -1
stack_size    = 100
stack_buffer  = array('i', [0 for i in range(0, stack_size * 3)])
stack_pointer = array('i', [EMPTY_POINTER, EMPTY_POINTER, EMPTY_POINTER])


class EmptyStackException(Exception):
    pass


class FullStackException(Exception):
    pass


def push_three(stack_num, item):
    pointer = stack_pointer[stack_num]
    if pointer == stack_size:
        raise FullStackException()
    inc_stack_pointer(stack_num)
    set_item_at_stack(stack_num, item)


def pop_three(stack_num):
    if stack_pointer[stack_num] == EMPTY_POINTER:
        raise EmptyStackException()
    item = get_item_at_stack(stack_num)
    dec_stack_pointer(stack_num)
    return item


def absolute_pos(stack_num):
    return stack_size * stack_num + stack_pointer[stack_num]


def inc_stack_pointer(stack_num):
    stack_pointer[stack_num] += 1


def dec_stack_pointer(stack_num):
    stack_pointer[stack_num] -= 1


def set_item_at_stack(stack_num, item):
    position = absolute_pos(stack_num)
    stack_buffer[position] = item

def get_item_at_stack(stack_num):
    position = absolute_pos(stack_num)
    return stack_buffer[position]

# Exercise 3.2
# ============
# How would you design a stack, in addition to push and pop, also has a
# function min which returns the minimum element? Push, pop, and min
# should be oparate on O(1) time?

class MinStack:
    def __init__(self):
        self.value_stack = Stack()
        self.min_stack = Stack()

    def push(self, item):
        min = self.min_stack.peek()
        if not min or item < min:
            self.min_stack.push(item)
        self.value_stack.push(item)

    def pop(self):
        item = self.value_stack.pop()
        if item == self.min_stack.peek():
            self.min_stack.pop()
        return item

    def min(self):
        return self.min_stack.peek()

