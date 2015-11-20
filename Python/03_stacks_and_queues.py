from array import array

class Node:
    def __init__(self, d):
        self.data = d
        self.next = None

    def __str__(self):
        return str(self.data) + ' :> ' + str(self.next)


class EmptyStackException(Exception):
    pass


class FullStackException(Exception):
    pass


class Stack:
    def __init__(self):
        self.MAX_ELEM_COUNT = 30
        self.top = None
        self.no_of_items = 0

    def is_empty(self):
        return not(bool(self.top))

    def is_full(self):
        return self.no_of_items == self.MAX_ELEM_COUNT

    def push(self, data):
        if self.is_full():
            raise FullStackException()
        self.no_of_items += 1
        new_top = Node(data)
        new_top.next = self.top
        self.top = new_top

    def pop(self):
        if self.top:
            self.no_of_items -= 1
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

    def __str__(self):
        return 'Stack ' + str(self.top)


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

# Exercise 3.3
# ============
# Set of stacks ...
# FOLLOWING
# Implement a popAt(int index) which permirms a pop operation on a
# specific sub-stack

class SetOfStacks:
    def __init__(self):
        # Invariant: At least one stack should be in the list of stacks
        # Invariant: There is only one empty stack, which is the active one
        self.stacks = [Stack()]

    def _is_last_stack(self):
        return len(self.stacks) == 1

    def _remove_last_stack(self):
        if self._is_last_stack():
            return 
        self.stacks.pop()

    def _get_last_stack(self):
        return self.stacks[-1]

    def _get_last_stack_for_pop(self):
        last_stack = self._get_last_stack()
        if self._is_last_stack():
            return last_stack
        if not last_stack.is_empty():
            return last_stack
        else:
            self._remove_last_stack()
            return self._get_last_stack()

    def pop(self):
        last_stack = self._get_last_stack_for_pop()
        return last_stack.pop()

    def _get_last_stack_for_push(self, item):
        last_stack = self._get_last_stack()
        if last_stack.is_full():
            self._add_stack()
            last_stack = self._get_last_stack()
        last_stack.push(item)

    def pop_at(self, index):
        item = self.stacks[index].pop()
        if self.stacks[index].is_empty():
            del self.stacks[index]
        return item

# Exercise 3.4
# ============
# Hanoi towers

def moveDisks(n, origin, destination, buffer):
    # Base case
    if n < 1:
        return

    moveDisks(n - 1, origin, buffer, destination)
    moveTop(origin, destination)
    moveDisks(n - 1, buffer, destination, origin)


def moveTop(origin, destination):
    destination.push(origin.pop())


def hanoi(n):
    t1 = Stack()
    t2 = Stack()
    t3 = Stack()
    for i in range(n,0,-1):
        t1.push(i)
    moveDisks(n, t1, t3, t2)
    print t1
    print t2
    print t3


# Exercise 3.5
# ============
# Implement a MyQueue class which implements a queue using two stacks

class MyQueue:
    def __init__(self):
        # Invariant: The queue is empty if the front is empty
        self.front = Stack()
        self.rear = Stack()

    def _checkf(self):
        if self.front.is_empty():
            while not self.rear.is_empty():
                self.front.push(self.rear.pop())

    def is_empty(self):
        self._checkf()
        return self.front.is_empty()

    def enqueue(self, item):
        self.rear.push(item)

    def dequeue(self):
        self._checkf()
        return self.front.pop()

    def __str__(self):
        return 'Queue: Front {' + str(self.front) + '} Rear {' + str(self.rear) + '}'


# Exercise 3.6
# ============
# Write a program to sort a tack in ascending order. You may use at one
# additional stack to hold items. The stack supports the following operations:
# pop, peek, push, is_empty

def two_stack_sort(stack):
    result = Stack()
    while not stack.is_empty():
        tmp = stack.pop()
        while not result.is_empty() and result.peek() > tmp:
            stack.push(result.pop())
        result.push(tmp)
    return result

