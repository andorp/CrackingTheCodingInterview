
# Exercise 7.3
# ============
# Given two lines on a Cartesian plane, determine whatever the two lines would
# intersect.
class Line:
    def __init__(self, s, y):
        self.slope = s
        self.yintercept = y


EPSILON = 0.0000001


def intersect(line1, line2):
    return abs(line1.slope - line2.slope) > EPSILON or \
           abs(line1.yintercept - line2.yintercept < EPSILON)


# Exercise 7.4
# ============
# Write method to implement the multiply, subsctract, and devide
# operations for integers. Use only the add operator.
def negate(a):
    neg = 0
    d = 1 if a < 0 else -1
    while a != 0:
        neg += d
        a += d
    return neg


def substract(a, b):
    return a + negate(b)


def multiply(a, b):
    if a < b:
        return multiply(b, a)

    sum = 0
    i = abs(b)
    while i > 0:
        sum += a
        i -= 1

    if b < 0:
        sum = negate(sum)

    return sum


def devide(a, b):
    if b == 0:
        raise Error("division by zero")
    absa = abs(a)
    absb = abs(b)
    product = 0
    x = 0
    while product + absa <= absb:
        product += absb
        x += 1

    if (a < 0 and b < 0) or (a > 0 and b > 0):
        return x
    else:
        return negate(x)

# Exercise 7.7
# ============
# Design an algorithm to find the kth number such that the only  prime
# factors are 3,5, and 7

class Queue:
    def __init__(self):
        self.queue = []

    def enqueue(self, item):
        self.queue.append(item)

    def dequeue(self):

        if self.is_empty():
            return None

        item = self.queue[0]
        self.queue.pop(0)
        return item

    def is_empty(self):
        return not bool(self.queue)

    def peek(self):

        if self.is_empty():
            return None

        return self.queue[0]

def test_queue():
    q = Queue()
    print q.is_empty()
    q.enqueue(1)
    print q.peek()
    print q.is_empty()
    print q.dequeue()
    print q.is_empty()

def get_magic_kth_number(k):

    if k < 0:
        return 0

    val = 0
    queue3 = Queue()
    queue5 = Queue()
    queue7 = Queue()
    queue3.enqueue(1)

    for i in range(0,k + 1):
        v3 = queue3.peek()
        v5 = queue5.peek()
        v7 = queue7.peek()

        val = min(filter(lambda x: x is not None, [v3,v5,v7]))
        if val == v3:
            queue3.dequeue()
            queue3.enqueue(3 * val)
            queue5.enqueue(5 * val)
        elif val == v5:
            queue5.dequeue()
            queue5.enqueue(5 * val)
        elif val == v7:
            queue7.dequeue()
        queue7.enqueue(7 * val)

    return val

def test_magic():
    print get_magic_kth_number(0)
    print get_magic_kth_number(1)
    print get_magic_kth_number(2)
    print get_magic_kth_number(10)
    print get_magic_kth_number(100)
