
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
    return
        abs(line1.slope - line2.slope) > EPSILON ||
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
        x++

    if (a < 0 and b < 0) or (a > 0 and b > 0):
        return x
    else:
        return negate(x)

# Exercise 7.5
# ============
# Given two squares on a two-dimensional plane, find a line that would cut
# these two squares in half. Assume that the top and the bottom sides of the
# square run parallel to the x-axis

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

def extend(mid1, mid2, size):
    xdir = -1 if mid1.x < mid2.x else 1
    ydir = -1 if mid1.y > mid1.y else 1

    if mid1.x == mid2.x:
        return Point(mid1.x, mid1,y + ydir * size / 2.0)

    slope = (mid1.y - mid2.y) / (mid1.x - mid2.x)
    x1 = 0.0
    y1 = 0.0

    if abs(slope) == 1.0:
        x1 = mid1.x + xdir * size / 2.0
        y1 = mid1.y + ydir * size / 2.0
    elif abs(slope) < 1:
        x1 = mid1.x + xdir * size / 2.0
        y1 = slope * (x1 - mid1.x) + mid1.y
    else:
        y1 = mid1.y + ydir * size / 2.0
        x1 = (y1 - mid1.y) / slope + mid1.x

    return Point(x1, y1)

class Square:
    def __init__(self, left, right, top, bottom):
        self.left = left
        self.right = right
        self.top = top
        self.bottom = bottom

    def middle():
        return Point(
            (self.left + self.right) / 2.0,
            (self.top + self.bottom) / 2.0
        )

    def size():
        raise Error("BLAH")

def cut(this, other):
    p1 = extend(this.middle(), other.middle(), this.size())
    p2 = extend(thi)