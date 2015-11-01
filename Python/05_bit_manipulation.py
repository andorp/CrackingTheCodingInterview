# Exercise 5.1
# ============
# You are given two 32-bit numbers, N and M, and two positions i and j.
# Write a method to insert M into N such that M starts at bit j and ends at bit i.
# You can assume that there are at least 5 bits between j and i.
# EXAMPLE
# Input N = 100000000000 M = 10011, i = 2, j = 6
# Output N = 10001001100

def update_bits(n, m, i, j):
    # Create a mask to clear bits i through j in n
    all_ones = ~0
    # Ones before position j, then 0s left = 11100000
    left = all_ones << (j + 1)
    # Ones after position i. right = 00000011
    right = ((1 << i) - 1)
    # All 1s, expect fro 0s between i and j.
    mask = left | right
    # Clear bits j through i then pu m there
    n_cleared = n & mask
    m_shifted = m << i
    # OR them and we are done
    result = n_cleared | m_shifted
    return result

print bin(update_bits(0b10000000000, 0b10011, 2, 6))

# Exercise 5.2
# ============
# Given a real number between 0 and 1 (eg 0.72) that is passed in as a double, print
# the binary representation. If the number cannot be represented accurately in binary
# with at most 32 characters, print "ERROR"
def print_binary(num):
    if num >= 1 or num < 0:
        return "ERROR"

    binary = ['.']
    while num > 0:
        if len(binary) > 32:
            return "ERROR:"
        r = num * 2
        if r >= 1:
            binary.append('1')
            num = r - 1
        else:
            binary.append('0')
            num = r
    return ''.join(binary)

print print_binary(0)
print print_binary(0.5)
print print_binary(0.25)
print print_binary(0.16)

# Exercise 5.5
# ============
# Write a function to determine the number of bits you would need to flip
# to convert integer A to integer B

def bit_swap_required(a, b):
    count = 0
    c = a ^ b
    while c != 0:
        count += 1
        c = c & (c - 1)
    return count

bit_swap_required(20, 13)

# Exercise 5.6
# ============
# Write a program to swap odd and even bits in an integer with as few
# instructions as possible.

def swap_odd_even_bits(x):
    return ((x & 0xAAAAAAAA) >> 1) | ((x & 0x55555555) << 1)

print bin(14)
print bin(swap_odd_even_bits(14))

# Exercise 5.7
# ============
# An array A contains all the integers from 0 through n, expect for one number
# which is missing. In this problem, we cannot access an entire integer in A with
# a single operation. The elements of A are represented in binary, and the only
# operation we can use to access them is "fetch jth bit of A[i]", which takes
# constant time. Write code to find the missing integer. Can you do it in O(n) time?

def bit_integer(n):
    def at(i):
        return n & (1 << i)
    return at

def find_missing(array):
    return find_missing_rec(array, 0)

MAX_SIZE = 32

def find_missing_rec(input, column):
    if column > MAX_SIZE:
        return 0

    one_bits = []
    zero_bits = []

    for t in input:
        (zero_bits if t(column) == 0 else one_bits).append(t)

    if len(zero_bits) <= len(one_bits):
        v = find_missing_rec(zero_bits, column + 1)
        return (v << 1) | 0
    else:
        v = find_missing_rec(one_bits, column + 1)
        return (v << 1) | 1

def test_find_missing():
    array = []
    for i in range(0, 32):
        if i != 3:
            array.append(bit_integer(i))
    print bin(find_missing(array))

test_find_missing()

# Exercise 5.8
# ============
# A monochrome screen is stored as a single array of bytes, allowing eight
# consequitive pixels to be stored in one byte. The screen has width w, where
# w is divisable by 8 (that is no byte will be split across).
# Implement a drawHorizontalLine function(screen, width, x1, x2, y) which
# draws a horizontal line from x1 to x2

BYTE_WIDTH = 8

def draw_horizontal_line(screen, width, x1, x2, y):

    def set_byte(b,v):
        screen[(width / BYTE_WIDTH) * y + b] = v

    def screen_byte_and_bit_pos(v):
        return (v / BYTE_WIDTH, BYTE_WIDTH - (v % BYTE_WIDTH))

    if x1 == x2:
        # Draw point
        (point_byte, point_offset) = screen_byte_and_bit_pos(x1)
        set_byte(point_byte, 1 << point_offset)
        return
    if x1 < x2:
        start = x1
        end = x2
    else:
        start = x2
        end = x1
    # Same byte
    (start_byte, start_offset) = screen_byte_and_bit_pos(start)
    (end_byte, end_offset) = screen_byte_and_bit_pos(end)
    if start_byte == end_byte:
        right = ~(0xFF >> (end_offset + 1))
        left = (0xFF >> start_offset)
        # line = ~(right | left)
        line = (right | left)
        set_byte(start_byte, line)
        return

    # Fill start byte e.g: 00000111
    print start_offset
    set_byte(start_byte, (0xFF >> (BYTE_WIDTH - start_offset)))
    # Fill end byte e.g: 111100000
    print end_offset
    set_byte(end_byte, ~(0xFF >> (end_offset + 1)))
    # Fill bytes between start and end
    for b in range(start_byte + 1, end_byte - 1):
        set_byte(b, 0xFF) # Fill with ones


def test_draw_horizontal_line():
    array = [0xFF, 0xFF]
    draw_horizontal_line(array, 1, 6, 6, 0)
    print map(lambda x:bin(x), array)

test_draw_horizontal_line()