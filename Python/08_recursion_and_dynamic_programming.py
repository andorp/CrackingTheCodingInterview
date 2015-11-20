
# Exercise 9.1
# ============
# A child is running a staircase with n steps, and can hop either 1 step,
# 2 steps, or 3 steps at a time. Implement a method to count how many
# possible ways the child can run up the stairs

class CountWays:
    def __init__(self):
        self.dp = {}

    def _count(selffff, n):
        if n < 0:
            return 0
        elif n == 0:
            return 1
        elif n in self.dp:
            return self.dp[n]
        else:
            steps = self._count(n - 1) + \
                    self._count(n - 2) + \
                    self._count(n - 3)
            self.dp[n] = steps
            return steps

    def __call__(self, n):
        return self._count(n)

def test_count_ways():
    count_ways = CountWays()
    print count_ways(1)
    print count_ways(2)
    print count_ways(3)
    print count_ways(10)

# Exercise 9.2
# ============
# Imagine a robot sitting on the left corner of an X by Y grid. The robot
# can only move in two directions: right and down. How many paths are there
# for the robot to go from (0,0) to (X,Y)?
#
# Imagine certain spots are "off limits", such that the robot cannot
# step on them. Design an algorithm to find a path for the robot
# from top left to bottom right.

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __str__(self):
        return "Point({x},{y})".format(x=self.x, y=self.y)

    def __eq__(self, other):
        return type(self) == type(other) and \
               self.x == other.x and \
               self.y == other.y

def is_free(x,y):
    return True

def get_path(x, y, path, cache):
    if x < 0 or y < 0 or not is_free(x,y):
        return False
    p = Point(x,y)
    if p in cache:
        return cache[p]

    is_at_origin = x == 0 and y == 0
    success = False
    if is_at_origin or \
       get_path(x, y - 1, path, cache) or \
       get_path(x - 1, y, path, cache):
       path.add(p)
       success = True
    cache[p] = success
    return success

# Exercise 9.3
# ============
# A magic index in an array A[1..n-1] is defined to be an index such that
# A[i] = i. Given a stored array of distinct integers, write a method
# to find a magic index, if one exist in array.
#
# What if the values are not distint?

def magic_fast(array, start, end):
    if start < 0 or end > len(array) or end < start:
        return None
    mid = (start + end) / 2
    if array[mid] == mid:
        return mid
    elif array[mid] > mid:
        return magic_fast(array, start, mid - 1)
    else:
        return magic_fast(array, mid + 1, end)

def magic(array, start, end):
    # Base cases
    if start < 0 or end < start or end > len(array):
        return None
    
    # Check middle
    mid_index = (start + end) / 2
    mid_value = array[mid_index]
    if mid_index == mid_value:
        return mid_index

    # Check left
    left_index = min(mid_index - 1, mid_value)
    left = magic(array, start, left_index)
    if left is not None:
        return left

    # Check right
    right_index = max(mid_index + 1, mid_value)
    right = magic(array, right_index, end)
    return right

# Exercise 9.4
# ============
# Write a method to return al the subsets of a set

def get_subsets(set, index=0):
    all_subsets = None
    if len(set) == index:
        # Base case, add empty set
        all_subsets = [[]]
    else:
        all_subsets = get_subsets(set, index + 1)
        item = set[index]
        more_subsets = []
        for subset in all_subsets:
            new_subset = []
            new_subset.extend(subset)
            new_subset.append(item)
            more_subsets.append(new_subset)
        all_subsets.extend(more_subsets)
    return all_subsets

def test_subsets():
    print get_subsets([])
    print get_subsets([1])
    print get_subsets([1,2,3])

def get_subsets2(set):
    all_subsets = []
    _2n = 1 << len(set) # 2 ^^ n
    for k in range(0,_2n):
        subset = convert_int_to_set(k, set)
        all_subsets.append(subset)
    return all_subsets

def convert_int_to_set(x, set):
    subset = []
    index = 0
    k = x
    while k > 0:
        if k & 1:
            subset.append(set[index])
        k = k >> 1
        index += 1
    return subset

def test_subsets2():
    print get_subsets2([])
    print get_subsets2([1])
    print get_subsets2([1,2,3])

# Exercise 9.5
# ============
# Write a methodto compute all permutations of a string of
# unique characters

def get_perms(str):
    permutations = []
    if str == '':
        permutations.append("")
        return permutations
    first = str[0]
    remainder = str[1:]
    words = get_perms(remainder)
    for word in words:
        for j in range(0, len(word) + 1):
            s = insert_char_at(word, first, j)
            permutations.append(s)
    return permutations

def insert_char_at(str, char, j):
    preffix = str[:j]
    suffix = str[j:]
    return preffix + char + suffix

def test_get_perms():
    print get_perms("")
    print get_perms("a")
    print get_perms("ab")
    print get_perms("abcde")

# Exercise 9.6
# ============
# Implement an algorithm to print all valid combinations of
# n-pairs of parenthesis

def add_params(list, left_rem, right_rem, chars, count):
    if left_rem < 0 or right_rem < left_rem:
        return
    if left_rem == 0 and right_rem == 0:
        s = ''.join(chars)
        list.append(s)
    else:
        if left_rem > 0:
            chars[count] = '('
            add_params(list, left_rem - 1, right_rem, chars, count + 1)
        if right_rem > 0:
            chars[count] = ')'
            add_params(list, left_rem, right_rem - 1, chars, count + 1)

def generate_parens(n):
    chars = [' '] * (2 * n)
    list = []
    add_params(list, n, n, chars, 0)
    return list

def test_generate_parens():
    print generate_parens(0) # [""]
    print generate_parens(1) # [()]
    print generate_parens(2) # [(()), ()()]
    print generate_parens(3)

# Exercise 9.7
# ============
# Given an infinite number of quarters (25 cents), dimes (10 cents),
# nickels (5 cents), and pennies (1 cent), write code to calculate
# the number of ways of represeenting n cents

def make_change(amount):
    denoms = [25, 10, 5, 1]
    dict = {}
    return make_change_alg(amount, denoms, 0, dict)

def make_change_alg(amount, denoms, index, dict):
    if get(dict, amount, index):
        return dict[amount][index]

    if index >= len(denoms) - 1:
        return 1

    denomAmount = denoms[index]
    ways = 0
    i = 0
    while i * denomAmount <= amount:
        amountRemaining = amount - i * denomAmount
        ways += make_change_alg(amountRemaining, denoms, index + 1, dict)
        i += 1
    set(dict, amount, index, ways)
    return ways

def get(dict, idx1, idx2):
    if idx1 in dict and idx2 in dict[idx1]:
        return dict[idx1][idx2]
    else:
        return None

def set(dict, idx1, idx2, val):
    if idx1 not in dict:
        dict[idx1] = {}
    dict[idx1][idx2] = val

def test_make_change():
    print make_change(1)
    print make_change(10)
    print make_change(50)
    print make_change(100)

# Exercise 9.9
# ============
# Write an algorithm to print all ways of arranging eight queens on an
# 8x8 chess board so that none of them share the same row, column or diagonal
# In this case diagonal means all diagonals, not just the two
# bisect the rows.

def coord(col,row):
    return (col,row)

def col(c):
    return c[0]

def row(c):
    return c[1]

def same_row(c1, c2):
    return row(c1) == row(c2)

def same_column(c1, c2):
    return col(c1) == col(c2)

def same_diagonal(c1, c2):
    dc = abs(col(c1) - col(c2))
    dr = abs(row(c1) - row(c2))
    return dc == dr

GRID_SIZE = 8

def place_queens():
    queens = [None] * GRID_SIZE
    result = []
    place_queens_alg(0, queens, result)
    return result

def place_queens_alg(row, queens, result):
    if row == GRID_SIZE:
        result.append(queens[:])
    else:
        for col in range(0, GRID_SIZE):
            if check_valid(queens, row, col):
                queens[row] = coord(col, row)
                place_queens_alg(row + 1, queens, result)

def check_valid(queens, row, col):
    for r in range(0, row):
        queen = queens[r]
        pos = coord(col, row)
        if same_row(queen, pos) or \
           same_column(queen, pos) or \
           same_diagonal(queen, pos):
           return False
    return True

def test_queens():
    assert same_column(coord(1,3), coord(1,5)) is True, "#1"
    assert same_column(coord(1,3), coord(2,5)) is False, "#2"
    assert same_row(coord(1,2), coord(2,2)) is True, "#3"
    assert same_row(coord(1,1), coord(2,2)) is False, "#4"
    assert same_diagonal(coord(2,2), coord(1,1)) is True, "#6"
    assert same_diagonal(coord(2,1), coord(1,2)) is True, "#7"
    assert same_diagonal(coord(1,1), coord(3,3)) is True, "#8"
    assert same_diagonal(coord(1,1), coord(2,3)) is False, "#9"
    print place_queens()

# Exercise 9.10
# =============
# You have a stack of n boxes with widths wi, heights him and depths di
# ...

class Box:
    def __init__(self, w, h, d):
        self.width = w
        self.height = h
        self.depth = d

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return "Box({w}, {h}, {d})".format(
            w=self.width,
            h=self.height,
            d=self.depth)

def can_above(box1, box2):
    if not box2:
        return True
    else:
        return box1.width > box2.width and \
               box1.depth > box2.depth

def stack_height(boxes):
    return sum(map(lambda box: box.height, boxes))

def create_stack_memo(boxes, bottom, stack_map):
    if bottom is not None and bottom in stack_map:
        return stack_map[bottom]

    max_height = 0
    max_stack = None
    for box in boxes:
        if can_above(box, bottom):
            new_stack = create_stack_memo(boxes, box, stack_map)
            new_height = stack_height(new_stack)
            if new_height > max_height:
                max_stack = new_stack
                max_height = new_height

    if max_stack is None:
        max_stack = []

    if bottom is not None:
        max_stack.insert(0, bottom)

    stack_map[bottom] = max_stack
    return max_stack

def create_stack(boxes):
    return create_stack_memo(boxes, None, {})

def test_create_stack():
    print create_stack([])
    print create_stack([Box(1,1,1)])
    print create_stack([Box(1,1,1), Box(2,2,2)])
    print create_stack([Box(2,2,2), Box(1,1,1)])
