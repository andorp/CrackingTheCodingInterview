from array import array

# Exercise 1.1
# ============
# Implement an algorithm to determine if a string has all unique charachters.
# What if you cannot use additional data structures?

def is_unique_chars(str):
    n = len(str)
    if n > 256:
        return False

    char_set = array('b', [False for c in range(0,256)])
    for i in range(0,n):
        c = ord(str[i])
        if char_set[c]:
            return False
        char_set[c] = True
    return True


print is_unique_chars("aa")

# Exercise 1.3
# ============
# Given two string write a method to decide if on is a permutation of the other.

def is_permutation_naive(s, t):
    if len(s) != len(t):
        return False
    return sorted(s) == sorted(t)


print is_permutation_naive("sdfa", "erss")


def is_permutation(s, t):
    if len(s) != len(t):
        return False

    letters = array('i', [0 for i in range(0,256)])
    for c in s:
        letters[ord(c)] += 1

    for c in t:
        i = ord(c)
        letters[i] -= 1
        if (letters[i] < 0):
            return False

    return True


print is_permutation("abba", "baba")


# Exercise 1.4
# ============
# Write a method to replace all spaces in a string with "%20". You may assume
# that the string has sufficient space at the end of the string to hold the
# additional characters, and that you are given the true length of the string.

def replace_spaces(str, length):
    space_count = 0
    for i in range(0, length):
        if str[i] == ' ':
            space_count += 1
    new_length = length + space_count * 2
    for i in range(length - 1, -1, -1):
        if str[i] == ' ':
            str[new_length - 1] = '0'
            str[new_length - 2] = '2'
            str[new_length - 3] = '%'
            new_length -= 3
        else:
            str[new_length - 1] = str[i]
            new_length -= 1
    return str


print replace_spaces(array('c', "replace spaces again      "), 20)


# Exercise 1.5
# ============
# Implement a method to perfirm basic string compression using the counts of
# repeated characters. For example, the string aabcccccaaa would become a
# a2b1c5a3. If the "compressed" string would not become smaller than the
# original string, your method should return the original string. You
# can assume the string has only upper and lower case letters (a-z)

def count_compression(value):
    if not value:
        return 0
    last = value[0]
    size = 0
    count = 1
    for i in range(1, len(value)):
        if value[i] == last:
            count += 1
        else:
            size += 1 + len(str(count))
            last = value[i]
            count = 1
    size += 1 + len(str(count))
    return size


def compression(value):
    if len(value) < count_compression(value):
        return value
    compressed = []
    last = value[0]
    count = 1
    for i in range(1, len(value)):
        if value[i] == last:
            count += 1
        else:
            compressed.append(str(last))
            compressed.append(str(count))
            last = value[i]
            count = 1
    compressed.append(str(last))
    compressed.append(str(count))
    return ''.join(compressed)


print compression("aabcccccaaa")
print compression("aa")


# Exercise 1.6
# ============
# Given an image represented by an NxN matrix, where each pixel in the image
# is 4 bytes, write a method to rotate the image by 90 degress. Can you
# do this in place?

def rotate(matrix, n):
    for layer in range(0, n / 2):
        first = layer
        last = n - 1 - layer
        for i in range(first, last):
            offset = i - first
            top = matrix[(first,i)]
            matrix[(first,i)] = matrix[(last-offset,first)]
            matrix[(last-offset,first)] = matrix[(last,last-offset)]
            matrix[(last,last-offset)] = matrix[(i,last)]
            matrix[(i,last)] = top

# Exercise 1.8
# ============
# Assume you have a method `isSubstring` which checks if one word is a
# substring of another. Given two strings, s1 and s2, write a code to check
# if s2 is a rotation of s1 using only one call to `isSubstring`

def is_substring(s1, s2):
    return s1.contains(s2)


def is_rotation(s1, s2):
    l = len(s1)
    if l == len(s2) and l > 0:
        return is_substring(s1 + s1, s2)
    return False
