

class Node:

  def __init__(self, d):
      self.next = None
      self.data = d

# Exercise 2.1
# ============
# Write code to remove duplicates from an unsorted linked list
# How would you solve this problem if a temporary buffer is not allowed

def delete_dups(n):
    elements = set([])
    previous = None
    while n != None:
        if n.data in elements:
            previous.next = n.next
        else:
            elements.add(n.data)
            previous = n
        n = n.next


def delete_dups_no_cache(head):
    if head is None:
         return

    current = head
    while current is not None:
        runner = current
        while runner.next is not None:
            if runner.next.data == current.data:
                runner.next = runner.next.next
            else:
                runner = runner.next
        current = current.next

# Exercise 2.2
# ============
# Implement an algorithm to find the kth to last element of a singly linked list

def nth_to_last(head, k):
    p2 = head
    while k > 0:
        if p2 is None:
            return None
        p2 = p2.next
    if p2 is None:
        return None

    p1 = head
    while p2 is not None:
        p1 = p1.next
        p2 = p2.next
    return p1

# Exercise 2.3
# ============
# Implement an algorithm to delete a node in the middle of a single linked list,
# given only access to that node

def delete_node(node):
    # Assume that the given node always have a subsequent element
    # NOTE: There are different ways to handle if the assumption is broken
    node.data = node.next.data
    node.next = node.next.next

# Exercise 2.4
# ============
# Write code to partition a linked list around a value x, such that all nodes
# less than x come before all nodes greater than or equal to x.
# NOTE: Ask about list stability

def partition(node, x):
    head = node
    tail = node
    while node is not None:
        next = node.next
        if node.data < x:
            # Insert node at head
            node.next = head
            head = node
        else:
            # Insert node at tail
            tail.next = node
            tail = node
        node = next
    tail.next = None
    return head
