# Two Sum Slow
def twoSumSlow(nums: list[int], target: int) -> list[int]:
    for i in range(len(nums)):
        for j in range(i + 1, len(nums)):
            if nums[i] + nums[j] == target:
                return [i, j]
    return [-1, -1] # Error
    
assert twoSumSlow([3, 2, 4], 6) == [1, 2]

# Two Sum Fast
def twoSumFast(nums: list[int], target: int) -> list[int]:
    dct = {}
    for i, val in enumerate(nums):
        if target - val in dct:
            return [i, dct[target - val]]
        else:
            dct[val] = i
    return [-1, -1] # Error
    
assert twoSumFast([2, 7, 11, 15], 9) == [1, 0]

# Definition for singly-linked list.
class ListNode:
 def __init__(self, val=0, next=None):
  self.val = val
  self.next = next
  
def addTwoNumbers(l1: ListNode, l2: ListNode) -> ListNode:
    answer = ListNode()
    sum_help = 0
    carry = 0
    array_help = []
    while l1 or l2:
        temp = ListNode()
        if l1: 
            sum_help+= l1.val
            l1 = l1.next
        if l2: 
            sum_help+= l2.val
            l2 = l2.next
        sum_help += carry
        carry = 0
        if sum_help >= 10: 
            carry = 1
            sum_help -= 10
        array_help.append(sum_help)
        sum_help = 0
    if carry == 1: array_help.append(carry)
    n = len(array_help)
    array_help[-1] = ListNode(array_help[-1], None)
    for i in range(n-2, -1, -1):
        array_help[i] = ListNode(array_help[i], array_help[i + 1])
    return array_help[0]
    
def testEqualList(l1: ListNode, l2: ListNode) -> bool:
    if l1 == None and l2 == None:
        return True
    elif l1 == None or l2 == None:
        return False
    else:
        if l1.val != l2.val:
            return False
        else:
            return testEqualList(l1.next, l2.next)  

assert testEqualList(addTwoNumbers(ListNode(2, ListNode(4, ListNode(3))), ListNode(5, ListNode(6, ListNode(4)))), ListNode(7, ListNode(0, ListNode(8))))

## Length of Longest Substring
def lengthOfLongestSubstring(s: str) -> int:
    dict_help = {}
    max_count = 0
    start_ind = 0
    for count, i in enumerate(s):
        if i in dict_help:
            start_ind = max(dict_help[i] + 1, start_ind)
        dict_help[i] = count
        max_count = max(max_count, count - start_ind + 1)
    return max_count
    
assert lengthOfLongestSubstring("abcabcbb") == 3


# Definition for a binary tree node.
class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right
            
            
def isValidBST(root: TreeNode) -> bool:
    def isBST(node, minkey, maxkey):
        if node is None: return True
        if node.val < minkey or node.val > maxkey: return False
            
        return isBST(node.left, minkey, node.val - 1) and isBST(node.right, node.val + 1, maxkey)
    return isBST(root, -float("inf"), float("inf"))
    
assert isValidBST(TreeNode(2, TreeNode(1), TreeNode(3))) == True
