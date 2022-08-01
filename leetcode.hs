import Data.Map (Map)
import qualified Data.Map as Map
-- Assertions for test
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x

-- Two Sum
-- Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.
-- The slow method works in O(n^2). For each element, iterate over the remaining elements to see if another number that adds to target exists
twoSumSlow :: [Int] -> Int -> [Int]
twoSumSlow ls target = twoSumSlowHelper ls target 0

twoSumSlowHelper :: [Int] -> Int -> Int -> [Int]
twoSumSlowHelper (x:xs) target index = if b == -1
										then twoSumSlowHelper xs target (index + 1)
										else [index, b]
										where b = twoSumSlowIterator xs (target - x) (index + 1)
twoSumSlowHelper _ _ _ = [-1, -1] -- Not Found

twoSumSlowIterator :: [Int] -> Int -> Int -> Int
twoSumSlowIterator (x:xs) target index = if target - x == 0
										then index
										else twoSumSlowIterator xs target (index + 1)
twoSumSlowIterator _ _ _ = -1 -- Not Found

-- The faster solution uses a hash map and runs in O(n) time with O(1) space
twoSumFast :: [Int] -> Int -> [Int]
twoSumFast ls target = twoSumFastHelper ls target Map.empty 0

twoSumFastHelper :: [Int] -> Int -> Map Int Int -> Int -> [Int]
twoSumFastHelper (x:xs) target my_map index = case Map.lookup (target - x) my_map of 
	Just val -> [index, val]
	_ -> twoSumFastHelper xs target (Map.insert x index my_map) (index + 1)
																							
	
twoSumFastHelper _ _ _ _ = [-1, -1] -- Not Found

-- ListNode -> Val Next
data ListNode a = Val a (ListNode a)
				| Empty
	deriving (Show, Eq)
	

-- Add the two numbers and return the sum as a linked list.	
addTwoNumbers :: ListNode Int -> ListNode Int -> ListNode Int
addTwoNumbers l1 l2 = addTwoNumbersHelper l1 l2 0

addTwoNumbersHelper :: ListNode Int -> ListNode Int -> Int -> ListNode Int
addTwoNumbersHelper Empty Empty 0 = Empty
addTwoNumbersHelper Empty Empty carry_val = Val carry_val Empty
addTwoNumbersHelper Empty (Val val next) carry_val = Val f_val (addTwoNumbersHelper Empty next new_carry)
													where 
														f_val = (carry_val + val) `mod` 10
														new_carry = (carry_val + val) `div` 10
addTwoNumbersHelper (Val val next) Empty carry_val = Val f_val (addTwoNumbersHelper Empty next new_carry)
													where 
														f_val = (carry_val + val) `mod` 10
														new_carry = (carry_val + val) `div` 10													
addTwoNumbersHelper (Val val next) (Val val1 next1) carry_val = Val f_val (addTwoNumbersHelper next next1 new_carry)
													where 
														f_val = (carry_val + val + val1) `mod` 10
														new_carry = (carry_val + val + val1) `div` 10
											
lengthOfLongestSubstring :: String -> Int
lengthOfLongestSubstring s = substringHelper s 0 0 0 Map.empty

substringHelper :: String -> Int -> Int -> Int -> Map Char Int -> Int
substringHelper (x:xs) index max_length start_ind my_map = case Map.lookup x my_map of 
	Just val -> substringHelper xs (index + 1) (max max_length (index - new_start_ind + 1)) new_start_ind (Map.insert x index my_map) 
				where new_start_ind = max (val + 1) start_ind
	_ -> substringHelper xs (index + 1) (max max_length (index - start_ind + 1)) start_ind (Map.insert x index my_map)
substringHelper [] index max_length start_ind my_map = max_length

-- TreeNode -> Val Left Right
data TreeNode a = Val1 a (TreeNode a) (TreeNode a)
				| Empty1
	deriving (Show, Eq)

isValidBST :: TreeNode Int -> Bool
isValidBST tree = isBSTHelper tree ((-2)^31 - 1) (2^31)

isBSTHelper :: TreeNode Int -> Int -> Int -> Bool
isBSTHelper Empty1 _ _ = True
isBSTHelper (Val1 val lefttree righttree) minkey maxkey = if val < minkey || val > maxkey 
													   then False 
													   else isBSTHelper lefttree minkey (val - 1) && isBSTHelper righttree (val + 1) maxkey
	

main = do 
	putStrLn (assert ((twoSumSlow [3, 2, 4] 6) == [1, 2]) "TwoSumSlow Passed")
	putStrLn (assert ((twoSumFast [2,7,11,15] 9) == [1,0]) "TwoSumFast Passed")
	putStrLn (assert ((addTwoNumbers (Val 2 (Val 4 (Val 3 Empty))) (Val 5 (Val 6 (Val 4 Empty)))) == (Val 7 (Val 0 (Val 8 Empty)))) "addTwoNumbers Passed")
	putStrLn (assert ((lengthOfLongestSubstring "abcabcbb") == 3) "lengthofLongestSubstring Passed")
	putStrLn (assert ((isValidBST (Val1 2 (Val1 1 Empty1 Empty1) (Val1 3 Empty1 Empty1))) == True) "isValidBST Passed")