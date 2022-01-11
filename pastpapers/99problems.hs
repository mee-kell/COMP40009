{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- 1. Find the last element of a list.
myLast :: [a] -> a
-- myLast = last
myLast [x]
  = x
myLast (x : xs)
  = myLast xs

-- 2. Find the last but one element of a list.
myButLast :: [a] -> a
myButLast
  = last . init

-- 3. Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt xs k
  = (last . take k) xs
-- Or: xs !! (k - 1)

-- 6. Find out whether a list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs
  = xs == reverse xs

-- 7. Flatten a nested list by replacing each list with its elements.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List nested)
  = concatMap flatten nested
flatten (Elem x)
  = [x]

-- 9. Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they are placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack []
  = []
pack (x : xs)
  = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)