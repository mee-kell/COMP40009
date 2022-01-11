import Data.Maybe
type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

value :: BinTree a -> a
value (Node v _ _)
  = v

rank :: BinTree a -> Int
rank (Node _ r _)
  = r

children :: BinTree a -> [BinTree a]
children (Node _ _ c)
  = c

-- Find tree t with smallest root and add other tree as a child of t
combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees t1 t2
  | value t1 < value t2 = Node (value t1) (rank t1 + 1) (t2 : children t1)
  | otherwise           = Node (value t2) (rank t2 + 1) (t1 : children t2)

--------------------------------------------------------------
-- PART II

-- Pre: given heap is non-empty
-- Inspect root values and return minimum
extractMin :: Ord a => BinHeap a -> a
extractMin heap
  = minimum (map value heap)
  -- optimum: minimum . map value

-- If either heap is empty, return the other heap
-- If rank t1 < rank t2, let t1 form the head and merge the tail of h1 with h2
-- Vice versa for rank t2 > rank t1
-- If rank t1 == rank t2, combine t1 and t2 then add to merge of two tails
mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps h1 []
  = h1
mergeHeaps [] h2
  = h2
mergeHeaps h1@(t1 : t1s) h2@(t2 : t2s)
  | rank t1 < rank t2 = t1 : mergeHeaps t1s h2
  | rank t2 < rank t1 = t2 : mergeHeaps h1 t2s
  | rank t1 == rank t2 = mergeHeaps [combineTrees t1 t2] (mergeHeaps t1s t2s)

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert val
  = mergeHeaps [Node val 0 []]

-- Reverse the list of root children and merge with remaining trees in heap
deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin heap
  = mergeHeaps flippedHeap heap'
  where
    valueToTree = zip (map value heap) heap
    root = fromJust (lookup (extractMin heap) valueToTree)
    flippedHeap = reverse (children root)
    heap' = filter (/= root) heap

remove :: Eq a => a -> BinHeap a -> BinHeap a
remove
  = undefined

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin
  = undefined

-- Insert elements of list into a binomial heap one by one
-- Repeatedly extract and delete the minimum element until heap is empty
binSort :: Ord a => [a] -> [a]
binSort list
  = binSort' (foldl (flip insert) [] list)
  where
    binSort' []
      = []
    binSort' heap
      = extractMin heap : binSort' (deleteMin heap)

--------------------------------------------------------------
-- PART III

-- Find the largest rank in the heap R
-- Check if the rth rank exists, append to list
-- Decrement R, until we reach R < 0 returns []
toBinary :: BinHeap a -> [Int]
toBinary heap
  = buildBinary (maximum heapRanks)
  where
    heapRanks = map rank heap
    buildBinary (-1)
      = []
    buildBinary r
      | r `elem` heapRanks = 1 : buildBinary (r - 1)
      | otherwise          = 0 : buildBinary (r - 1)

-- Recursively traverse both numbers from right to left
-- Check what carry-in input is
-- Define outputs (sum, carry-out)
-- Append output sum to a list, feed carry-out into recursive
binarySum :: [Int] -> [Int] -> [Int]
binarySum num1 num2
  = binarySum' num1 num2 0 []
  where
    binarySum' :: [Int] -> [Int] -> Int -> [Int] -> [Int]
    binarySum' [] [] cin sum
      = cin : sum
    binarySum' [] n2 cin sum
      = binarySum' [0] n2 cin sum
    binarySum' n1 [] cin sum
      = binarySum' n1 [0] cin sum
    binarySum' n1 n2 cin sum
      | b1 + b2 == 0 && cin == 0 = binarySum' f1 f2 0 (0 : sum)
      | b1 + b2 == 1 && cin == 0 = binarySum' f1 f2 0 (1 : sum)
      | b1 + b2 == 2 && cin == 0 = binarySum' f1 f2 1 (0 : sum)
      | b1 + b2 == 0 && cin == 1 = binarySum' f1 f2 0 (1 : sum)
      | b1 + b2 == 1 && cin == 1 = binarySum' f1 f2 1 (0 : sum)
      | b1 + b2 == 2 && cin == 1 = binarySum' f1 f2 1 (1 : sum)
      | otherwise                = sum
      where
        (f1, b1) = (init n1, last n1)
        (f2, b2) = (init n2, last n2)

-- More elegant solution is possible here...

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []],
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []],
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []],
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]


