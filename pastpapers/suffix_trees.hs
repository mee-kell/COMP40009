data SuffixTree = Leaf Int | Node [(String, SuffixTree)]
                deriving (Eq, Show)

------------------------------------------------------

-- Checks for prefix in string letter by letter
isPrefix :: String -> String -> Bool
isPrefix "" _
  = True
isPrefix _ ""
  = False
isPrefix (x : xs) (y : ys)
  | x == y    = isPrefix xs ys
  | otherwise = False
-- MARKING NOTE: better using higher order functions
-- e.g. isPrefixOf or take (length s) s' == s

-- Returns string with prefix removed
-- Pre: s is a prefix of s'
removePrefix :: String -> String -> String
removePrefix (p : ps) (s : ss)
  = removePrefix ps ss
removePrefix _ str
  = str
-- MARKING NOTE: again, don't use recursion, use HOF
-- e.g. drop (length s) s' == s

-- Returns a list of all suffixes of a given string
suffixes :: [a] -> [[a]]
suffixes str
  = take (length str) (iterate tail str)
-- or take (init str), which can written as take . init

-- Checks if str1 is a prefix of any of str2's suffixes
isSubstring :: String -> String -> Bool
isSubstring str1 str2
  = any (isPrefix str1) (suffixes str2)

-- Returns indices of substring in text (naive)
findSubstrings :: String -> String -> [Int]
findSubstrings str text
  = [index sfx | sfx <- suffixes text, isPrefix str sfx]
  where
      index sfx = length text - length sfx
-- MARKING NOTE: Again, don't overuse recursion!!!
-- findIndices (isPrefix s) (suffixes s')

------------------------------------------------------

-- Returns indices stored in leaves of suffix tree (DFS)
getIndices :: SuffixTree -> [Int]
-- Base case: if leaf, return [index]
getIndices (Leaf index)
    = [index]
-- Recursive: for each subtree in the list, traverse and concatenate
getIndices (Node namedSubtrees)
  = concatMap getIndices subtrees
  where
      subtrees = map snd namedSubtrees

-- Returns a common prefix and the remaining suffixes from two strings
-- If letters match, recursively append letter to prefix.
-- If letters do not match, return [] for prefix and remaining letters.
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition [] str2
  = ([], [], str2)
partition str1 []
  = ([], str1, [])
{- MARKING NOTE: this can be simplifed into:
partition xs ys
  = ([], xs, ys)
-}
partition str1@(x : xs) str2@(y : ys)
  | x == y    = (x : prefix, sfx1, sfx2)
  | otherwise = ([], str1, str2)
  where
      (prefix, sfx1, sfx2) = partition xs ys

-- Returns indices of substring in suffix tree (search)
findSubstrings' :: String -> SuffixTree -> [Int]
-- Check all the subtree labels of a tree and concatenate result lists.
-- If the residual s is empty at a leaf, return the index.
findSubstrings' [] (Leaf index)
    = [index]
-- If the residual s is non-empty at a leaf, return []
findSubstrings' _ (Leaf _)
    = []
-- MARKING NOTE: better use catchalls at end of function
-- UNNECESSARY HELPER FUNCTION
findSubstrings' s (Node subtrees)
    = concatMap processSubtree subtrees
    where
      processSubtree (label, tree)
        -- If s is prefix of label, return all leaf nodes. 
        | null res_s = getIndices tree
        -- If label is prefix of s, recursively search with residual s.
        | null res_label = findSubstrings' res_s tree
        -- If no common prefix, proceed to next subtree.
        | null prefix = []
        where
            (prefix, res_s, res_label) = partition s label
      processSubtree _
        = []
{-

findSubstrings' s (Node ((label, tree) : trees))
  | null res_s      = getIndices t'
  | null res_label  = findSubstrings' res_s tree
  | otherwise       = findSusbtrings' s (Node ps) 
  where
    (_, res_s, res_label) = partition s label

-}

------------------------------------------------------

-- E.g. insert ("banana", 0) (Node []) -> Node [("banana", Leaf 0)]
-- E.g. insert ("anana", 1) (Node [("banana", Leaf 0)]) 
--  -> Node [("banana", Leaf 0), ("anana", Leaf 1)]
-- E.g. insert ("a", 5) (Node [("banana", Leaf 0), ("anana", Leaf 1)])
--  -> Node [("banana", Leaf 0), ("a", Node [("", Leaf 5), ("nana", Leaf 1)])]

-- Insert a given suffix with its associated index into a tree
insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (suffix, index) tree
  -- If no subtree labels shares a prefix with the suffix, add a new leaf.
  | newTree == tree = Node (sfxtrees ++ [(suffix, Leaf index)])
  -- Otherwise, return the combined subtrees.
  | otherwise = newTree
  where
    (Node sfxtrees) = tree
    -- Inspect each subtree in the provided suffix tree. 
    newTree = Node (map insert' sfxtrees)
    -- For each subtree, return a suffix tree to be appended to the root.
    insert' :: (String, SuffixTree) -> (String, SuffixTree)
    insert' subtree@(label, subtree')
      | null prefix     = subtree
      | prefix == label = (label, insert (res_suffix, index) subtree')
      | otherwise       = (prefix, extendedTree)
      where
        -- For each subtree, partition its label into prefix and residuals.
        (prefix, res_suffix, res_label) = partition suffix label
        extendedTree = Node [(res_suffix, Leaf index), (res_label, subtree')]

-- MARKING NOTE: oh my god. why would you write code like this.
-- Too many helper functions! Separate

{-

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) (Node [])
  = Node [(s, Leaf n)]
insert (s, n) (Node ((a, t) : ats))
  | null pre  = merge (Node [(a, t)]) (insert (s, n) (Node ats))
  | pre == a  = Node ((a, insert (remS, n) t) : ats)
  | otherwise = merge (Node [(pre, Node [(remS, Leaf n), (remA, t)])]) (Node ats)
  where
    (pre, remS, remA) = partition s a

-- Pre: Arguments must be nodes
merge :: SuffixTree -> SuffixTree -> SuffixTree
merge (Node xs) (Node ys)
  = Node (xs ++ ys)

-}

  -- If the suffix and the label have no common prefix,
  -- the string and subtree are unchanged.

  -- If the label is a prefix of the suffix (e.g. "p" and ("pi", 9)),
  -- insert the residual suffix into the subtree 
  -- e.g. ("p", Node [("pi", Leaf 8)]) -> insert ("i", 9) Node [("pi", Leaf 8)]
    -- -> Node [("pi", Leaf 8), ("i", Leaf 9)]
    -- => ("p", Node [("pi", Leaf 8), ("i", Leaf 9)]).

  -- Otherwise (when the suffix is a prefix of the label),
  -- replace the label with the prefix and
  -- replace the subtree with a node leading to two subtrees.

    -- One subtree, labelled with the residual suffix, leads to a leaf.

    -- The other subtree is the original subtree, relabelled with the residual label.


-- This function is given
buildTree :: String -> SuffixTree
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1
  = "banana"

s2 :: String
s2
  = "mississippi"

t1 :: SuffixTree
t1
  = Node [("banana", Leaf 0),
          ("a", Node [("na", Node [("na", Leaf 1),
                                   ("", Leaf 3)]),
                     ("", Leaf 5)]),
          ("na", Node [("na", Leaf 2),
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2
  = Node [("mississippi", Leaf 0),
          ("i", Node [("ssi", Node [("ssippi", Leaf 1),
                                    ("ppi", Leaf 4)]),
                      ("ppi", Leaf 7),
                      ("", Leaf 10)]),
          ("s", Node [("si", Node [("ssippi", Leaf 2),
                                   ("ppi", Leaf 5)]),
                      ("i", Node [("ssippi", Leaf 3),
                                  ("ppi", Leaf 6)])]),
          ("p", Node [("pi", Leaf 8),
                      ("i", Leaf 9)])]

