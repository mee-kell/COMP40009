import Data.List
import Data.Char
-- 2. FUNCTIONS

-- 15 minutes
chop :: Int -> (Int, Int)
chop n
    | n < 10 = (0, n)
    | otherwise = (q + 1, r)
    where
        (q, r) = chop (n - 10)

-- 20 minutes, with help
concatenate :: Int -> Int -> Int
concatenate x y
    | y < 10    = 10 * x + y
    | otherwise = 10 * concatenate x a + b
    where
        (a, b) = chop y

-- 20 minutes total
fib :: Int -> Int
fib 0
    = 0
fib 1
    = 1
fib n
    -- = fib (n - 1) + fib (n - 2)
    = fib' 0 1 n

fib' :: Int -> Int -> Int -> Int
fib' m n k
    | k == 0    = m
    | otherwise = fib' n (m + n) (k - 1)

goldenRatio :: Float -> Float
goldenRatio e
    = goldenRatio' 1 0
    where
        goldenRatio' :: Int -> Float -> Float
        goldenRatio' n rm
            | abs(rn - rm) < e = rn
            | otherwise        = goldenRatio' (n + 1) rn
            where
                rn = fromIntegral (fib (n + 1)) / fromIntegral (fib n)

-- 3.2. List comprehensions
-- 15 minutes
quicksort :: Ord a => [a] ->  [a]
quicksort []
    = []
quicksort (anchor : remaining)
    = quicksort less ++ (anchor : quicksort more)
    where
        less = [x | x <- remaining, x <= anchor]
        more = [x | x <- remaining, x > anchor]

-- 30 minutes! Still needed help
-- Had the right idea, but needed to implement
-- using one list comprehension instead of many
perms :: [Int] -> [[Int]]
perms []
    = [[]]
perms xs
    = [ x : ps| x <- xs, ps <- perms (xs \\ [x])]

-- 30 minutes!
-- Did the extension in 5 tho :)
routes :: Int -> Int -> [(Int, Int)] -> [[Int]]
routes n1 n2 vs
    | n1 == n2 = [[n2]]
    | otherwise = [ p | p <- paths, last p == n2 ]
    where
        edges = [ v | v@(x, y) <- vs, x == n1 ]
        cutGraph = vs \\ edges
        paths = [ a : ns | (a, b) <- edges, ns <- routes b n2 cutGraph ]

-- 4. HIGHER ORDER FUNCTIONS
-- 10 minutes
depunctuate :: String -> String
depunctuate
    = filter (`notElem` ".,:")

makeString :: [Int] -> String
makeString
    = map chr

-- struggling! got it! 15 min
enpower :: [Int] -> Int
enpower lst
    = foldl1 (^) (reverse lst)

-- 5 minutes
revAll :: [[a]] -> [a]
revAll
    = concatMap reverse

-- 15 minutes
-- tip: draw out with an example
-- notice if its going thru the list element by element
rev :: [a] -> [a]
rev
    = foldl (flip (:)) []

-- 10 minutes with help
-- Remember that foldr f x y operates as f y x
-- whereas foldr (f) x y operates as y (f) x
dezip :: [(a,b)] -> ([a],[b])
dezip
    = foldr dezip' ([],[])
    where
        dezip' (a, b) (as, bs)
            = (a : as, b : bs)

-- 5 minutes! :)))
allSame :: [Int] -> Bool
allSame xs
    = and (zipWith (==) (tail xs) xs)

-- 15 minutes for both squash
squash :: (a -> a -> b) -> [a] -> [b]
squash f []
    = []
squash f [x]
    = []
squash f (x1 : x2 : xs)
    = f x1 x2 : squash f xs

-- Tip: consider combining zipWith and tail
squash1 :: (a -> a -> b) -> [a] -> [b]
squash1 f xs
    = zipWith f xs (tail xs)

-- 5.7. USER DEFINED TYPES (TREES)
data Tree a b = Empty | Node (Tree a b) a (Tree a b) | Leaf b

mapT :: (a -> a) -> (b -> b) -> Tree a b -> Tree a b
mapT f1 f2 (Leaf x)
    = Leaf (f2 x)
mapT f1 f2 Empty
    = Empty
mapT f1 f2 (Node t1 x t2)
    = Node (mapT f1 f2 t1) (f1 x) (mapT f1 f2 t2)
