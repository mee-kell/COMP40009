import Data.List
import Data.Char
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

-- 3 minutes
mapT :: (a -> a) -> (b -> b) -> Tree a b -> Tree a b
mapT f1 f2 (Leaf x)
    = Leaf (f2 x)
mapT f1 f2 Empty
    = Empty
mapT f1 f2 (Node t1 x t2)
    = Node (mapT f1 f2 t1) (f1 x) (mapT f1 f2 t2)

-- The type definition was tricky!
-- 15 minutes, with a LOT of help
foldT :: (b -> c) -> (c -> a -> c -> c) -> c -> Tree a b -> c
foldT leafF nodeF base Empty
    = base
foldT leafF nodeF base (Leaf b)
    = leafF b
foldT leafF nodeF base (Node t1 x t2)
    = nodeF (foldInto t1) x (foldInto t2)
    where
        foldInto = foldT leafF nodeF base

-- 5 minutes
-- Again, with help. Key understanding: foldT replaces leaf values 
-- and performs a function on node values
countLeaves :: Tree a b -> Int
countLeaves
    = foldT (const 1) count 0
    where
        count t1 x t2 = t1 + t2

-- 4 minutes
sumValues :: Tree Int Int -> Int
sumValues
    = foldT id addLeaves 0
    where
        addLeaves t1 x t2 = x + t1 + t2

-- 15 minutes (with help)
-- Problem was that you tried to apply functions twice
-- and forgot that foldT applies functions to nodes automatically
flattenl :: Tree a a -> [a]
flattenl
    = foldT (: []) squishNode []
    where
        squishNode t1 x t2 = t1 ++ [x] ++ t2

-- ahahah 1 minute, same as above
flattenr :: Tree a a -> [a]
flattenr
    = foldT (: []) squishNode []
    where
        squishNode t1 x t2 = t2 ++ [x] ++ t1

-- 5 minutes :D
ast :: Tree (Int -> Int -> Int) Int -> Int
ast
    = foldT id binFun 0
    where
        binFun :: Int -> (Int -> Int -> Int) -> Int -> Int
        binFun t1 x t2 = x t1 t2

-- 6. Type classes

-- Note: force datatype using ::, 
-- e.g. minBound :: Colour
-- e.g. toEnum 0 :: Colour
data Colour = Red | Green | Blue
            deriving (Show, Bounded, Enum)

data AmPm = AM | PM
            deriving (Show, Eq)

data Time h m = Military h m | Wall h m AmPm

-- 25 minutes
instance (Eq h, Eq m, Num h, Num m) => Eq (Time h m) where
    (==) = equalTime

-- 10 minutes
to24 :: (Eq h, Num h) => Time h m -> Time h m
to24 (Military h m)
    = Military h m
to24 (Wall h m amPm)
    | amPm == AM && h == 12 = Military 00 m
    | amPm == PM && h /= 12 = Military (h + 12) m
    | otherwise             = Military h m

equalTime :: (Eq h, Eq m, Num h) => Time h m -> Time h m -> Bool
equalTime t1 t2
    = h1 == h2 && m1 == m2
    where
        (Military h1 m1) = to24 t1
        (Military h2 m2) = to24 t2

-- 5 minutes
instance (Show h, Show m, Eq h, Eq m, Num h, Num m) => Show (Time h m) where
    show (Military h m) = show h ++ show m ++ "hrs"
    show (Wall 12 00 AM) = "Midnight"
    show (Wall 12 00 PM) = "Midday"
    show (Wall h m amPm) = show h ++ show m ++ map toLower (show amPm)

-- 6.3. Difficult!
-- ASK FOR HELP

type VarName = String

data Fun = Add | Sub | Mul
        deriving (Eq, Show)

data Exp = Val Int | Id VarName | App Fun Exp Exp
        deriving (Eq, Show)
    
type Assignment = (VarName, Exp)

type Program = [Statement]

data Statement = A Assignment | Loop Int Program

type Environment a = [(String, a)]

-- Classes and instances
class Vars a where
    x, y, z :: a

instance Vars Exp where
    x = x
    y = y
    z = z

instance Num Exp where
    (+) = App Add
    (*) = App Mul
    (-) = App Sub
    abs = undefined
    signum = undefined 
    fromInteger = undefined 

-- Assignment operator: VarName <-- Exp = A (VarName, Exp)
infixl 1 <--
(<--) :: VarName -> Exp -> Statement
(<--)
    = (A .) . (,)

loop :: Int -> Program -> Statement
loop = Loop

evalS :: Statement -> Environment Int -> Environment Int
evalS s
    = undefined

------
instance Vars Exp where
x = Id "x"
y = Id "y"
z = Id "z"
-- This requires the FlexibleInstances language extension...
instance Vars String where
x = "x"
y = "y"
z = "z"
lift :: Fun -> Exp -> Exp -> Exp
lift f x y = App f x y
instance Num Exp where
fromInteger = Val . fromInteger
(+) = lift Add
(-) = lift Sub
(*) = lift Mul
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp
= (fromJust .) . lookup
update :: VarName -> Int -> Environment Int -> Environment Int
update v x env
= (v, x) : [p | p@(v’, x’) <- env, v /= v’]
eval :: Exp -> Environment Int -> Int
eval (Val n) env
= n
eval (Id id) env
= lookUp id env
eval (App f e e’) env
= apply f (eval e env) (eval e’ env)
apply op v v’
= lookUp op [(Add, (+)),(Sub, (-)), (Mul, (*))] v v’
evalS :: Statement -> Environment Int -> Environment Int
evalS (A (v, e)) env
= update v (eval e env) env
evalS (Loop n p) env
= foldr run’ env (replicate n p)
run :: Program -> Environment Int
run p
= run’ p []
where
run’ p env
= foldr evalS env (reverse p)