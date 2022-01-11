{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Debug.Trace
import Graphics.Rendering.OpenGL.GL.DisplayLists (listBase)
import Data.Char
import Data.List

precedes :: String -> String -> Bool
precedes [] _
    = True
precedes _ []
    = False
precedes (s1: s1s) (s2 : s2s)
    = (s1 < s2 || s1 == s2) && precedes s1s s2s

pos :: Eq a => a -> [a] -> Int
pos num []
    = 0
pos num (n : ns)
    | num == n  = 0
    | otherwise = 1 + pos num ns

-- Complexity = O(n^2)
rev :: [a] -> [a]
rev []
    = []
rev (x : xs)
    = rev xs ++ [x]

-- Complexity O(n)
rev1 :: [a] -> [a]
rev1 list
    = rev1' list []
    where
        rev1' [] acc
            = acc
        rev1' (x : xs) acc
            = rev1' xs (x : acc)

-- 7. Anagrams
-- For each character in s, get its position
-- Find its corresponding character in angrm1
-- Locate the position of that character in angrm2
-- Get the character at that position in s
-- Add that character to a list
transpose :: String -> String -> String -> String
transpose s angrm1 angrm2
    = transpose' (rev1 s) []
    where
        transpose' [] angrms
            = angrms
        transpose' (x : xs) angrms
            = transpose' xs (mapChr : angrms)
            where
                chrt = angrm2 !! pos x s
                mapPos = pos chrt angrm1
                mapChr = s !! mapPos

-- 4.8.
any' :: (a -> Bool) -> [a] -> Bool
any' p
    = any p

all' :: (a -> Bool) -> [a] -> Bool
all' p
    = all p

-- 4.9
isElem' :: Eq a => a -> [a] -> Bool
isElem' = (.) any (==)

-- 4.10
(<.>) :: (a -> a) -> (a -> a -> a) -> (a -> a -> a)
-- (<.>) f1 f2 n1 n2 = (f1 . (f2 n1)) n2
-- (<.>) f1 f2 n1 = f1 . f2 n1
(<.>) = (.) . (.)

-- 4.11.
pipeline :: [a -> a] -> [a] -> [a]
pipeline = map . foldr (.) id

-- 5.1.
data Shape =
    Triangle Float Float Float |
    Square Float |
    Circle Float |
    Polygon [(Float, Float)]

area :: Shape -> Float
area (Triangle a b c)
    = sqrt (s * (s - a) * (s - b) * (s - c))
    where
        s = (a + b + c) / 2
area (Square x)
    = x ^ 2
area (Circle r)
    = pi * (r ^ 2)
-- 4.2.
area (Polygon (v1 : v2 : v3 : vs))
    = triArea + area (Polygon vs)
    where
        l1 = lineLength v1 v2
        l2 = lineLength v2 v3
        l3 = lineLength v1 v3
        triArea = area (Triangle l1 l2 l3)
        lineLength (x, y) (x', y')
            = sqrt ((x - x') ^ 2 + (y - y') ^ 2)
area (Polygon _)
    = 0

-- 5.4. CHECK THIS OUT
data Tree a = Empty | Node (Tree a) a (Tree a)
            deriving (Show, Eq)

{--
flatten :: Tree a -> [a]
flatten Empty
  = []
flatten (Node t1 x t2)
  = flatten t1 ++ (x : flatten t2)
--}
{--}
flatten :: Tree a -> [a]
flatten Empty
    = []
flatten tree
    = flatten' tree []
    where
        flatten' :: Tree a -> [a] -> [a]
        flatten' Empty acc
            = acc
        flatten' (Node t1 x t2) acc
            = x : flatten' t1 (flatten' t2 acc)
--}

-- 5.6.
data BinTree a = Leaf a | Branch (BinTree a) (BinTree a)
                deriving (Eq, Show, Ord)

build :: [a] -> BinTree a
-- Pre: input list is non-empty
build [x]
    = Leaf x
build list@(x : xs)
    | null xs   = Leaf x
    | otherwise = Branch (build x1s) (build x2s)
    where
        snip = length list `div` 2
        (x1s, x2s) = splitAt snip list
build []
    = error "List cannot be empty"

ends :: BinTree a -> [a]
ends (Leaf a)
    = [a]
ends (Branch x y)
    = ends x ++ ends y

swaps :: BinTree a -> BinTree a
swaps (Leaf a)
    = Leaf a
swaps (Branch x y)
    = Branch (swaps y) (swaps x)

-- 6.1.
data Colour = Red | Green | Blue
            deriving (Show)

instance Bounded Colour where
    minBound = Red
    maxBound = Blue

instance Enum Colour where
    succ Red = Green
    succ Green = Blue
    succ Blue = Red
    fromEnum Red = 0
    fromEnum Green = 1
    fromEnum Blue = 2
    toEnum 0 = Red
    toEnum 1 = Green
    toEnum 2 = Blue
    enumFrom Red = [Red, Green, Blue]

-- 3.8.
removeWhitespace :: String -> String
-- Pre: first character is non-whitespace
removeWhitespace ""
    = ""
removeWhitespace string@(str : strs)
    | isSpace str = removeWhitespace strs
    | otherwise   = string

-- 3.9.
nextWord :: String -> (String, String)
nextWord ""
    = ("", "")
nextWord (c : cs)
    | isSpace c = ("", cs)
    | otherwise = (c : w, s)
    where
        (w, s) = nextWord cs

-- 3.9.
splitUp :: String -> [String]
splitUp ""
    = []
splitUp s
    = w : splitUp ws
    where
        (w, ws) = (nextWord . removeWhitespace) s

-- 3.11.
primeFactors :: Int -> [Int]
primeFactors = factors 2
    where
        factors p 1
            = []
        factors p m
            | r == 0    = p : factors p q
            | otherwise = factors (p + 1) m
            where
                (q, r) = quotRem m p

-- 3.12.
hcf :: Int -> Int -> Int
hcf a b
    = product common
    where
        common = ps \\ (ps \\ ps')
        ps = primeFactors (max a b)
        ps' = primeFactors (min a b)

-- 3.2.5.
prefixes :: [t] -> [[t]]
prefixes []
    = []
prefixes (c : cs)
    = [c] : [c : ps | ps <- prefixes cs]

-- 3.2.6.
substrings :: String -> [String]
substrings lst
    = concatMap prefixes [lst]