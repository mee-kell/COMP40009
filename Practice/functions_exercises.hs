-- UNASSESSED EXERCISES: HASKELL FUNCTIONS

-- Add a single-digit integer onto the right-hand end of an arbitrary-size integer
addDigit :: Int -> Int -> Int
-- Pre: digit must be a single digit
addDigit number digit
    = number * 10 + digit

-- Converts a temperature given in degrees Celcius to Fahrenheit
convert :: Float -> Float 
convert celsius
    = celsius * 9 / 5 + 32

-- Returns the factorial of a given non-negative integer
fact :: Int -> Int 
fact n
    | n == 0    = 1
    | otherwise = n * fact (n - 1)

-- Evaluates the permutations nPr
perm :: Int -> Int -> Int 
perm n r
    | r == 1    = n
    | otherwise = n * perm (n - 1) (r - 1)

-- Computes the remainder after integer division
remainder :: Int -> Int -> Int
remainder n div
    | n < div   = n 
    | otherwise = remainder (n - div) div
 
-- Calculate the quotient and remainder after division by 10
chop :: Int -> (Int, Int)
chop n
    = divCount (n, 0)
    where
        divCount :: (Int, Int) -> (Int, Int)
        divCount (n, q)
            | n < 10    = (q, n)
            | otherwise = divCount (n - 10, q + 1)

-- QUESTION 8: Calculate the combination nCr
choose :: Int -> Int -> Int
-- Pre: n >= r, n >= 0, r >= 0
choose n r
    | n == r    = 1
    | r == 0    = 1
    | otherwise = choose (n - 1) r + choose (n - 1) (n - r)

-- QUESTION 11: Compute the binary representation of a given integer
binary :: Int -> Int
binary n
    | n < 2 = n
    | otherwise = binary q * 10 + r
    where
        (q, r) = quotRem n 2

baseNum :: Int -> Int -> Int
baseNum n b
    | n < b = n
    | otherwise = baseNum q b * 10 + r
    where
        (q, r) = quotRem n b