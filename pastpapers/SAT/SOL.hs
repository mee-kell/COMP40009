-- Timing: finished parts I, II and III under 3 hours.
-- Spend an extra 30 minutes puzzling over part IV (optional).
-- 24/25 - 1 = 23/25. You should add comments!

module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I. 5/5

-- 1 mark. 1/1
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp x
  = fromJust . lookup x

-- 3 marks. 3/3
vars :: Formula -> [Id]
vars
  = nub . sort . vars'
  where
    vars' :: Formula -> [Id]
    vars' (Var x)
      = [x]
    vars' (Not x)
      = vars' x
    vars' (And x y)
      = vars' x ++ vars' y
    vars' (Or x y)
      = vars' x ++ vars' y

-- 1 mark. 1/1
idMap :: Formula -> IdMap
idMap formula
  = zip (vars formula) [1..]

--------------------------------------------------------------------------
-- Part II. 11/11

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks. 4/4
toNNF :: Formula -> NNF
toNNF (Not (Or a b)) -- yes
  = And (toNNF (Not a)) (toNNF (Not b))
toNNF (Not (And a b)) -- yes
  = Or (toNNF (Not a)) (toNNF (Not b))
toNNF (Not (Not a)) -- yes
  = toNNF a
toNNF (And a b) -- yes
  = And (toNNF a) (toNNF b)
toNNF (Or a b) -- yes
  = Or (toNNF a) (toNNF b)
toNNF (Not a) -- yes
  = Not (toNNF a)
toNNF (Var x) -- yes
  = Var x

-- 3 marks. 3/3
toCNF :: Formula -> CNF
toCNF formula
  = toCNF' (toNNF formula)
  where
    toCNF' :: Formula -> CNF
    toCNF' (Or a b) -- yes
      = distribute (toCNF' a) (toCNF' b)
    toCNF' (And a b) -- yes
      = And (toCNF' a) (toCNF' b)
    toCNF' x -- yes
      = x

-- 4 marks. 4/4
flatten :: CNF -> CNFRep
flatten cnf
  = flatten' cnf
  where
    varInt = idMap cnf -- yes
    flatten' :: CNF -> CNFRep
    flatten' (Var x)
      = [[lookUp x varInt]] -- yes
    flatten' (Not (Var x))
      = [[- (lookUp x varInt)]] -- yes
    flatten' (Or a b) -- yes
      = [x ++ y]
      where
        [x] = flatten' a
        [y] = flatten' b
    flatten' (And a b) -- yes
      = flatten' a ++ flatten' b
    flatten' _
      = error "Case not expected."

--------------------------------------------------------------------------
-- Part III. 8/9.

-- 5 marks. 4/5
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits rep
  = propUnits' rep []
-- Nice to do it in a helper function, but can you make it work in one?
-- e.g. ... = (rep, []) ... (rep', u : sols), where (rep', u) = propUnits clauses
propUnits' :: CNFRep -> [Int] -> (CNFRep, [Int])
propUnits' rep prop
  | null us   = (rep, prop)
  | otherwise = propUnits' clauses (u : prop)
  where
    us = [x | [x] <- rep] -- good!
    u = head us
    clauses = map (filter (/= (-u))) (filter (notElem u) rep) -- yes

-- 4 marks. 4/4

dp :: CNFRep -> [[Int]]
dp rep
  = dp' rep []
-- Same as above: you use a stack in this recursive function
-- but can you make it without a helper function / accum. parameter?
dp' :: CNFRep -> [Int] -> [[Int]]
dp' rep units
  | null rep'      = [trace]
  | [] `elem` rep' = [] -- yes
  | otherwise      = dp' ([var] : rep') trace ++ dp' ([-var] : rep') trace
  where
    (rep', units') = propUnits rep
    trace = units ++ units'
    var = (head . head) rep'
  
  -- map (sols ++) (dp' ([var] : rep') ++ dp' ([-var] : rep'))

--------------------------------------------------------------------------
-- Part IV. 0/2. Made progress, but got confused - split into helper functions!

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat formula
  = map convert truthVals
  where
    truthVals = (dp . flatten . toCNF) formula
    mapp = map (\(i, n) -> (n, i)) (idMap formula)
    convert :: [Int] -> [(Id, Bool)]
    convert ns
     = sort (zip tLetts (repeat True) ++ zip fLetts (repeat False))
     where
       tLetts = map (`lookUp` mapp) (filter (>= 0) ns)
       fLetts = map ((`lookUp` mapp) . abs) (filter (< 0) ns)
       -- Didn't get around to implementing missing
       missing = filter (`notElem` (tLetts ++ fLetts)) (vars formula)



