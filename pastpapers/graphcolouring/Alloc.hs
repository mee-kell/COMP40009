module Alloc where

import Data.Maybe
import Data.List
import Data.Tuple

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count x
  = length . filter (== x)

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (nodes, edges)
  = [(n, count n (f ++ e)) | n <- nodes]
  where
    (f, e) = unzip edges

neighbours :: Eq a => a -> Graph a -> [a]
neighbours n (_, edges)
  = [f | (f, e) <- edges, e == n] ++ [e | (f, e) <- edges, f == n]
  -- Return a node if the node n exists in a edge pair with it

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode n (nodes, edges)
  = (filter (/= n) nodes, [p | p@(f, e) <- edges, f /= n && e /= n])
  -- Only return edges where the removed node is not in the pair

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([], _)
  = []
colourGraph numc g
  = (n, colour) : cMap
  where
    (_, n) = minimum (map swap (degrees g))
    cMap = colourGraph numc (removeNode n g)
    cMap' = [c | c@(x, _) <- cMap, x `elem` neighbours n g]
    colour = head (([1..numc] \\ map snd cMap') ++ [0])

------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap colouring
  = ("return", "return") : map buildIdMap' colouring
  where
    buildIdMap' :: (Id, Colour) -> (Id, Id)
    buildIdMap' (v, c)
      | c == 0    = (v, v)
      | otherwise = (v, 'R' : show c)

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments vars idMap
  = map (\v -> Assign (lookUp v idMap) (Var v)) vars

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Var e) idMap
  = Var (lookUp e idMap)
renameExp (Apply op e e') idMap
  = Apply op (renameExp e idMap) (renameExp e' idMap)
renameExp x _
  = x

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock [] _
  = []
renameBlock (While p b : ss) m
  = While (renameExp p m) (renameBlock b m) : renameBlock ss m
renameBlock (If p tB fB : ss) m
  = If (renameExp p m) (renameBlock tB m) (renameBlock fB m) : renameBlock ss m
-- Remove assignments of the form v = v
renameBlock (Assign id e : ss) m
  | Var id' == e' = reBlock
  | otherwise     = Assign id' e' : reBlock
  where
    reBlock = renameBlock ss m
    id'     = lookUp id m
    e'      = renameExp e m

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG live
  = (nodes, edges)
  where
    nodes = (nub . concat) live
    -- Generate all possible combinations of nodes
    pairs = removeSwaps [(n, n') | n <- nodes, n' <- nodes, n /= n']
    -- Find unique pairs where both elements exist in a set together
    edges = nub [p | p@(n, n') <- pairs, s <- live, n `elem` s && n' `elem` s]
    -- Helper function: remove duplicate combinations
    removeSwaps :: [(Id, Id)] -> [(Id, Id)]
    removeSwaps (p : ps)
      | swap p `elem` ps = removeSwaps ps
      | otherwise        = p : removeSwaps ps
    removeSwaps []
      = []


-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars 
  = undefined

buildCFG :: Function -> CFG
buildCFG 
  = undefined