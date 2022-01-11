{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Timings: 45 min for Part I and Part II. 95 min for Part III.
-- Used an extra hour to fix part III. Check diagrams!!!
-- 22/25. 92% :D
import Data.Maybe
import Data.List

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int

data Label = C Char | Eps
           deriving (Eq, Ord, Show)

type Transition = (State, State, Label)

type Automaton = (State, [State], [Transition])

--------------------------------------------------------
-- showRE - this may be useful for testing

showRE :: RE -> String
showRE (Seq re re')
  = showRE re ++ showRE re'
showRE (Alt re re')
  = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)
  = showRE' re ++ "*"
showRE (Plus re)
  = showRE' re ++ "+"
showRE (Opt re)
  =  showRE' re ++ "?"
showRE re
  = showRE' re

showRE' Null
  = ""
showRE' (Term c)
  = [c]
showRE' (Alt re re')
  = showRE (Alt re re')
showRE' re
  = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Part I. 2/3.

-- Look up a given item in a list of item/value pairs. 1/1
lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: There is exactly one occurrence of the item being looked up.
lookUp x
  = fromJust . lookup x

-- Remove any + or ? expressions using simplification rules. 2/3
simplify :: RE -> RE
simplify (Plus e)
  = Seq e (Rep e)
simplify (Opt e)
  = Alt e Null
simplify (Seq e e')
  = Seq (simplify e) (simplify e')
simplify (Alt e e')
  = Alt (simplify e) (simplify e')
simplify re
  = re
{-
You forgot simplify (Rep e)!
-}

--------------------------------------------------------
-- Part II. 11/11

-- Return the start state, terminal states and transitions in an NDA. 1/1
startState :: Automaton -> State
startState (s, _, _)
  = s
terminalStates :: Automaton -> [State]
terminalStates (_, ss, _)
  = ss
transitions :: Automaton -> [Transition]
transitions (_, _, ts)
  = ts

-- Checks if the given state is a terminal state in the given automaton. 1/1
isTerminal :: State -> Automaton -> Bool
isTerminal s nda
  = s `elem` terminalStates nda

-- Returns the list of transitions emanating from a given state. 2/2
transitionsFrom :: State -> Automaton -> [Transition]
transitionsFrom s nda
  = [t | t@(s', _, _) <- transitions nda, s == s']
  {- Marking note: you can list alternatives in comments, e.g. filter -}

-- Returns the labels in the list of transitions, with no duplicates or Eps. 1/1
labels :: [Transition] -> [Label]
labels ts
  = nub [l | (_, _, l) <- ts, l /= Eps]

-- Checks if the given automaton accepts the given string using backtracking. 6/6
accepts :: Automaton -> String -> Bool
accepts nda
  = accepts' (startState nda)
  where
    accepts' :: State -> String -> Bool
    accepts' s str
      | isTerminal s nda && null str = True
      | otherwise                    = any (try str) (transitionsFrom s nda)
    try :: String -> Transition -> Bool
    try cs (_, t, Eps)
      | null cs   = False
      | otherwise = accepts' t cs
    try (c' : cs') (_, t, C c)
      | c' == c   = accepts' t cs'
      | otherwise = False

--------------------------------------------------------
-- Part III. 8/8

makeNDA :: RE -> Automaton
makeNDA re
  = (1, [2], sort transitions)
  where
    (transitions, k) = make (simplify re) 1 2 3

-- Generate list of transitions. /8
make :: RE -> Int -> Int -> Int -> ([Transition], Int)
-- Base cases: null and terminal. good!
make Null m n k
  = ([(m, n, Eps)], k)
make (Term c) m n k
  = ([(m, n, C c)], k)
-- Recursively make NDAs
make (Seq r1 r2) m n k -- good!
  = (concat [r1T, r2T, epsT], i')
  where
    epsT      = addEpsT k [(k, k + 1)]
    (r1T, i)  = make r1 m k (k + 2)
    (r2T, i') = make r2 (k + 1) n i
make (Alt r1 r2) m n k -- good
  = (concat [r1T, r2T, epsT], i')
  where
    epsT      = addEpsT k [(m, k), (m, k + 2), (k + 1, n), (k + 3, n)]
    (r1T, i)  = make r1 k (k + 1) (k + 4)
    (r2T, i') = make r2 (k + 2) (k + 3) i
make (Rep r) m n k -- good
  = (rT ++ epsT, i)
  where
    epsT    = addEpsT k [(m, k), (m, n), (k + 1, k), (k + 1, n)]
    (rT, i) = make r k (k + 1) (k + 2)

-- Helper function to add Null transitions between states in a list. nice!
addEpsT :: Int -> [(Int, Int)] -> [Transition]
addEpsT k = concatMap (\(m, n) -> fst (make Null m n k))

--------------------------------------------------------
-- Part IV

type MetaState = [State]

type MetaTransition = (MetaState, MetaState, Label)

getFrontier :: State -> Automaton -> [Transition]
getFrontier
  = undefined

groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions
  = undefined

makeDA :: Automaton -> Automaton
-- Pre: Any cycle in the NDA must include at least one non-Eps transition
makeDA
  = undefined

--------------------------------------------------------
-- Test cases

reFigure, re1, re2, re3, re4, re5 :: RE
reFigure
  = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1
  = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2
  = Seq (Term 'x') (Rep (Term '\''))
re3
  = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4
  = Seq (Alt (Term 'a') Null) (Term 'a')
re5
  = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

nd, nd' :: Automaton
nd = (1,[4],[(1,2,C 'a'),(1,3,C 'b'),(2,3,Eps),(2,4,C 'c')])

nd' = (1,[4],[(1,2,Eps),(1,3,C 'a'),(2,4,C 'a'),(2,4,C 'b'),
              (3,4,C 'b'),(3,4,Eps)])

da :: Automaton
da = (0,[3],[(0,1,C 'a'),(0,2,C 'a'),(0,2,C 'b'),(1,2,C 'a'),
             (1,3,C 'b'),(2,2,C 'a'),(2,1,C 'a'),(2,3,C 'b')])

re :: RE
re = Seq (Alt (Term 'a') (Term 'b')) (Seq (Rep (Term 'a')) (Term 'b'))

ndaFigure, nda1, nda2, nda3, nda4, nda5 :: Automaton
daFigure, da1, da2, da3, da4, da5 :: Automaton
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],
     [(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1
  = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2
  = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2
  = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3
  = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3
  = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4
  = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5
  = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])
