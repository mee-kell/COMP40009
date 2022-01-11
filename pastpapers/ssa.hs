{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- TIMING: couldn't finish in 3 hours again... took extra time for part III
-- Make sure you skip to different parts if you're stuck.
-- 24/30 = 80%
import Data.Maybe
import Data.List

type Id = String

-- Function = (name, [arguments], body)
type Function = (Id, [Id], Block)

type Block = [Statement]

data Statement = Assign Id Exp |        -- Assign variable expression
                 If Exp Block Block |   -- If predicate (true body) (false body)
                 DoWhile Block Exp      -- DoWhile body (termination predicate)
               deriving (Eq, Show)

data Exp = Const Int | Var Id | Apply Op Exp Exp | Phi Exp Exp
         deriving (Eq, Show)

data Op = Add | Mul | Eq | Gtr
        deriving (Eq, Show)

------------------------------------------------------------------------
-- Given functions to support the interpreter...

lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp i table
  = fromMaybe (error ("lookup failed on identifier: " ++ show i))
              (lookup i table)

execFun :: Function -> [Int] -> State
execFun (name, args, p) vs
  = execBlock p (zip args vs)

------------------------------------------------------------------------
-- Part I. 12/14

type State = [(Id, Int)] -- Associates variable identifiers to integer bindings

-- Update a given state with a new binding for a variable.  1/2
-- If the variable is not bound in the state its binding should be added
update :: (Id, Int) -> State -> State
update newBinding@(id, _) state
  | id `elem` map fst state = newBinding : filter (\(id', _) -> id /= id') state
  | otherwise               = newBinding : state
-- The guard condition isn't needed.
-- = newBinding : filter (\(id', _) -> id /= id) state    is sufficient.

-- Applies a given operator to its two arguments. 1/2
apply :: Op -> Int -> Int -> Int
apply Add x y
  = x + y
apply Mul x y
  = x * y
apply Eq x y
  | x == y    = 1
  | otherwise = 0
apply Gtr x y
  | x > y     = 1
  | otherwise = 0
-- Better to create a mapping: ops = [(Add, (+)), (Mul, (*)), (Eq, eq), (Gtr, gtr)]
-- where eq = (fromEnum .) . (==), gtr = (fromEnum .) . (>)
-- then do a lookUp op ops.

-- Evaluate a given expression with respect to a given state. 3/3
eval :: Exp -> State -> Int
-- Pre: the variables in the expression will all be bound in the given state 
-- Pre: expressions do not contain phi instructions
eval (Const x) _
  = x
eval (Var id) state
  = lookUp id state
eval (Apply op e1 e2) state
  = apply op (eval e1 state) (eval e2 state)
-- eval (Phi x y) env = error "Case not expected."

-- Returns an updated state after statement is executed with given state. 7/7
execStatement :: Statement -> State -> State
execStatement (Assign var exp) state -- good
  = update (var, eval exp state) state
execStatement (If cond trueB falseB) state -- good
  | eval cond state == 1 = execBlock trueB state
  | otherwise            = execBlock falseB state
execStatement loop@(DoWhile block cond) state -- good!
  | eval cond state' == 0 = state'
  | otherwise             = execStatement loop state'
  where
    state' = execBlock block state

-- Returns an updated state after block is executed with given state
execBlock :: Block -> State -> State
execBlock sts state -- good!
  = foldl (flip execStatement) state sts

------------------------------------------------------------------------
-- Given function for testing propagateConstants...

-- Converts a function in SSA form into its optimised SSA form...
applyPropagate :: Function -> Function
applyPropagate (name, args, body)
  = (name, args, propagateConstants body)

------------------------------------------------------------------------
-- PART II. 11/13

-- Simplify expressions in SSA form. 3/3
foldConst :: Exp -> Exp
-- Pre: the expression is in SSA form
foldConst (Phi (Const x) (Const y))
  | x == y = Const x
foldConst (Apply op (Const x) (Const y))
  = Const (apply op x y)
foldConst (Apply op (Var v) (Const 0))
  = Var v -- might use v@(Var y) instead?
foldConst (Apply op (Const 0) (Var v))
  = Var v
foldConst exp
  = exp

-- Substitute an identifier with an integer in a expression and fold it. 3/3
sub :: Id -> Int -> Exp -> Exp
-- Pre: the expression is in SSA form
sub id val (Var x)
  | x == id = Const val
sub id val (Apply op e1 e2)
  = foldConst (Apply op (sub id val e1) (sub id val e2))
sub id val (Phi e1 e2)
  = foldConst (Phi (sub id val e1) (sub id val e2))
sub _ _ exp
  = exp

-- Use (by uncommenting) any of the following, as you see fit...
type Worklist = [(Id, Int)]
-- scan :: (Exp -> Exp) -> Block -> (Exp -> Exp, Block)

{-
scan :: Id -> Int -> Block -> (Worklist, Block)
scan id val []
  = ([], [])
scan id val (Assign x exp : sts)
  | isConst repExp = ((x, eval repExp []) : list, block')
  | otherwise      = (list, Assign x repExp : block')
  where
    repExp         = sub id val exp
    (list, block') = scan id val sts
scan id val (If exp trueB falseB : sts)
  = (list, If (sub id val exp) trueB falseB : block')
  where
    (list, block') = scan id val sts
    (list', trueB') = scan id val trueB
    (list'', falseB') = scan id val falseB'
scan id val (DoWhile b exp : sts)
  = (list ++ list', DoWhile b' (sub id val exp) : block')
  where
    (list, block') = scan id val sts
    (list', b') = scan id val b

isConst :: Exp -> Bool
isConst (Const _) 
  = True
isConst _ 
  = False

-- Apply constant propagation and constant folding to a given code block
propagateConstants :: Block -> Block
-- Pre: the block is in SSA form
propagateConstants b
  = snd (iterateList (scan "$INVALID" 0 b))
  where
    iterateList :: (Worklist, Block) -> (Worklist, Block)
    iterateList ([], block)
      = ([], block)
    iterateList ((id, val) : list, block)
      = iterateList (newList, newBlock) -- might be (newList, newBlock)?
      where
        (newList, newBlock) = scan id val block
-}

{-
Propagate constants, transforming a block.
	Build an initial work list using a dummy scan.
	Remove the first (v, c) item from the work list and scan the block.
	Append the returned work list to the current work list.
	If the work list is empty, return the current (modified) block.
-}

propagateConstants :: Block -> Block -- good. 5/7
-- Pre: the block is in SSA form. 
propagateConstants block
  = snd (iterateScan (scan "$INVALID" 0 block))

iterateScan :: (Worklist, Block) -> (Worklist, Block) -- good!
iterateScan ([], block)
  = ([], block)
iterateScan ((v, c) : worklist, block)
  = iterateScan (worklist ++ newAssignments, modBlock)
  where
    (newAssignments, modBlock) = scan v c block

{-
Scan the block.
	Locate every expression in the block. Do this recursively.
	If the expression is a constant assignment, add it to a work list.
		Remove the constant assignment expression from the block.
		Do not remove the expression if it is an assignment to $return.
	If the expression is not a constant assignment,
		Replace v with c if v is in the expression, and fold using `sub`.
	Return the modified block and the new work list.
-}

scan :: Id -> Int -> Block -> (Worklist, Block)
-- You're better off defining another helper function that takes a (Worklist, Block)
-- then applying foldr instead of recursing on the block!
-- Recursion on the block is the problem, everything else is fine.
scan var val []
  = ([], [])
scan var val (Assign id (Const x) : block)
  | id == "$return" = ((id, x) : worklist, Assign id (Const x) : modBlock)
  | otherwise       = ((id, x) : worklist, modBlock)
  where
    (worklist, modBlock) = scan var val block
scan var val (Assign id exp : block) -- good (?)
  | isConst repExp = ((id, eval repExp []) : worklist, modBlock)
  | otherwise      = (worklist, Assign id repExp : modBlock)
  where
    (worklist, modBlock) = scan var val block
    repExp = sub var val exp
    isConst (Const _) = True
    isConst _ = False
scan var val (If exp block1 block2 : block) -- good
  = (worklist', If (sub var val exp) modBlock1 modBlock2 : modBlock)
  where
    (worklist1, modBlock1) = scan var val block1
    (worklist2, modBlock2) = scan var val block2
    (worklist, modBlock) = scan var val block
    worklist' = worklist ++ worklist1 ++ worklist2
scan var val (DoWhile body exp : block) -- good
  = (worklist', DoWhile modBody (sub var val exp) : modBlock)
  where
    (worklist, modBlock) = scan var val block
    (worklist1, modBody) = scan var val body
    worklist' = worklist ++ worklist1

------------------------------------------------------------------------
-- Given functions for testing unPhi...

-- Applies unPhi to a given function...
applyUnPhi :: Function -> Function
applyUnPhi (name, args, body)
  = (name, args, unPhi body)

-- Combines propagation/folding and unPhi to convert a function from its
-- unoptimised SSA form to a final non-SSA form...
optimise :: Function -> Function
optimise (name, args, body)
  = (name, args, unPhi (propagateConstants body))

------------------------------------------------------------------------
-- PART III. 1/3

-- remove the phi assignments from any function in SSA form. 1/3
unPhi :: Block -> Block
-- Pre: the block is in SSA form
unPhi (If exp trueB falseB : Assign id (Phi e1 e2) : sts)
  = If exp trueB' falseB' : unPhi sts
  where
    trueB' = trueB ++ [Assign id e1]
    falseB' = falseB ++ [Assign id e2]
unPhi (DoWhile (Assign id (Phi e1 e2) : block) exp : sts)
  = DoWhile block' exp : sts
  where
    block' = Assign id e1 : unPhi block ++ [Assign id e2]
unPhi block
  = block

{- ACCOUNT FOR ALL CASES! you need to recurse.

unPhi []
   = []
unPhi (If p q r : b)
   = If p q' r' : unPhi b'
   where
     q' = unPhi q ++ ass
     r' = unPhi r ++ ass'
     (ass, ass', b') = getPhiAssignments b
unPhi (DoWhile db p : b)
   = ass ++ (DoWhile db'' p : unPhi b)
   where
     db'' = unPhi db' ++ ass'
     (ass, ass', db') = getPhiAssignments db
unPhi (assign : b)
   = assign : unPhi b

getPhiAssignments (Assign v (Phi e e') : b)
   = (Assign v e : ass, Assign v e' : ass', unPhi b')
   where
     (ass, ass', b') = getPhiAssignments b
getPhiAssignments b
   = ([], [], b)

-}

------------------------------------------------------------------------
-- Part IV

makeSSA :: Function -> Function
makeSSA
  = undefined


------------------------------------------------------------------------
-- Predefined functions for displaying functions and blocks...

opNames
  = [(Add, "+"), (Mul, "*"), (Eq, "=="), (Gtr, ">")]

precTable
  = [(Add, 1), (Mul, 2), (Eq, 0), (Gtr, 0)]

prec op
  = lookUp op precTable

showArgs []
  = ""
showArgs as
  = foldr1 (\a s -> a ++ (", " ++ s)) as

showExp :: Int -> Exp -> String
showExp _ (Const n)
  = show n
showExp _ (Var id)
  = id
showExp n (Apply op' e e')
  | n > n'    = "(" ++ s ++ ")"
  | otherwise = s
  where
    n' = prec op'
    s = showExp n' e ++ " " ++ fromJust (lookup op' opNames ) ++ " " ++
        showExp n' e'
showExp _ (Phi e e')
  = "PHI(" ++ showArgs (map (showExp 0) [e, e']) ++ ")"

showLine s n k
  =  putStrLn (show n ++ ": " ++ replicate (k + 2 - length (show n)) ' ' ++ s)

showBlock' b n
  = showBlock'' b n 2
  where
    showBlock'' :: Block -> Int -> Int -> IO Int
    showBlock'' [] n k
      = return n
    showBlock'' (s : b) n k
      = do n'  <- showStatement s n k
           n'' <- showBlock'' b n' k
           return n''
    showStatement (Assign id e) n k
      = do showLine (id ++ " = " ++ showExp 0 e) n k
           return (n + 1)
    showStatement (If p q []) n k
      = do showLine ("if " ++ "(" ++ showExp 0 p ++ ") {") n k
           n' <- showBlock'' q (n + 1) (k + 2)
           showLine "}" n' k
           return (n' + 1)
    showStatement (If p q r) n k
      = do showLine ("if " ++ "(" ++ showExp 0 p ++ ") {") n k
           n'  <- showBlock'' q (n + 1) (k + 2)
           showLine "} else {" n' k
           n'' <- showBlock'' r (n' + 1) (k + 2)
           showLine "}" n'' k
           return (n'' + 1)
    showStatement (DoWhile b p) n k
      = do showLine "do {" n k
           n' <- showBlock'' b (n + 1) (k + 2)
           showLine ("} while " ++ showExp 9 p) n' k
           return (n' + 1)

showFun :: Function -> IO()
showFun (name, args, body)
  = do putStrLn ("1:  " ++ name ++ "(" ++ showArgs args ++ ") {")
       n <- showBlock' body 2
       showLine "}" n 0

showBlock ::  Block -> IO()
showBlock b
  = do n <- showBlock' b 1
       return ()

------------------------------------------------------------------------
-- Example state and expressions for testing...

s1 :: State
s1 = [("x", 7), ("y", 8)]

e1, e2, e3, e4, e5 :: Exp
e1 = Var "x"
e2 = Apply Mul (Apply Add (Var "x") (Const 1)) (Var "y")
e3 = Phi (Const 2) (Const 2)
e4 = Apply Add (Const 0) (Var "x")
e5 = Apply Add (Var "a") (Var "x")

------------------------------------------------------------------------
-- Example functions...

-- Figure 1...
example :: Function
example
  = ("example",["x"],[Assign "a" (Const 1),Assign "b" (Apply Add (Var "x")
    (Const 2)),Assign "c" (Const 3),If (Apply Eq (Var "x") (Const 10))
    [Assign "a" (Const 1),Assign "c" (Const 5)] [],Assign "d"
    (Apply Add (Var "a") (Const 3)),Assign "e" (Apply Add (Var "d") (Var "b")),
    Assign "$return" (Apply Add (Var "e") (Var "c"))])

test :: Block
test
  = [If (Apply Eq (Var "x") (Const 10)) [Assign "a1" (Const 1),
  Assign "c1" (Const 5)] []]

exampleSSA :: Function
exampleSSA
  = ("example",["x"],[Assign "a0" (Const 1),Assign "b0" (Apply Add (Var "x")
    (Const 2)),Assign "c0" (Const 3),If (Apply Eq (Var "x") (Const 10)) [Assign
    "a1" (Const 1),Assign "c1" (Const 5)] [],Assign "a2" (Phi (Var "a1") (Var
    "a0")),Assign "c2" (Phi (Var "c1") (Var "c0")),Assign "d0" (Apply Add (Var
    "a2") (Const 3)),Assign "e0" (Apply Add (Var "d0") (Var "b0")),
    Assign "$return" (Apply Add (Var "e0") (Var "c2"))])

exampleSSAPropagated :: Function
exampleSSAPropagated
  = ("example",["x"],[Assign "b0" (Apply Add (Var "x") (Const 2)),If (Apply Eq
    (Var "x") (Const 10)) [] [],Assign "c2" (Phi (Const 5) (Const 3)),
    Assign "e0" (Apply Add (Const 4) (Var "b0")),Assign "$return"
    (Apply Add (Var "e0") (Var "c2"))])

exampleOptimised :: Function
exampleOptimised
  = ("example",["x"],[Assign "b0" (Apply Add (Var "x") (Const 2)),If (Apply Eq
    (Var "x") (Const 10)) [Assign "c2" (Const 5)] [Assign "c2" (Const 3)],Assign
    "e0" (Apply Add (Const 4) (Var "b0")),Assign "$return" (Apply Add (Var "e0")
    (Var "c2"))])


-- Figure 2 (there is no SSA version of this)...
fact :: Function
fact
  = ("fact",
     ["n"],
     [If (Apply Eq (Var "n") (Const 0))
        [Assign "$return" (Const 1)]
        [Assign "prod" (Const 1),
         Assign "i" (Var "n"),
         DoWhile
           [Assign "prod" (Apply Mul (Var "prod") (Var "i")),
            Assign "i" (Apply Add (Var "i") (Const (-1)))
           ]
           (Apply Gtr (Var "i") (Const 0)),
         Assign "$return" (Var "prod")
        ]
     ]
    )


-- Summation loop, specialised loop for the case k=0...
loop :: Function
loop
  = ("loop",["n"],[Assign "i" (Var "n"),Assign "k" (Const 0),Assign "sum"
    (Const 0),If (Apply Eq (Var "i") (Const 0)) [Assign "$return" (Const 0)]
    [DoWhile [Assign "sum" (Apply Add (Var "sum") (Apply Mul (Apply Add
    (Var "i") (Apply Mul (Const 2) (Var "k"))) (Apply Add (Apply Add (Var "i")
    (Apply Mul (Const 2) (Var "k"))) (Const 1)))),Assign "i" (Apply Add
    (Var "i") (Const (-1)))] (Apply Gtr (Var "i") (Const 0)),
    Assign "$return" (Var "sum")]])

loopSSA :: Function
loopSSA
  = ("loop",["n"],[Assign "i0" (Var "n"),Assign "k0" (Const 0),Assign "sum0"
    (Const 0),If (Apply Eq (Var "i0") (Const 0)) [Assign "$return" (Const 0)]
    [DoWhile [Assign "sum1" (Phi (Var "sum0") (Var "sum2")),Assign "i1"
    (Phi (Var "i0") (Var "i2")),Assign "k1" (Apply Mul (Var "k0") (Const 2)),
    Assign "a0" (Apply Add (Var "i1") (Var "k1")),Assign "k2" (Apply Mul
    (Var "k0") (Const 2)),Assign "b0" (Apply Add (Var "k2") (Const 1)),
    Assign "b1" (Apply Add (Var "i1") (Var "b0")),Assign "m0" (Apply Mul
    (Var "a0") (Var "b1")),Assign "sum2" (Apply Add (Var "sum1") (Var "m0")),
    Assign "i2" (Apply Add (Var "i1") (Const (-1)))] (Apply Gtr (Var "i2")
    (Const 0)),Assign "$return" (Var "sum2")]])

loopSSAPropagated :: Function
loopSSAPropagated
  = ("loop",["n"],[Assign "i0" (Var "n"),If (Apply Eq (Var "i0") (Const 0))
    [Assign "$return" (Const 0)] [DoWhile [Assign "sum1" (Phi (Const 0) (Var
    "sum2")),Assign "i1" (Phi (Var "i0") (Var "i2")),Assign "a0" (Var "i1"),
    Assign "b1" (Apply Add (Var "i1") (Const 1)),Assign "m0" (Apply Mul
    (Var "a0") (Var "b1")),Assign "sum2" (Apply Add (Var "sum1") (Var "m0")),
    Assign "i2" (Apply Add (Var "i1") (Const (-1)))] (Apply Gtr (Var "i2")
    (Const 0)),Assign "$return" (Var "sum2")]])

loopOptimised :: Function
loopOptimised
  = ("loop",["n"],[Assign "i0" (Var "n"),If (Apply Eq (Var "i0") (Const 0))
    [Assign "$return" (Const 0)] [Assign "sum1" (Const 0),Assign "i1" (Var
    "i0"),DoWhile [Assign "a0" (Var "i1"),Assign "b1" (Apply Add (Var "i1")
    (Const 1)),Assign "m0" (Apply Mul (Var "a0") (Var "b1")),Assign "sum2"
    (Apply Add (Var "sum1") (Var "m0")),Assign "i2" (Apply Add (Var "i1")
    (Const (-1))),Assign "sum1" (Var "sum2"),Assign "i1" (Var "i2")]
    (Apply Gtr (Var "i2") (Const 0)),Assign "$return" (Var "sum2")]])


-- Basic block (no conditionals or loops)...
basicBlock :: Function
basicBlock
  = ("basicBlock",[],[Assign "x" (Const 1),Assign "y" (Const 2),Assign "x"
    (Apply Add (Var "x") (Var "y")),Assign "y" (Apply Mul (Var "x") (Const
    3)),Assign "$return" (Var "y")])

basicBlockSSA :: Function
basicBlockSSA
  = ("basicBlock",[],[Assign "x0" (Const 1),Assign "y0" (Const 2),Assign "x1"
    (Apply Add (Var "x0") (Var "y0")),Assign "y1" (Apply Mul (Var "x1") (Const
    3)),Assign "$return" (Var "y1")])

basicBlockSSAPropagated :: Function
basicBlockSSAPropagated
  = ("basicBlock", [], [Assign "$return" (Const 9)])

-- (This is the same as above, as there were no phi functions.)
basicBlockOptimised :: Function
basicBlockOptimised
  = ("basicBlock", [], [Assign "$return" (Const 9)])

-- Computes the maximum of two integers; useful for testing unPhi...
max2 :: Function
max2
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m" (Var "x")]
    [Assign "m" (Var "y")],Assign "$return" (Var "m")])

max2SSA :: Function
max2SSA
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m0" (Var
    "x")] [Assign "m1" (Var "y")],Assign "m2" (Phi (Var "m0") (Var "m1")),Assign
    "$return" (Var "m2")])

max2SSAPropagated :: Function
max2SSAPropagated
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m0" (Var
    "x")] [Assign "m1" (Var "y")],Assign "m2" (Phi (Var "m0") (Var "m1")),Assign
    "$return" (Var "m2")])

max2Optimised :: Function
max2Optimised
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m0" (Var
    "x"),Assign "m2" (Var "m0")] [Assign "m1" (Var "y"),Assign "m2" (Var
    "m1")],Assign "$return" (Var "m2")])

