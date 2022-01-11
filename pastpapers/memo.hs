{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Timings: 15 min for reading. 50 min for part I (!).
-- 25 min for part II. 90 min for part III.
-- 21/28 = 75%

import Data.Maybe
import Data.List

-- All programs are assumed to be well-formed in the following sense:
--
-- All operators, functions and procedures will always be applied
-- to the correct number of arguments, all of which will be of the appropriate
-- type.
--
-- Boolean-valued expressions will always evaluate to either 0 (false) or 1
-- (true).
--
-- In an array element assignment the array being assigned to will always be 
-- in scope.
--
-- In a procedure call of the form Call x p es the procedure p will always exit 
-- via a Return statement.
--
-- A Return statement will always be the last statement to be executed in a 
-- procedure's defining code block (there is no `dead code').
--

--------------------------------------------------------------------
type Id = String

data Value = I Int | A [(Int, Int)]
           deriving (Eq, Show)

data Op = Add | Mul | Less | Equal | Index
          deriving (Eq, Show)

data Exp = Const Value |
           Var Id |
           OpApp Op Exp Exp |
           Cond Exp Exp Exp |
           FunApp Id [Exp]
         deriving (Eq, Show)

type FunDef = (Id, ([Id], Exp))

type Block = [Statement]

data Statement = Assign Id Exp |
                 AssignA Id Exp Exp |
                 If Exp Block Block |
                 While Exp Block |
                 Call Id Id [Exp] |
                 Return Exp
               deriving (Eq, Show)

type ProcDef = (Id, ([Id], Block))

data Scope = Local | Global
           deriving (Eq, Show)

type Binding = (Id, (Scope, Value))

type State = [Binding]

--------------------------------------------------------------------
-- Part I. 6/7.

-- Look up the Value of a given variable in a given state. 1/1
getValue :: Id -> State -> Value
-- Pre: The identifier has a binding in the state
getValue x
  = snd . lookUp x

-- Return the list of local variables in a given state in order. 1/1
getLocals :: State -> State
getLocals state
  = [ x | x@(_, (scope, _)) <- state, scope == Local]
  -- Or: filter (\(_, (scope, _)) -> scope == Local) state

-- Return the list of global variables in a given state in order. 1/1
getGlobals :: State -> State
getGlobals state
  = [ x | x@(_, (scope, _)) <- state, scope == Global]
  -- Alternatively, state \\ getLocals state

-- Returns an array with its ith element modified to bind to v. 1/2.
assignArray :: Value -> Value -> Value -> Value
-- The arguments are the array, index and (new) value respectively
-- Pre: The three values have the appropriate value types (array (A), 
--      integer (I) and integer (I)) respectively.
assignArray (A arr) (I i) (I v)
  = A ((i, v) : filter (/= arr !! i) arr)
  -- Edge case: no guarantee that i is an index in the length of arr!


-- Update the value of a variable in a given state whilst preserving its scope. 2/2
updateVar :: (Id, Value) -> State -> State
{-
updateVar (id, v) (b@(id', (s, _)) : bs)
  | id' == id = (id, (s, v)) : bs
  | otherwise = b : updateVar (id, v) bs -}

updateVar (id, v) s
  | isNothing oldv = s ++ [(id, (Local, v))]
  | otherwise      = (id, (scp, v)) : filter (/= fromJust oldv) s
  where
    oldv = find (\(id', _) -> id' == id) s
    scp = (fst . snd) (fromJust oldv)


  -- Get the index of the variable to be updated.
  -- If it exists, assignArray state index value
  -- If it doesn't exist, create a new binding: (Id, (Local, Value))

---------------------------------------------------------------------
-- Part II. 12/12

-- Apply the given operator to the given argument value. 3/3
applyOp :: Op -> Value -> Value -> Value
-- Pre: The values have the appropriate types (I or A) for each primitive
applyOp Add (I x) (I y)
  = I (x + y)
applyOp Mul (I x) (I y)
  = I (x * y)
applyOp Less (I x) (I y)
  = I (boolToInt (x < y))
applyOp Equal (I x) (I y)
  = I (boolToInt (x == y))
applyOp Index (A arr) (I i)
  | isNothing v = I 0
  | otherwise   = I (fromJust v)
  where
    v = lookup i arr
{- alternative recursive solution
applyOp Index (A []) (I _)
  = I 0
applyOp Index (A ((x, y) : xys)) (I i)
  | x == i    = I y
  | otherwise = applyOp Index (A xys) (I i)
 -}

-- Helper function: boolean to integer
boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0 -- very pythonic...

-- Generate a list of bindings. 1/1
bindArgs :: [Id] -> [Value] -> State
-- Pre: the lists have the same length
bindArgs
  = zipWith (\i v -> (i, (Local, v)))
  -- Alternative: zip ids (zip (repeat Local) vals)

-- Returns a list of values by applying eval to each expression in a list
evalArgs :: [Exp] -> [FunDef] -> State -> [Value]
evalArgs es fs s
  = [eval e fs s | e <- es] -- map (\e -> eval e fs s) es

-- Returns the value of an expression evaluated using a list of FunDefs. 8/8
eval :: Exp -> [FunDef] -> State -> Value
-- Pre: All expressions are well formed
-- Pre: All variables referenced have bindings in the given state
eval (Const c) _ _ -- good
  = c
eval (Var v) _ s -- good
  = getValue v s
eval (OpApp op e e') fs s -- good
  = applyOp op (eval e fs s) (eval e' fs s)
eval (Cond c e e') fs s -- good
  | eval c fs s == I 1 = eval e fs s
  | otherwise          = eval e' fs s
eval (FunApp i es) fs s -- good!
  = eval e fs (s' ++ s)
  where
    (as, e) = lookUp i fs
    vs = evalArgs es fs s
    s' = bindArgs as vs

---------------------------------------------------------------------
-- Part III. 3/8
-- type State = [(Id, (Scope, Value))]
-- Input state: argument values, global variable values
-- Output state: argument (local) values, (possibly modified) global variables

-- Execute statements to transform the given input state into a final state. 3/6
executeStatement :: Statement -> [FunDef] -> [ProcDef] -> State -> State
-- Pre: All statements are well formed 
-- Pre: For array element assignment (AssignA) the array variable is in scope,
--      i.e. it has a binding in the given state
executeStatement (Assign i e) fs ps s -- good
  = updateVar (i, eval e fs s) s
executeStatement (AssignA i e e') fs ps s -- !! NO
  = (i, (Local, assignArray (A a) (I index) v)) : s' -- use updateVar instead!
  -- = updateVar (i, assignArray (getValue i s) (A a) v)) s
  where
    v = eval e' fs s
    s' = updateVar (i, v) s
    (A a) = eval e fs s' -- getValue i s
    index = fromJust (findIndex (\(id, _) -> i == id) s')
executeStatement (If e b b') fs ps s -- good
  | eval e fs s == I 1 = executeBlock b fs ps s
  | otherwise          = executeBlock b' fs ps s
executeStatement st@(While e b) fs ps s -- good
  | eval e fs s == I 1 = executeStatement st fs ps (executeBlock b fs ps s)
  | otherwise          = s
-- Update
executeStatement (Call x p es) fs ps s -- incorrect, mixed yourself up with vars
  | null x    = execution -- getGlobals execution ++ locals
  | otherwise = updateVar (x, getValue "$res" execution) execution -- getGlobals execution ++ locals
  where
    (args, procedure) = lookUp p ps
    bindArgs = [(arg, eval v fs s) | arg <- args, v <- es]
    s' = foldl (flip updateVar) s bindArgs
    execution = executeBlock procedure fs ps s'
    -- missing var: getGlobals execution ++ locals
executeStatement (Return e) fs ps s
  = ("$res", (Local, eval e fs s)) : s -- use UPDATE VAR PLEASE

-- Input state: bindings for procedure’s argument values and global variables. 0/2
-- type Block = [Statement]
executeBlock :: Block -> [FunDef] -> [ProcDef] -> State -> State
-- Pre: All code blocks and associated statements are well formed
executeBlock sts fs ps s
  = getGlobals execBlockState ++ returnRes ++ locals
  where
    locals = getLocals s
    globals = getGlobals s
    pState = globals \\ [g | l@(i, _) <- locals, g@(i', _) <- globals, i == i']
    -- If local argument has same name as global variable, delete global variable
    execS :: State -> Statement -> State
    execS state statement 
      = executeStatement statement fs ps state
    execBlockState = foldl execS pState sts
    returnRes = filter (\(id, _) -> id == "$res") execBlockState

-- You mixed up executeBlock and executeStatement's functions.
{- if foldl isn't working, use recursion!
executeBlock [] _ _ bs
  = bs
executeBlock (s : ss) fs ps bs
  = executeBlock ss fs ps (executeStatement s fs ps bs)
-}
---------------------------------------------------------------------
-- Part IV

translate :: FunDef -> Id -> [(Id, Id)] -> ProcDef
translate (name, (as, e)) newName nameMap
  = (newName, (as, b ++ [Return e']))
  where
    (b, e', ids') = translate' e nameMap ['$' : show n | n <- [1..]]

translate' :: Exp -> [(Id, Id)] -> [Id] -> (Block, Exp, [Id])
translate'
  = undefined

---------------------------------------------------------------------
-- PREDEFINED FUNCTIONS

-- A helpful predefined lookUp function...
lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp x t
  = fromMaybe (error ("\nAttempt to lookUp " ++ show x ++
                      " in a table that only has the bindings: " ++
                      show (map fst t)))
              (lookup x t)

 -- Turns an int into an Exp...
intToExp :: Int -> Exp
intToExp n
  = Const (I n)

-- Turns a list of ints into an array Exp...
listToExp :: [Int] -> Exp
listToExp
  = Const . listToVal

-- Turns a list of ints into an array Value...
listToVal :: [Int] -> Value
listToVal xs
  = A (zip [0..] xs)

-- memoise generates a procedure that caches values computed by function f.  
-- In general f will be a variant of some originally recursive function 
-- that calls the procedure generated here (named p) instead of itself.
-- Arguments:
--    p = procedure name; a = argument name; f = function variant; 
--    pt = 'isPresent' table; mt = memo table.

memoise :: Id -> Id -> Id -> Id -> Id -> ProcDef
memoise p a f pt mt
  = (p,
     ([a], [If (OpApp Equal (OpApp Index (Var pt) (Var a)) (Const (I 0)))
               [Call "x" f [Var a],
                AssignA pt (Var a) (Const (I 1)),
                AssignA mt (Var a) (Var "x")
               ]
               [],
            Return (OpApp Index (Var mt) (Var a))
           ]
     )
    )


---------------------------------------------------------------------
-- Predefined States, arrays and expressions for testing...

sampleState, gState, fibState :: State
sampleState
  = [("x", (Local, I 5)), ("y", (Global, I 2)), ("a", (Global, listToVal [4,2,7]))]

gState
  = [("gSum", (Global, I 0))]

fibState
  = [("fibPres", (Global, A [])), ("fibTab", (Global, A []))]

sampleArray :: Exp
sampleArray
  = Const (listToVal [9,5,7,1])

e1, e2, e3, e4, e5 :: Exp
e1 = Const (I 1)
e2 = Var "y"
e3 = OpApp Add (Var "x") (Const (I 2))
e4 = Cond e1 (Var "x") (Const (I 9))
e5 = FunApp "fib" [Const (I 6)]

---------------------------------------------------------------------
-- Example (pure) functions for testing...

-- Equivalent of Haskell's max function...
biggest :: FunDef
biggest
  = ("biggest",
     (["m", "n"], Cond (OpApp Less (Var "m") (Var "n"))
                       (Var "n")
                       (Var "m"))
    )

-- Factorial, equivalent to: if n == 0 then 1 else n * fact (n - 1)...
fac :: FunDef
fac
  = ("fac",
     (["n"], Cond (OpApp Equal (Var "n") (intToExp 0))
                  (intToExp 1)
                  (OpApp Mul (Var "n")
                             (FunApp "fac" [OpApp Add (Var "n") (intToExp (-1))])))
    )

-- Sums elements 0..n of an array...
sumA :: FunDef
sumA
  = ("sumA",
     (["a", "n"], Cond (OpApp Less (Var "n") (Const (I 0)))
                       (Const (I 0))
                       (OpApp Add (OpApp Index (Var "a") (Var "n"))
                                  (FunApp "sumA"
                                     [Var "a", OpApp Add (Var "n")
                                                         (Const (I (-1)))]))
     )
    )


-- Vanilla Haskell fib
fibH :: Int -> Int
-- Pre: n > 0
fibH n
  = if n < 3 then 1 else fibH (n-1) + fibH (n-2)

-- fib in the purely functional subset
fib :: FunDef
fib
  = ("fib",
     (["n"], Cond (OpApp Less (Var "n") (Const (I 3)))
                  (Const (I 1))
                  (OpApp Add (FunApp "fib" [OpApp Add (Var "n") (Const (I (-1)))])
                             (FunApp "fib" [OpApp Add (Var "n") (Const (I (-2)))]))
     )
    )

-- May be useful for testing translate...?
testFun :: FunDef
testFun
  = ("testFun",
     (["x", "y"], Cond (OpApp Equal (Var "x") (Var "y"))
                       (Cond (FunApp "p" [Var "y"])
                             (OpApp Add (Var "x") (Const (I 1)))
                             (OpApp Add (Var "x") (Var "y")))
                       (OpApp Add (FunApp "g" [Var "y"]) (Const (I 2))))
    )

---------------------------------------------------------------------
-- Example procedures for testing...

-- Add two integers and assign the result to a global variable, gSum, 
-- that is assumed to be in scope when the procedure is called...
gAdd :: ProcDef
gAdd
  = ("gAdd",
     (["x", "y"], [Assign "gSum" (OpApp Add (Var "x") (Var "y"))])
    )

-- Sums elements 0..n of an array...
sumA' :: ProcDef
sumA'
  = ("sumA'",
     (["a", "n"], [Assign "s" (Const (I 0)),
                   Assign "i" (Const (I 0)),
                   Assign "limit" (OpApp Add (Var "n") (Const (I 1))),
                   While (OpApp Less (Var "i") (Var "limit"))
                         [Assign "s" (OpApp Add (Var "s")
                                                (OpApp Index (Var "a") (Var "i"))),
                          Assign "i" (OpApp Add (Var "i") (Const (I 1)))
                         ],
                   Return (Var "s")]
     )
    )

-- A procedural version of fib...
fibP :: ProcDef
-- Pre: n > 0
fibP
  = ("fibP",
     (["n"], [If (OpApp Less (Var "n") (Const (I 3)))
                 [Return (Const (I 1))]
                 [Call "f1" "fibP" [OpApp Add (Var "n") (Const (I (-1)))],
                  Call "f2" "fibP" [OpApp Add (Var "n") (Const (I (-2)))],
                  Return (OpApp Add (Var "f1") (Var "f2"))
                 ]
             ]
     )
    )

fibManager :: ProcDef
fibManager
  = ("fibManager",
     (["n"], [If (OpApp Equal (OpApp Index (Var "fibPres") (Var "n"))
                              (Const (I 0)))
                 [Call "x" "fibM" [Var "n"],
                  AssignA "fibPres" (Var "n") (Const (I 1)),
                  AssignA "fibTab" (Var "n") (Var "x")
                 ]
                 [],
              Return (OpApp Index (Var "fibTab") (Var "n"))
             ]
     )
    )

fibM :: ProcDef
-- Pre: n > 0
-- The value of fibMGenerator below
fibM
  = ("fibM",
     (["n"], [If (OpApp Less (Var "n") (Const (I 3)))
                 [Assign "$3" (Const (I 1))]
                 [Call "$1" "fibManager" [OpApp Add (Var "n") (Const (I (-1)))],
                  Call "$2" "fibManager" [OpApp Add (Var "n") (Const (I (-2)))],
                  Assign "$3" (OpApp Add (Var "$1") (Var "$2"))
                 ],
              Return (Var "$3")
             ]
     )
    )

---------------------------------------------------------------------
-- Sample top-level calls for testing...

-- This instantiates the table manager template (predefined)...
fibTableManager :: ProcDef
fibTableManager
  = memoise "fibManager" "n" "fibM" "fibPres" "fibTab"

-- This uses the translate function to build the procedural, memoised,
-- version of fib...
fibMGenerator :: ProcDef
fibMGenerator
  = translate fib "fibM" [("fib", "fibManager")]


-- Useful predefined executors...

execBiggest :: Int -> Int -> State
execBiggest m n
  = executeBlock [Return (FunApp "biggest" [intToExp m, intToExp n])] [biggest] [] []

execFac :: Int -> State
execFac n
  = executeBlock [Return (FunApp "fac" [intToExp n])] [fac] [] []

execSumA :: [Int] -> Int -> State
execSumA a n
  = executeBlock [Return (FunApp "sumA" [listToExp a, intToExp n])] [sumA] [] []

execGAdd :: Int -> Int -> State
execGAdd x y
  = executeBlock [Call "" "gAdd" [intToExp x, intToExp y]] [] [gAdd] gState

execSumA' :: [Int] -> Int -> State
execSumA' a n
  = executeBlock [Call "s" "sumA'" [listToExp a, intToExp n]] [] [sumA'] []

execGlobalSumA' :: [Int] -> Int -> State
execGlobalSumA' a n
  = executeBlock [Call "s" "sumA'" [listToExp a, intToExp n]]
                 [] [sumA'] [("s", (Global, I 0))]

execFibP :: Int -> State
execFibP n
  = executeBlock [Call "f" "fibP" [intToExp n]] [] [fibP] fibState

execFibM :: Int -> State
execFibM n
  = executeBlock [Call "f" "fibM" [intToExp n]] [] [fibM, fibManager] fibState
