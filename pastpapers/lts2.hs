import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process]
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I. 8/8

lookUp :: Eq a => a -> [(a, b)] -> b -- 1/1
--Pre: The item is in the table
lookUp x
  = fromJust . lookup x

states :: LTS -> [State] -- 2/2
states
  = nub . concatMap (\((s, s'), _) -> [s, s'])

transitions :: State -> LTS -> [Transition] -- 3/3
transitions state
  = filter (\((s, _), _) -> s == state)
  -- Alternatively, [ t | t@((s, _), _) <- lts, s == state]

alphabet :: LTS -> Alphabet -- 2/2
alphabet
  = nub . map snd

------------------------------------------------------
-- PART II. 11/11

actions :: Process -> [Id] -- 5/5
actions (Prefix id process)
  = nub (id : actions process)
actions (Choice processes)
  = nub (concatMap actions processes)
actions _
  = []

accepts :: [Id] -> [ProcessDef] -> Bool -- 6/6
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts trace pDefs
  = accepts' trace (snd (head pDefs))
  where
    accepts' :: [Id] -> Process -> Bool
    accepts' [] _
      = True
    accepts' trace' (Choice options)
      = any (accepts' trace') options
    accepts' (t : ts) (Prefix id process)
      | t == id   = accepts' ts process
      | otherwise = False
    accepts' trace' (Ref id)
      = accepts' trace' (lookUp id pDefs)
    accepts' trace' STOP
      = False

------------------------------------------------------
-- PART III. 8/9

composeTransitions :: Transition -> Transition
                   -> Alphabet -> Alphabet
                   -> StateMap
                   -> [Transition] -- 5/5
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s, t), a) ((s', t'), a') alp1 alp2 m
  | a == a'                         = [((ss', tt'), a)]
  | a `elem` alp2 && a' `elem` alp1 = []
  | a' `elem` alp1                  = [((ss', ts'), a)]
  | a `elem` alp2                   = [((ss', st'), a')]
  | otherwise                       = [((ss', ts'), a), ((ss', st'), a')]
  where
    ss' = lookUp (s, s') m
    tt' = lookUp (t, t') m
    ts' = lookUp (t, s') m
    st' = lookUp (s, t') m

pruneTransitions :: [Transition] -> LTS -- 3/4
pruneTransitions ts
  = visit [] [] 0
  where
    visit :: [State] -> [Transition] -> State -> LTS
    visit seen res currS
      | currS `elem` seen || null outgoing = res
      | otherwise                          = concatMap (visit seen' res') nextS
      where
        outgoing = transitions currS ts
        seen'    = currS : seen
        res'     = outgoing ++ res
        nextS    = map (snd . fst) outgoing

-- More elegant solution is possible: bottom-up recursion (base case = [])
{- 

pruneTransitions :: [Transition] -> LTS
pruneTransitions ts
  = visit 0 []
  where
    visit :: State -> [State] -> [Transition]
    visit s ss 
      | elem s ss = []
      | otherwise = tran ++ next
        where
          tran   = transitions s ts 
          next   = concat (map (\s' -> visit s' (s : ss)) target)
          target = [to | ((_, to), _) <- tran]

-}

-- For testing
m :: StateMap
m = [((0,0),0),((0,1),1),((1,0),2),((1,1),3)]

------------------------------------------------------
-- PART IV. 4/4

compose :: LTS -> LTS -> LTS -- 4/4
compose lts1 lts2
  = (nub . pruneTransitions) composedTs
  where
    stateProduct = [(s, s') | s <- states lts1, s' <- states lts2]
    mapS = zip stateProduct [0..]
    composedTs = concatMap getCompositions stateProduct
    getCompositions :: (State, State) -> [Transition]
    getCompositions (s1, s2)
      = concat [composeTransitions t t' alp1 alp2 mapS | t <- ts1, t' <- ts2]
      where
        (ts1, alp2) = getTransitions s1 lts1 (alphabet lts2) "$"
        (ts2, alp1) = getTransitions s2 lts2 (alphabet lts1) "$'"

getTransitions :: State -> LTS -> Alphabet -> Id -> ([Transition], Alphabet)
getTransitions s lts alp sentinel
  | null outgoing = ([dummy], sentinel : alp)
  | otherwise     = (outgoing, alp)
  where
    outgoing = transitions s lts
    dummy = ((s, s), sentinel)

------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS
  = undefined

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")),
                     Prefix "end" STOP])

maker
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch
  = ("SWITCH", Ref "OFF")

off
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS,
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS
  = [((0,1),"tick"),((1,0),"tock")]

playLTS
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS
  = [((0,1),"make"),((1,0),"ready")]

userLTS
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS
  = [((0,1),"on"),((1,0),"off")]
