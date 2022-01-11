-- 24/30 = 80%    Completely average hahaha 
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
-- PART I (20 min) 8/8

-- Look up a given item in a list of item value pairs (1/1)
lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp item pairs
  = fromJust (lookup item pairs)

-- Compute a list of state indices in a given LTS without duplicates (2/2)
states :: LTS -> [State]
states
  = nub . concatMap (\((s1, s2), _) -> [s1, s2])

-- Return the list of transitions from a specified state in a given LTS (3/3)
transitions :: State -> LTS -> [Transition]
transitions s
  = filter (\((s1, _), _) -> s1 == s)

-- Return the alphabet of a given LTS with no duplicates (2/2)
alphabet :: LTS -> Alphabet
alphabet
  = nub . map snd

------------------------------------------------------
-- PART II (50 min)  5/11 :O

-- Compute the (duplicate free) list of action names in a given FSP (4/5)
actions :: Process -> [Id]
actions STOP
  = []
actions (Ref _)
  = []
actions (Prefix name process)
  = nub (name : actions process)
actions (Choice processes)
  = (nub . concatMap actions) processes
{- Better to use catch-alls for STOP and Ref!
actions _ 
  = []
-}

-- Example: [switch, off, on]
-- Follow action prefixes and replace process names by defining process

-- Start from switch = REF "OFF". 
-- -> Choice [Prefix "on" (Ref "ON")]. Match "on". Recurse over ref.
-- -> Choice [Prefix "off" (Ref "OFF")]. Match "off". Recurse over ref.
-- -> Choice [Prefix "on" (Ref "ON")]. Append "on". Check if matches. Yes.
-- When length of trace and length of list are equal, stop. check. 

-- Checks if the given list of process definitions can produce the given trace
accepts :: [Id] -> [ProcessDef] -> Bool   -- (1/6) don't stick to one example!
--Pre: The first item in the list of process definitions is
--     that of the start process.
{-
accepts t defs
  = startTrace [] (snd (head defs)) == t
  where
    -- Helper function to extract next processes
    getRefs :: Process -> [Process]
    getRefs STOP
      = []
    getRefs (Ref r)
      = [lookUp r defs]
    getRefs (Prefix name process)
      = getRefs process
    getRefs (Choice processes)
      = (nub . concatMap getRefs) processes

    -- Helper function to recurse through actions
    startTrace :: [Id] -> Process -> [Id]
    startTrace trace process
      | length trace == length t = trace
      | otherwise                = concatMap (startTrace trace') next
      where
        next = getRefs process
        trace' = actions process ++ trace
-}
accepts ids defs@(d : _)
  = accepts' ids (snd d)
  where
    accepts' :: [Id] -> Process -> Bool
    accepts' [] _
      = True
    -- Replace process names by defining process to follow transitions
    accepts' ids (Ref id')
      = accepts' ids (lookUp id' defs)
    -- Check action label matches given trace, then proceed
    accepts' (id : ids) (Prefix id' process)
      = id == id' && accepts' ids process
    -- If first choice matches action, check all actions and first choice
    -- Otherwise, check all actions and remaining choices
    accepts' ids@(i : is) (Choice (process : processes))
      | accepts' [i] process = accepts' ids process
      | otherwise            = accepts' ids (Choice processes)
    -- In all other cases, does not match
    accepts' _ _
      = False

------------------------------------------------------
-- PART III (50 min)  8/9

-- Implements transition composition  4/5
composeTransitions :: Transition -> Transition
                   -> Alphabet -> Alphabet
                   -> StateMap
                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s, t), a) ((s', t'), a') a1 a2 i
  | a == a'                     = [((id1, id2), a)]
  | a `elem` a2 && a' `elem` a1 = []
  | a' `elem` a1                = [((id1, id3), a)]
  | a `elem` a2                 = [((id1, id4), a')]
  | otherwise                   = [((id1, id3), a), ((id1, id4), a')]
  where
    id1 = lookUp (s, s') i
    id2 = lookUp (t, t') i
    id3 = lookUp (t, s') i
    id4 = lookUp (s, t') i
  -- Better to rename these variables: ss', tt', ts', st'

-- Return transitions that are ‘reachable’ from state 0   4/4
pruneTransitions :: [Transition] -> LTS
pruneTransitions lts
  = visit 0 []
  where
    -- Helper function: visits a state and its paths
    visit :: State -> [State] -> [Transition]
    visit s visited
      | s `elem` visited = []
      | otherwise        = outgoing ++ concatMap (`visit` visited') targets
      where
        outgoing = transitions s lts
        targets = map (\((_, to), _) -> to) outgoing
        visited' = s : visited

------------------------------------------------------
-- PART IV (60 min)  3/4 Works but can definitely be simplified.

compose :: LTS -> LTS -> LTS
compose lts1 lts2
  = pruneTransitions transList
  where
    statePairs = cartProd (states lts1) (states lts2)
    transPairs = concatMap combineTransitions statePairs
    transList  = concatMap getTransComps transPairs

    -- Helper function: build the Cartesian Product of two sets
    cartProd :: [a] -> [b] -> [(a, b)]
    cartProd xs ys
      = [(x, y) | x <- xs, y <- ys]

    -- Helper function: get transitions from states, adding dummy transitions
    sentinelTrans :: State -> LTS -> [Transition]
    sentinelTrans s lts
      | null transList = [((s, s + 1), '$' : show s)]
      | otherwise      = transList
      where
        transList = transitions s lts

    -- Helper function: build transition combinations from state pairs
    combineTransitions :: (State, State) -> [(Transition, Transition)]
    combineTransitions (s, s')
      = cartProd (sentinelTrans s lts1) (sentinelTrans s' lts2)

    -- MARKING NOTE: you can just add "$1" and "$2" automatically, only 2 LTS
    -- Hence this function is unnecessary!
    -- Helper function: return alphabet, modified if sentinel is detected
    getAlphabet :: Transition -> LTS -> Alphabet
    getAlphabet t lts
      | "$" `isPrefixOf` snd t = snd t : alphabet lts
      | otherwise              = alphabet lts

    -- Helper function: compose transitions from transition pairs
    getTransComps :: (Transition, Transition) -> [Transition]
    getTransComps (t1, t2)
      = composeTransitions t1 t2 a1 a2 mapStates
      where
        a1 = getAlphabet t2 lts1
        a2 = getAlphabet t1 lts2
        mapStates = identifyStatePairs statePairs


-- Helper function: I
identifyStatePairs :: [(State, State)] -> StateMap
identifyStatePairs pairs
  = zip pairs (iterate (1 +) 0)

-- Get states from each LTS (using `states`)
-- Build the cartesian product of states (all ordered combinations)

-- Examine each pair of states (s, s').
-- Get the transitions for s from lts1 and s' from lts2.
-- Build the cartesian product of transitions -> get a pair of transitions.
-- Apply `composeTransitions` for each pair of transitions

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

m :: StateMap
m = [((0,0),0),((0,1),1),((1,0),2),((1,1),3)]