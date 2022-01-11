-- TOTAL: 24 / 30. 80%. 

import Data.Maybe
import Data.List

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue |
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p
  where
    log2 x = logBase 2 x

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++
                      " in table: " ++ show table))
              (lookup x table)

--------------------------------------------------------------------
-- PART I. 11/14
--------------------------------------------------------------------

-- Check if every item in a given list is the same. 2/2
allSame :: Eq a => [a] -> Bool
allSame list
  = all (== head list) (tail list)

-- Remove an item of type a from a table of (a, b) pairs. 1/2
remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove item
  = filter (\(a, b) -> a /= item) -- You don't need b here.
  -- Alternatively, can be written as filter ((/= x) . fst)

-- Look up the value of a given attribute in a given data row. 2/2
lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt attribute header row
  = lookUp attribute (zip (map fst header) row)
  -- Alternatively: row !! fromJust (elemIndex attribute (map fst header))
  -- but I prefer this solution. Shorter. Use more concise variable names.

-- Remove the value of a named attribute from a given data row. 2/2
removeAtt :: AttName -> Header -> Row -> Row
removeAtt attribute header row
  = delete (lookUpAtt attribute header row) row
  -- Alternatively: attribute \\ [lookUpAtt attribute header row]
  -- Double backslash is list difference operator.

-- Add a new pair to a given mapping. 2/3
addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
addToMapping (key, value) mapping
  | isNothing binding = mapping ++ [(key, [value])]
  | otherwise         = remove key mapping ++ [(key, value : fromJust binding)]
  where
    binding = lookup key mapping
  -- Alternative: you can destructure mapping, which could fix the order.
{-   This is more elegant than the multi-step process of removal in line 72.

addToMapping (key, val) []
  = [(key, [val])]
addToMapping (key, val) (pair@(pKey, pVal) : pairs)
  | key == key' = (key, val : pVal) : pairs
  | otherwise   = pair : addToMapping (key, val) pairs

-}

-- Build a frequency table for each value of an attribute in a data set. 2/3
buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Pre: Each row of the data set contains an instance of the attribute
buildFrequencyTable attribute (header, rows) -- Destructure attribute -> (attName, attValue)
  = map (\name -> (name, (length . filter (== name)) values)) attrNames
  -- Try using list comprehensions here instead!
  where
    -- Use lookUpAtt: vals = map (lookUpAtt attName header)
    values = map (!! fromJust (elemIndex attribute header)) rows
    attrNames = fromMaybe [] (lookup (fst attribute) header)
-- Note: I think the last test case is wrong. 
-- If no headers are given, why would we return fishingData values?

--------------------------------------------------------------------
-- PART II. 5/5
--------------------------------------------------------------------

-- Count the total number of nodes and leaves in a given decision tree. 2/2
nodes :: DecisionTree -> Int
nodes Null
  = 0
nodes (Leaf value)
  = 1
nodes (Node name branches)
  = 1 + sum (map (nodes . snd) branches)

-- Evaluate a given tree using the attribute values in a given data row. 3/3
evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _
  = ""
evalTree (Leaf result) _ _
  = result
evalTree (Node attrName branches) header row
  = evalTree subtree header row
  where
    value   = lookUpAtt attrName header row
    subtree = lookUp value branches

--------------------------------------------------------------------
-- PART III. 8/9
--------------------------------------------------------------------

--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

-- Partition a data set using the specified attribute. 4/5
-- Note: I use reverse to match the order of elements in the test cases
partitionData :: DataSet -> Attribute -> Partition
partitionData (header, rows) (attr, _)
  = map (\(a, t) -> (a, (newHeader, reverse t))) (foldl mapValRows [] rows)
  -- [(a, (newHeeader, t)) | (a, t) <- foldlmapValRows [] rows] List comprehension.
  where
    -- In this case, you actually might want to use more variables to be clearer
    newHeader = remove attr header
    mapValRows dict row
      = addToMapping (lookUpAtt attr header row, removeAtt attr header row) dict

-- Build a decision tree from a given data set and attribute selector function. 4/4.
buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree
-- Base cases: every row has the same classification or data set is empty
-- Could write null as its own case, then put allSame into below function.
buildTree (header, rows) (attr, _) _
  | null rows           = Null
  | allSame classValues = Leaf (head classValues)
  where
    classValues = map (lookUpAtt attr header) rows
-- Recursively build tree
buildTree table@(header, rows) classAttr@(attr, val) selector
  = Node name (map buildChildren partitions) -- Again, use list comprehensions
  -- Node name [(attVal, buildTree pTable classAttr selector) | (attVal, pTable) <- partitions]
  where
    selectAttr@(name, _) = nextAtt table classAttr
    partitions           = partitionData table selectAttr
    buildChildren :: (AttValue, DataSet) -> (AttValue, DecisionTree)
    buildChildren (attValue, parTable)
      = (attValue, buildTree parTable classAttr selector)

--------------------------------------------------------------------
-- PART IV
--------------------------------------------------------------------

entropy :: DataSet -> Attribute -> Double
entropy
  = undefined

gain :: DataSet -> Attribute -> Attribute -> Double
gain
  = undefined

bestGainAtt :: AttSelector
bestGainAtt
  = undefined

--------------------------------------------------------------------

outlook :: Attribute
outlook
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute
temp
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute
humidity
  = ("humidity", ["high", "normal"])

wind :: Attribute
wind
  = ("wind", ["windy", "calm"])

result :: Attribute
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header
  =  [outlook,    temp,   humidity, wind,    result]
table
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header'
  =  [outlook,    result, temp,   humidity, wind]
table'
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

{-

Node "outlook" [("sunny",Node "temp" [("hot",Null),("cool",Null),("mild",Node "humidity" [("high",Null),("normal",Null)])]),("overcast",Null),("rainy",Node "temp" [("cool",Node "humidity" [("normal",Node "wind" [("calm",Null),("windy",Null)])]),("mild",Node "humidity" [("normal",Null),("high",Node "wind" [("calm",Null),("windy",Null)])])])]

-}

fig1 :: DecisionTree
fig1
  = Node "outlook"
         [("sunny", Node "temp"
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity"
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp"
                         [("hot", Null),
                          ("mild", Node "humidity"
                                        [("high",Node "wind"
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity"
                                        [("high", Null),
                                         ("normal", Node "wind"
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook"
         [("sunny", Node "humidity"
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind"
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]