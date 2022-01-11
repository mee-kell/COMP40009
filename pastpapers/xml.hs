-- Timing: managed to make it in 3 hours!
-- Spent 1 hour 45 min on parts I and II. Aim: spend 1 hour 30 min.
-- 26/30 = 87% :D
module Exam where

import Data.Char
import Data.Maybe

type Name = String

type Attributes = [(Name, String)]

data XML = Null | Text String | Element Name Attributes [XML]
         deriving (Eq, Show)

type Stack = [XML]

-----------------------------------------------------------------------
-- Some useful show/print functions

-- The 'show' function for XML objects
showXML :: XML -> String
showXML (Text t)
  = t
showXML (Element n as es)
  = "<" ++ n ++ showAtts as ++ ">" ++ concatMap showXML es ++ "</" ++ n ++ ">"
  where
    showAtts as = concatMap showAtt as
    showAtt (n, v) = " " ++ n ++ "=" ++ "\"" ++ v ++ "\""
showXML Null
  = ""

-- The 'show' function for lists of XML objects
showXMLs :: [XML] -> String
showXMLs
  = concatMap showXML

-- Prints an XML object to the terminal
printXML :: XML -> IO()
printXML
  = putStrLn . showXML

-- Prints a list of XML objects to the terminal (useful for testing the
-- output from expandXML')
printXMLs :: [XML] -> IO()
printXMLs
  = mapM_ printXML

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- Part I. 12/12

skipSpace :: String -> String -- 1/1
skipSpace
  = dropWhile (== ' ')
  -- Alternative: dropWhile isSpace

getAttribute :: String -> XML -> String -- 2/2
getAttribute a (Element _ attributes _)
  = fromMaybe "" (lookup a attributes)
getAttribute _ _
  = ""

getChildren :: String -> XML -> [XML] -- 2/2
getChildren n (Element _ _ children)
  = filter (\c -> getElemName c == n) children
  where
    getElemName (Element x _ _) = x
    getElemName _ = ""
getChildren _ _
  = []

getChild :: String -> XML -> XML -- 2/2
getChild n xml
  | null children = Text ""
  | otherwise     = head children
  where
    children = getChildren n xml

addChild :: XML -> XML -> XML -- 1/1
-- Pre: the second argument is an Element
addChild new (Element n a children)
  = Element n a (children ++ [new])
addChild _ _
  = error "Case not expected"

getValue :: XML -> XML -- 4/4
getValue xml
  = Text (getValue' xml)
  where
    getValue' :: XML -> String
    getValue' (Text t)
      = t
    getValue' (Element _ _ children)
      = concatMap getValue' children
    getValue' _
      = ""

-------------------------------------------------------------------------
-- Part II. 12/12

-- Parses an element/attribute name
parseName :: String -> (Name, String)
parseName []
  = error "Error: attempt to read empty name"
parseName s@(c : cs)
  | isAlpha c = break (not . isNameChar) s
  | otherwise = error ("parseName error: name " ++ show s ++
                      " must begin with a letter")
  where
    isNameChar c = isAlpha c || isDigit c || elem c "-."

sentinel :: XML
sentinel
  = Element "" [] []

addText :: String -> Stack -> Stack -- 1/1
-- Pre: There is at least one Element on the stack
addText text stack -- decompose into (s : ss)
  = addChild (Text text) (head stack) : tail stack

popAndAdd :: Stack -> Stack -- 1/1
-- Pre: There are at least two Elements on the stack
popAndAdd (e1 : e2 : stack)
  = addChild e1 e2 : stack
popAndAdd _
  = error "Case not expected"

parseAttributes :: String -> (Attributes, String) -- 3/3
-- Pre: The XML attributes string is well-formed
parseAttributes s
  | x == '>'  = ([], xs)
  | otherwise = ((n, attr) : as, s')
  where
    str@(x : xs)  = skipSpace s
    (n, ss)       = parseName str
    (_, text)     = break (== '\"') ss
    (attr, text') = break (== '\"') (tail text)
    (as, s')      = parseAttributes (tail text')

parse :: String -> XML
-- Pre: The XML string is well-formed
parse s
  = parse' (skipSpace s) [sentinel]

parse' :: String -> Stack -> XML -- 7/7
parse' "" ((Element _ _ children) : stack) -- OK
  = head children
parse' ('<' : '/' : str) stack -- OK
  = parse' (tail text) (popAndAdd stack)
  where
    (_, text) = break (=='>') str
parse' ('<' : str) stack -- OK
  = parse' text stack'
  where
    (name, attStr) = parseName str
    (atts, text) = parseAttributes attStr
    stack' = Element name atts [] : stack
parse' str stack -- OK
  = parse' str' stack'
  where
    (text, str') = break (=='<') str
    stack' = addText text stack

-------------------------------------------------------------------------
-- Part III. 2/6

type Context = XML

type XSL = XML

-- Parses XSL and XML source documents and transforms the latter using the
-- former. The output is written to the given file (String).
-- Example use:
--   output "out.html" filmsXSL films
-- To render output.html in a browser, type this at the Linux prompt:
--   firefox output.html &
output :: String -> XML -> XML -> IO()
output file xsl source
  = writeFile file (showXMLs (expandXSL xsl source))

expandXSL :: XSL -> XML -> [XML]
expandXSL xsl source
  = expandXSL' root xsl
  where
    root = Element "/" [] [source]

expandXSL' :: Context -> XSL -> [XML]
expandXSL' root xsl@(Element "value-of" atts children)
  = followPath root path : concatMap (expandXSL' root) children

  where
    path = getAttribute "select" xsl
expandXSL' root xsl@(Element "for-each" atts children)
  = followPath' root path ++ concatMap (expandXSL' root) children
  -- = concat [expandXSL' e c | c <- children, e <- elements] -- this is right
  where
    path = getAttribute "select" xsl
    elements = followPath' root path

expandXSL' root e@(Element _ _ children)
  = concatMap (expandXSL' e) children -- Should be [Element n att (concatMap (expandXSL' root) xs)]
expandXSL' _ _ -- Incorrect
  = []
-- expandXSL' _ (Text t) = [Text t]

-- For value-of.
-- Marking note: you can condense your code.
followPath :: Context -> String -> XML
followPath root path
  | null path' = getValue nextRoot -- should be getValue root
  | otherwise  = followPath nextRoot (tail path')
  where
    (p, path') = break (== '/') path
    (p1 : ps) = p
    nextRoot :: Context
    nextRoot
      | p == "."  = root
      | p1 == '@' = Text (getAttribute ps root)
      | otherwise = getChild p root

{- 

followPath root ""
  = getValue root
followPath root path
  | c == '.' = followPath root path'
  | c == '@' = Text (getAttribute cs root)
  | otherwise = followPath (getChild root current) path'
  where
    (current, rest) = break (== '/') path
    (c : cs) = current
    path' = tail path

-}

-- For for-each. Again, you can simplify this.
followPath' :: Context -> String -> [XML]
followPath' root path
  | null path' = map getValue nextRoots
  | otherwise  = concatMap (`followPath'` tail path') nextRoots
  where
    (p, path') = break (== '/') path
    (p1 : ps) = p
    nextRoots :: [Context]
    nextRoots
      | p == "."  = [root]
      | p1 == '@' = [Text (getAttribute ps c) | c <- nextRoots]
      | otherwise = getChildren p root

-- Try to do this with just one additional function: getContext!

{- 

expandXSL' :: Context -> XSL -> [XML]

expandXSL' _ (Text text)
  = [Text text]

expandXSL' root xsl@(Element "value-of" _ _)
  | null nextRoot = []
  | otherwise     = [getValue (head nextRoots)]
  where
    path = getAttribute "select" xsl
    nextRoots = followPath (path ++ "/") root

expandXSL' root xsl@(Element "for-each" _ xsls)
  = concat [expandXSL' nr x | nr <- nextRoots, x <- xsls]
  where
    path = getAttribute "select" xsl
    nextRoots = followPath (path ++ "/") root

expandXSL' context (Element name atts xsls)
  = [Element name atts (concatMap (expandXSL' context) xsls)]


getContext :: String -> XML -> [XML]
getContext path xml@(Element name _ children)
  | null currentPath   = [xml]
  | p == '@'           = [Text (getAttribute (ps) xml)]
  | currentPath == "." = getContext path' xml
  | otherwise          = concatMap (getContext path') (getChildren currentPath xml)
  where
    (currentPath, path') = break (== '/') path
    (p : ps) = currentPath

getContext _ _
  = []

-}

-------------------------------------------------------------------------
-- Test data for Parts I and II

-- Simple test cases (no whitespace)
s1, s2, s3 :: String
s1
  = "<a>A</a>"
s2
  = "<a x=\"1\"><b>A</b><b>B</b></a>"
s3
  = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>"

-- Parsed versions of the above
x1, x2, x3 :: XML
x1
  = Element "a" [] [Text "A"]
x2
  = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3
  = Element "a"
            []
            [Element "b"
                     []
                     [Element "c"
                              [("att","att1")]
                              [Text "text1"],
                      Element "c"
                              [("att","att2")]
                              [Text "text2"]],
             Element "b"
                     []
                     [Element "c"
                              [("att","att3")]
                              [Text "text3"],
                      Element "d"
                              []
                              [Text "text4"]]]

casablanca :: String
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML
casablancaParsed
  = Element "film"
            [("title","Casablanca")]
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]

-- Films mark-up of Figure 1
films :: String
films
  = "<filmlist>\n\
    \  <film title = \"Rear Window\">\n\
    \    <director>Alfred Hitchcock</director>\n\
    \    <composer>Franz Waxman</composer>\n\
    \    <year>1954</year>\n\
    \  </film>\n\
    \  <film   title =  \"2001: A Space Odyssey\">\n\
    \    <director>Stanley Kubrick</director>\n\
    \    <composer>Richard Strauss</composer>\n\
    \    <composer>Gyorgy Ligeti</composer>\n\
    \    <composer>Johann Strauss</composer>\n\
    \    <year>1968</year>\n\
    \  </film>\n\
    \  <film title=\"Lawrence of Arabia\"  >\n\
    \    <duration>228</duration>\n\
    \    <director>David Lean</director>\n\
    \    <composer>Maurice Jarre</composer>\n\
    \  </film>\n\
    \</filmlist>\n\n\n"

-- Parsed version of films ('parse films'), suitably formatted
filmsParsed :: XML
filmsParsed
  = Element "filmlist"
            []
            [Text "\n  ",
             Element "film" [("title","Rear Window")]
                            [Text "\n    ",
                             Element "director" [] [Text "Alfred Hitchcock"],
                             Text "\n    ",
                             Element "composer" [] [Text "Franz Waxman"],
                             Text "\n    ",
                             Element "year" [] [Text "1954"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","2001: A Space Odyssey")]
                            [Text "\n    ",
                             Element "director" [] [Text "Stanley Kubrick"],
                             Text "\n    ",
                             Element "composer" [] [Text "Richard Strauss"],
                             Text "\n    ",
                             Element "composer" [] [Text "Gyorgy Ligeti"],
                             Text "\n    ",
                             Element "composer" [] [Text "Johann Strauss"],
                             Text "\n    ",
                             Element "year" [] [Text "1968"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","Lawrence of Arabia")]
                            [Text "\n    ",
                             Element "duration" [] [Text "228"],
                             Text "\n    ",
                             Element "director" [] [Text "David Lean"],
                             Text "\n    ",
                             Element "composer" [] [Text "Maurice Jarre"],
                             Text "\n  "],
             Text "\n"]

-------------------------------------------------------------------------
-- XSL tests

-- value-of test cases
xsl1, xsl2, xsl3, xsl4, xsl5, xsl6, xsl7,
  xsl8, xsl9 :: String
xsl1
  = "<value-of select = \"a/b/c\"></value-of>"
xsl2
  = "<value-of select = \"a/b\"></value-of>"
xsl3
  = "<value-of select = \"a/b/d\"></value-of>"
xsl4
  = "<value-of select = \"a/b/c/@att\"></value-of>"
xsl5
  = "<value-of select = \"./a/./b/c/./.\"></value-of>"
xsl6
  = "<t1><t2>Preamble</t2><t3><value-of select = \"a/b/c\"></value-of></t3></t1>"

-- for-each test cases
xsl7
  = "<for-each select=\"a/b/c\"><value-of select=\"./@att\"></value-of>\
    \</for-each>"
xsl8
  = "<for-each select=\"a/b\"><t><value-of select=\"c\"></value-of></t>\
    \</for-each>"
xsl9
  = "<for-each select=\"a/b\"><t1><value-of select=\"absent\"></value-of>\
    \</t1></for-each>"

-- Parsed versions of the above
xsl1Parsed, xsl2Parsed, xsl3Parsed, xsl4Parsed, xsl5Parsed,
  xsl6Parsed, xsl7Parsed, xsl8Parsed, xsl9Parsed :: XML
xsl1Parsed
  = Element "value-of" [("select","a/b/c")] []
xsl2Parsed
  = Element "value-of" [("select","a/b")] []
xsl3Parsed
  = Element "value-of" [("select","a/b/d")] []
xsl4Parsed
  = Element "value-of" [("select","a/b/c/@att")] []
xsl5Parsed
  = Element "value-of" [("select","./a/./b/c/./.")] []
xsl6Parsed
  = Element "t1"
            []
            [Element "t2" [] [Text "Preamble"],
             Element "t3" [] [Element "value-of" [("select","a/b/c")] []]]

xsl7Parsed
  = Element "for-each"
            [("select","a/b/c")]
            [Element "value-of" [("select","./@att")] []]
xsl8Parsed
  = Element "for-each"
            [("select","a/b")]
            [Element "t" [] [Element "value-of" [("select","c")] []]]
xsl9Parsed
  = Element "for-each"
            [("select","a/b")]
            [Element "t1" [] [Element "value-of" [("select","absent")] []]]

-- XSL template for building a films summary (example from spec.)
filmsXSL :: String
filmsXSL
  = "<html>\n\
    \<body>\n\
    \  <h2>Film List</h2>\n\
    \  <table border=\"1\">\n\
    \    <tr>\n\
    \      <th align=\"left\">Title</th>\n\
    \      <th align=\"left\">Director</th>\n\
    \      <th align=\"left\">Principal composer</th>\n\
    \    </tr>\n\
    \    <for-each select=\"filmlist/film\">\n\
    \      <tr>\n\
    \        <td><value-of select=\"@title\"></value-of></td>\n\
    \        <td><value-of select=\"director\"></value-of></td>\n\
    \        <td><value-of select=\"composer\"></value-of></td>\n\
    \      </tr>\n\
    \    </for-each>\n\
    \  </table>\n\
    \</body>\n\
    \</html>"

-- XSL template for building a list of composers (example from spec.)
composersXSL :: String
composersXSL
  = "<for-each select=\"filmlist/film\">\
      \<h2><value-of select=\"@title\"></value-of> composers</h2>\
      \<ul>\
      \<for-each select=\"composer\">\
        \<li><value-of select=\".\"></value-of></li>\
      \</for-each>\
      \</ul>\
    \</for-each>"
