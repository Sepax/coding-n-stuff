-- Exercises week 6!

import Test.QuickCheck
import Data.List (union)
import Data.Maybe (fromJust)

-- | 0(*). Expression and Integer Trees

data Expr = Lit Int 
          | Op Ops Expr Expr

data Ops = Add | Sub | Mul | Div

--------------------------------------------------------------
e1 :: Expr
e1 = Op Add (Op Sub (Lit 3) (Lit 1)) (Lit 3)
--------------------------------------------------------------

eval :: Expr -> Int
eval (Lit n)     = n
eval (Op Add e1 e2) = eval e1 + eval e2
eval (Op Sub e1 e2) = eval e1 - eval e2
eval (Op Mul e1 e2) = eval e1 * eval e2
eval (Op Div e1 e2) = eval e1 `div`eval e2

eval' :: Expr -> Maybe Int
eval' (Lit n) = Just n
eval' (Op Add e1 e2) = eval' e1 `add` eval' e2
 where
    Nothing `add` _       = Nothing
    _       `add` Nothing = Nothing
    Just x  `add` Just y  = Just (x+y)
eval' (Op Sub e1 e2) = eval' e1 `sub` eval' e2
 where
    Nothing `sub` _       = Nothing
    _       `sub` Nothing = Nothing
    Just x  `sub` Just y  = Just (x-y)
eval' (Op Mul e1 e2) = eval' e1 `mul` eval' e2
 where
    Nothing `mul` _       = Nothing
    _       `mul` Nothing = Nothing
    Just x  `mul` Just y  = Just (x*y)
eval' (Op Div e1 e2) = eval' e1 `dvv` eval' e2
 where
    Nothing `dvv` _       = Nothing
    _       `dvv` Nothing = Nothing
    Just x  `dvv` Just 0  = Nothing
    Just x  `dvv` Just y  = Just (x `div` y)
    


showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Op Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Op Sub e1 e2) = showExpr e1 ++ "-" ++ showExpr e2

-- A Give calculations of
-- eval (Lit 67)
-- ANSWER: 67
-- eval (Add (Sub (Lit 3) (Lit 1)) (Lit 3))
-- ANSWER: 5
-- showExpr (Add (Lit 67) (Lit (-34)))
-- ANSWER: "67 + - 34"

-- B(*). Define the function
size :: Expr -> Int
size (Lit n)    = 0
size (Op Add e1 e2) = 1 + size e1 + size e2
size (Op Sub e1 e2) = 1 + size e1 + size e2

-- C(*). Add the operations of multiplication and integer division to the type Expr,
-- and redefine the functions eval, showExpr, and size to include the new cases.
-- Also, write one version of eval with the result type Maybe Int.

-- D. Instead of adding extra constructors to the Expr datatype as in C it is
--    possible to factor the definitions
-- ANSWER: If I want to add another extra operation @Mod@, I'll simply just add
--         it to the datatype @Ops@.

-- E. In Haskell back-quptes allows us to use constructors in infix (indeed any function)
-- like in (Lit 3) `Add` (Lit 15). However, if this expression is shown (using deriving show)
-- it appears in prefix form as Add (Lit 3) (Lit 15)

-- It is also possible to use infix operators for constructor names where the first character
-- must be a ':'. We can, e.g., redefine @Expr@ as
{- data Expr = Lit Int
             | Expr :+: Expr
             | Expr :-: Expr        -}

-- Redefine the above functions using this datatype with infix constructors.  
-- ANSWER: ... No, I don't pallar to do that.

-- | 1(*). Integer Trees

data NTree = NilT
           | Node Int NTree NTree
           deriving (Show, Eq, Ord)

instance Arbitrary NTree where
    arbitrary = sized genNTree

genNTree :: Int -> Gen NTree
genNTree a = do
    n <- choose (1,10)
    l <- frequency [(1,genNilT), (a,genNTree (a `div` 2))]
    r <- frequency [(1,genNilT), (a,genNTree (a `div` 2))]
    return (Node n l r)

genNilT :: Gen NTree
genNilT = do
    return NilT


ex1 = 
 Node 20 (Node 10 (Node 5 NilT NilT) (Node 15 NilT NilT)) (Node 40 NilT NilT)

sumTree :: NTree -> Int
sumTree NilT           = 0
sumTree (Node n t1 t2) = n + sumTree t1 + sumTree t2

depth :: NTree -> Int
depth NilT = 0
depth (Node _ t1 t2) = 1 + max (depth t1) (depth t2)

-- A. Give a calculation of
-- sumTree (Node 3) (Node 4 NilT NilT) NilT)
-- ANSWER: 3 + 4 = 7
-- depth (Node 3 (Node 4 NilT NilT) NilT)
-- ANSWER: 2

-- B(*). Define dunctions to return the left- and right-hand sub-trees of an NTree.
leftSub :: NTree -> NTree
leftSub NilT = NilT
leftSub (Node n l r) = l

rightSub :: NTree -> NTree
rightSub NilT = NilT
rightSub (Node n l r) = r

-- Alternative that covers both cases with an extra parameter
leftOrRightSub :: String -> NTree -> NTree
leftOrRightSub _ NilT = NilT
leftOrRightSub (d:xs) (Node n l r)
    | d == 'l' = l
    | d == 'r' = r

-- C(*). Define a function to decide whether a number is an element of an NTree.
inTree :: Int -> NTree -> Bool
inTree _ NilT = False
inTree e (Node n l r)
    | e == n = True
    | otherwise = inTree e l || inTree n r

-- D. Define functions to find the maximum and minimum values held in an NTree.
maxTree :: NTree -> Maybe Int
maxTree NilT = Nothing
maxTree t@(Node n l r) = max (Just n) (max (maxTree l) (maxTree r))

minTree :: NTree -> Maybe Int
minTree NilT = Nothing
minTree (Node n NilT NilT) = Just n
minTree t@(Node n l r) = min (Just n) (min (minTree l) (minTree r))

-- E(*). A tree is reflected by swapping left and right sub-trees, recursively.
-- Define a function to reflect an NTree. What is the result of reflecting twice?
-- Write a QuickCheck property for that!
-- (In order to be able to test your properties, 
-- you have to make NTree an instance of Arbitrary.)

reflectTree :: NTree -> NTree
reflectTree NilT = NilT
reflectTree (Node n l r) = Node n (reflectTree r) (reflectTree l)

prop_reflect :: NTree -> Bool
prop_reflect t = reflectTree (reflectTree t) == t

-- F. Define functions
collapse, sort :: NTree -> [Int]
collapse NilT = []
collapse (Node n l r) = collapse l ++ [n] ++ collapse r

sort = qsort . collapse
 where
    qsort :: Ord a => [a] -> [a]
    qsort [] = []
    qsort (x:xs) =
        let smaller = qsort [a | a <- xs, a <= x]
            larger  = qsort [a | a <- xs, a > x]
        in smaller ++ [x] ++ larger

prop_collapse :: NTree -> Bool
prop_collapse t = collapse (reflectTree t) == reverse (collapse t)

prop_sort :: NTree -> Bool
prop_sort t = and $ zipWith (<=) (sort t) (tail (sort t))

-- | 2(*). File Systems
{- A file either contains data or is a directory. A directory contains other files
(which may themselves be directories) along with a name for each one. -}

-- A. 
{- Design a data type to represent the contents of a directory. Ignore the contents
of files: your are just trying to represent file names and the way they are
organised into directories here. -}

data File = File String 
          | Dir String [File]
          deriving (Eq, Show)

-- B. 
{- Define a function to search for a given file name in a directory. You should
return a path leading to a file with the given name. Thus if your directory contains
a, b, and c, and b is a directory containing x and y, then searching for x should
produce b/x -}

type FileSystem = [File]

exampleFilSystem =
    [ File "apa"
    , Dir "bepa" [ File "apa", Dir "beba" [], Dir "cepa" [ File "bepa" ]]
    , Dir "cepa" [ Dir "bepa" [], Dir "cepa" [ File "apa" ] ] 
    ]

search :: FileSystem -> String -> [String]
search files name = 
    [ name 
    | File name' <- files
    , name == name']
     ++ 
    [ dir ++ "/" ++ path 
    | Dir dir files' <- files
    , path <- search files' name]

-- This was tough and I looked at the solutions...
-- I didn't know we were supposed to search in a FileSystem :: [File]...?

-- | 3. Exercises on Propositional Logic
{- A proposotion is a boolean formula of one of the following forms:
• a variable name (a string)
• p & q (and)
• p | q (or)
• ~p    (not)
where p and q are propositions. For example, p | ~p is a proposition.
-}

-- A. Design a data type Prop to represent propositions.

data Prop = Var Name 
          | And Prop Prop 
          | Or Prop Prop
          | Not Prop
          deriving (Eq, Show)

type Name = String

exProp = And (Or (Var "p") (Var "q")) (Not (Var "p"))


-- B. Define a function vars... 
 
vars :: Prop -> [Name]
vars (Var x)    = [x]
vars (And p1 p2) = vars p1 `union` vars p2
vars (Or p1 p2)  = vars p1 `union` vars p2
vars (Not p)     = vars p

truthValue :: Prop -> [(String,Bool)] -> Bool
truthValue (Var x) pvs     = fromJust (lookup x pvs)
truthValue (And p1 p2) pvs = truthValue p1 pvs && truthValue p2 pvs
truthValue (Or p1 p2) pvs  = truthValue p1 pvs || truthValue p2 pvs
truthValue (Not p) pvs     = not (truthValue p pvs)

exEnv = [("p", True), ("q",True)]

-- C. Define a function...

tautology :: Prop -> Bool
tautology p = and [truthValue p val | val <- allVals (vars p)]

allVals :: [Name] -> [[(Name, Bool)]]
allVals [] = [[]]
allVals (x:xs) = [ (x,b):val |val <- allVals xs, b <- [False,True]]

hamlet :: Prop
hamlet = Or (Var "to be")  (Not (Var "to be"))

-- | 4. Approximating 0-solutions of functions.

-- Define a function solve0
solve0 :: (Double -> Double) -> (Double,Double) -> Double
solve0 f (a,b)
    | f x =? x           = x
    | a == b             = error "no solution found!"
    | f a > 0 || f b < 0 = error "interval does not contain 0!"
    | f x < 0            = solve0 f (a,x)
    | f x > 0            = solve0 f (x,b)
 where
    x   = (a + b) / 2
    x =? y = abs (x-y) < 0.0000000001
    
-- This is not working... 
