-- Exercises week 6!

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
           deriving (Show, Eq)

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