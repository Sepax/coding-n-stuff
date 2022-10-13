{- |
Module      : Simplify
Description : Skeleton for Lab 4: simplifying polynomials.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Simplify where

import Poly
import Test.QuickCheck

-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * A1
-- Define a data type 'Expr' which represents three kinds of expression:
-- binary operators (use 'BinOp' as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable,
-- x, your data type should *not* use 'String' or 'Char' anywhere, since this is
-- not needed.

data Expr = Num Int | Op BinOp Expr Expr | Pow Int

-- x^2 + 1 -- Op AddOp (Pow 2) (Num 1)

type Name = String

--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
prop_Expr :: Expr -> Bool
prop_Expr expr = case expr of
  Pow n -> n >= 0
  _     -> True



e1,e2,e3,e4,e5,e6,e7,e8 :: Expr
e1 = Num 5
e2 = Op MulOp (Num 2) (Num 3)
e3 = Op MulOp (Num 2) (Num (-3))
e4 = Op AddOp (Num 2) (Num 3)
e5 = Op AddOp (Num 2) (Num (-3))
e6 = Pow 3
e7 = Pow (-3) -- This fails the invariant
e8 = Op MulOp (Pow 3) (Op AddOp (Pow 2) (Pow 1))




--------------------------------------------------------------------------------
-- * A3
-- Make 'Expr' an instance of 'Show' (along the lines of the example in the 
-- lecture). You can use Haskell notation for powers: x^2. You should show x^1 
-- as just x. 

{- instance Show Expr where
  show = showExpr
    where
      showExpr :: Expr -> String
      showExpr expr = case expr of
        Num n               -> if n < 0 then "(" ++ show n ++ ")" else show n
        Op AddOp expr expr' -> showExpr expr ++ " + " ++ showExpr expr' 
        Op MulOp expr expr' -> showFactor expr ++ " * " ++ showFactor expr' 
        Pow n               -> if n == 1 then "x" else "x^" ++ show n
        where
          showFactor e@(Op AddOp x y) = "(" ++ showExpr e ++ ")"
          showFactor e           = showExpr e -}
 
instance Show Expr where
  show = showExpr
    where
      showExpr :: Expr -> String
      showExpr expr = case expr of
        Num n                 -> if n < 0 then "(" ++ show n ++ ")" else show n
        Op AddOp expr expr'   -> showExpr expr ++ " + " ++ showExpr expr' 
        Op MulOp expr expr'   -> showFactor expr ++ " * " ++ showFactor expr' 
        Pow n                 -> if n == 1 then "x" else "x^" ++ show n
        where
          showFactor e@(Op AddOp x y) = "(" ++ showExpr e ++ ")"
          showFactor e           = showExpr e  


--------------------------------------------------------------------------------
-- * A4
-- Make 'Expr' and instance of 'Arbitrary'.
-- Now you can check the data type invariant that you defined in A2 using
-- QuickCheck.

-- (Optional)
-- Add a definition of function @shrink :: Expr -> [Expr]@ to 'Arbitrary'
-- which gives hints to QuickCheck on possible smaller expressions that it
-- could use to find a smaller counterexample for failing tests.

instance Arbitrary Expr
  where 
    arbitrary = sized genExpr
     
genExpr :: Int -> Gen Expr
genExpr size = frequency [(1, genNum), (size, genOp), (size, genPow)]
  where
    genNum :: Gen Expr
    genNum = do
      n <- choose (0, 5)
      return (Num n)

    genOp :: Gen Expr
    genOp = let n = size `div` 2 in do
      o <- elements [AddOp, MulOp]
      x <- genExpr n
      y <- genExpr n
      return (Op o x y)

    genPow :: Gen Expr
    genPow = let n = size `div` 2 in do
      ex <- genExpr n
      n <- choose (1, 5)
      return(Pow n)


--------------------------------------------------------------------------------
-- * A5
-- Define the @eval@ function which takes a value for x and an expression and
-- evaluates it.

eval :: Int -> Expr -> Int
eval v expr = case expr of
  Num n         -> n
  Op AddOp e e' -> eval v e + eval v e'
  Op MulOp e e' -> eval v e * eval v e'
  Pow n         -> v^n


--------------------------------------------------------------------------------
-- * A6
-- Define @exprToPoly@ that converts an expression into a polynomial.
-- Here it is important to think recursively to just solve the bigger problem
-- by solving the smaller problems and combining them in the right way. 

exprToPoly :: Expr -> Poly
exprToPoly expr = case expr of
  (Num n) -> fromList [n]
  (Pow n) -> fromList (1:replicate n 0)
  (Op AddOp e e') -> exprToPoly e + exprToPoly e'
  (Op MulOp e e') -> exprToPoly e * exprToPoly e'


-- Define (and check) @prop_exprToPoly@, which checks that evaluating the
-- polynomial you get from @exprToPoly@ gives the same answer as evaluating
-- the expression.
prop_exprToPoly :: Int -> Expr -> Bool
prop_exprToPoly n e = eval n e == evalPoly n (exprToPoly e) 


--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction.

-- Example Poly
p1 = fromList [1,0,0]


polyToExpr :: Poly -> Expr
polyToExpr p
  | p == 0 = Num 0
  | x == 0 && (x:xs) /= [] = polyToExpr rest
  | x == 1 && (x:xs) /= [] = Op AddOp pwr (polyToExpr rest)
  | (x:xs) /= [] = Op AddOp (Op MulOp (Num x) pwr) (polyToExpr rest)
  | otherwise = Op MulOp (Num x) pwr
    where l@(x:xs) = toList p
          rest = fromList xs
          pwr = Pow (length l-1)


polyToExpr' :: Poly -> Expr
polyToExpr' p
  | p == 0 = Num 0
  | x == 0 = rest
  | x == 1 = Op AddOp pwr rest
  | otherwise = case xs of
      [] -> Num x
      _  -> Op AddOp (Op MulOp (Num x) pwr) rest
  where l@(x:xs) = toList p
        rest = polyToExpr' (fromList xs)
        pwr = Pow (length l-1)
 


-- Write (and check) a quickCheck property for this function similar to
-- question 6. 
prop_polyToExpr :: Int -> Poly -> Bool
prop_polyToExpr n p = eval n (polyToExpr p) == evalPoly n p

--------------------------------------------------------------------------------
-- * A8
-- Write a function @simplify@ which simplifies an expression by converting it 
-- to a polynomial and back again.

simplify :: Expr -> Expr
simplify = polyToExpr . exprToPoly

--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property that checks that a simplified expression does not 
-- contain any "junk", where junk is defined to be multiplication by one or 
-- zero, addition of zero, addition or multiplication of numbers, or x to the
-- power of zero. (You may need to fix A7)

junkExpr :: Expr
junkExpr = Op AddOp (Num 1) (Num 0)

nojunkExpr :: Expr
nojunkExpr = Num 1

prop_noJunk :: Expr -> Bool
prop_noJunk 

--------------------------------------------------------------------------------
-- * A10
-- Write two IO functions that read respectively write the difficulty, which is
-- modelled as a natural number. Use the 'diffFile' as file path. Note that the
-- difficulty should never be below zero.

type Difficulty = Int

diffFile :: FilePath
diffFile = "difficulty.txt"

readDifficulty :: IO Difficulty
readDifficulty = undefined

writeDifficulty :: Difficulty -> IO ()
writeDifficulty = undefined

--------------------------------------------------------------------------------
-- * A11
-- Define the 'play' function that generates a random expression, a random 
-- value for @x@, show the simplified expression and ask the user to solve it. 
-- If the guess is as expected, give a nice feedback message and increase the 
-- difficulty by one. If the guess was wrong, again give feedback and decrease 
-- the difficulty by one. Then play again.

play :: IO ()
play = undefined

--------------------------------------------------------------------------------
