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

data Expr = Num Int | Op BinOp Expr Expr | Pwr Int | Empty deriving Eq

--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative

prop_Expr :: Expr -> Bool
prop_Expr expr = case expr of
  Pwr n -> n >= 0
  _     -> True

--------------------------------------------------------------------------------
-- * A3
-- Make 'Expr' an instance of 'Show' (along the lines of the example in the 
-- lecture). You can use Haskell notation for Pwrers: x^2. You should show x^1 
-- as just x. 
instance Show Expr where
  show = showExpr
    where
      showExpr :: Expr -> String
      showExpr expr = case expr of
        Num n                 -> if n < 0 then "(" ++ show n ++ ")" else show n
        Op AddOp expr Empty   -> showExpr expr
        Op AddOp expr expr'   -> showExpr expr ++ " + " ++ showExpr expr' 
        Op MulOp expr expr'   -> showFactor expr ++ " * " ++ showFactor expr' 
        Pwr n                 -> if n == 1 then "x" else "x^" ++ show n
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
genExpr size = frequency [(1, genNum), (size, genOp), (size, genPwr)]
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

    genPwr :: Gen Expr
    genPwr = let n = size `div` 2 in do
      ex <- genExpr n
      n <- choose (0, 5)
      return(Pwr n)

--------------------------------------------------------------------------------
-- * A5
-- Define the @eval@ function which takes a value for x and an expression and
-- evaluates it.

eval :: Int -> Expr -> Int
eval v expr = case expr of
  Num n         -> n
  Op AddOp e e' -> eval v e + eval v e'
  Op MulOp e e' -> eval v e * eval v e'
  Pwr n         -> v^n
  Empty         -> 0

--------------------------------------------------------------------------------
-- * A6
-- Define @exprToPoly@ that converts an expression into a polynomial.
-- Here it is important to think recursively to just solve the bigger problem
-- by solving the smaller problems and combining them in the right way. 

exprToPoly :: Expr -> Poly
exprToPoly expr = case expr of
  Empty   -> fromList []
  (Num n) -> fromList [n]
  (Pwr n) -> fromList (1:replicate n 0)
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

polyToExpr :: Poly -> Expr
polyToExpr = listToExpr . toList 
  where
    listToExpr :: [Int] -> Expr
    listToExpr [a]
      | a == 0 = Empty
      | otherwise = Num a     
    listToExpr l@(x:xs)
      | x == 0    = listToExpr xs
      | x == 1    = Op AddOp (Pwr (length l-1)) (listToExpr xs)
      | otherwise = Op AddOp (Op MulOp (Num x) (Pwr (length l-1))) (listToExpr xs)

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
-- Pwrer of zero. (You may need to fix A7)

prop_noJunk :: Expr -> Bool
prop_noJunk expr = case expr of
  Op MulOp expr expr' -> Op MulOp expr expr' `notElem` [expr, expr']
  Num 0               -> False
  Pwr 0               -> False
  _                   -> True

--------------------------------------------------------------------------------
-- * A10
-- Write two IO functions that read respectively write the difficulty, which is
-- modelled as a natural number. Use the 'diffFile' as file path. Note that the
-- difficulty should never be below zero.

type Difficulty = Int

diffFile :: FilePath
diffFile = "difficulty.txt"

readDifficulty :: IO Difficulty
readDifficulty = do 
  txt <- readFile diffFile
  return (read txt)
  
writeDifficulty :: Difficulty -> IO ()
writeDifficulty diff = do
  if diff <= 0 then writeFile diffFile "1"
  else writeFile diffFile (show diff)

--------------------------------------------------------------------------------
-- * A11
-- Define the 'play' function that generates a random expression, a random 
-- value for @x@, show the simplified expression and ask the user to solve it. 
-- If the guess is as expected, give a nice feedback message and increase the 
-- difficulty by one. If the guess was wrong, again give feedback and decrease 
-- the difficulty by one. Then play again.

play :: IO ()
play = do
    putStrLn "Welcome to game"
    quiz

quiz :: IO ()
quiz = do
  d    <- readDifficulty
  expr' <- generate (genExpr d)
  let expr = simplify expr'
  e    <- generate (choose (1,10))
  putStr $ "Solve this with x = " ++ show e ++ "\n"++ show expr ++ "\n\n> "
  n    <- readLn
  
  if eval e expr == n
    then quizWin
    else quizFail  
  return ()
  where
    quizFail = do
      putStrLn "Oh no, nice try!"
      d    <- readDifficulty
      writeDifficulty (subtract 1 d)
      quiz
    quizWin = do
      putStrLn "Good job! Try a harder one!"
      d    <- readDifficulty
      writeDifficulty (d+1)
      quiz




 
--------------------------------------------------------------------------------
