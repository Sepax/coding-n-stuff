{- |
Module      : Lab1
Description : Skeleton for lab 1: Power to the People
Copyright   : (c) TDA555/DIT440, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : Sebastian Pålsson, Tim Persson, Gustav Dalemo
Lab group   : 9
-}


-- The power function uses explicit recursion to calculate n^k. We developed
-- this function during a lecture.
power :: Integer -> Integer -> Integer
power n k 
  | k < 0 = error "power: negative argument"
  | k == 0 = 1
power n k = n * power n (k-1)


-- Part A ----------------------------------------------------------------------

-- stepsPower k gives the number of multiplications executed by power n k
stepsPower :: Integer -> Integer
stepsPower k = k 


-- Part B ----------------------------------------------------------------------

-- Uses list comprehention to calculate the power of n^k using the prelude function product
power1 :: Integer -> Integer -> Integer
power1 n k
  | k < 0 = error "power: negative argument"
  | k == 0 = 1
power1 n k = product [n | _ <- [1..k]]


-- Part C ----------------------------------------------------------------------

{- Power2 uses less calculations than power1. When k is even, we take advantage of power laws to divide k in half.
When k is odd, we use the original recursive method.
-}
power2 :: Integer -> Integer -> Integer
power2 n k
  | k < 0 = error "power: negative argument"
  | k == 0 = 1
  | even k = power2 (n*n) (div k 2)
  | odd k = n * (power2 n (k-1))

-- Part D ----------------------------------------------------------------------

comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k = power n k == power1 n k

comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k = power n k == power2 n k

{- 
The reasons to why we choose these particular tests is that we want to test all combinations 
of base and exponent when they are less than zero, equal to zero and greater than zero, except for
when the exponent is < 0, scince that would result in an error. (To check if all functions gives an error
when exponent is < 0, one can check that individually).
That leaves us with the following tests: [(-2,0),(-2,2),(0,0),(0,2),(2,0),(2,2)]
-}

testAll :: IO()
testAll = putStrLn 
  ("Our tests are " ++ show([(x,y) | x <- [(-2),0,2], y <- [0,2]]) ++ "\n\n" ++
  "ComparisionPower1:" ++ "\n" ++ 
  "Test 1: " ++ (show(comparePower1 (-2) 0)) ++ "\n" ++
  "Test 2: " ++ (show(comparePower1 (-2) 2)) ++ "\n" ++
  "Test 3: " ++ (show(comparePower1 0 0)) ++ "\n" ++
  "Test 4: " ++ (show(comparePower1 0 2)) ++ "\n" ++
  "Test 5: " ++ (show(comparePower1 2 0)) ++ "\n" ++
  "Test 6: " ++ (show(comparePower1 2 2)) ++ "\n\n" ++
  "ComparisionPower2:" ++ "\n" ++
  "Test 1: " ++ (show(comparePower2 (-2) 0)) ++ "\n" ++
  "Test 2: " ++ (show(comparePower2 (-2) 2)) ++ "\n" ++
  "Test 3: " ++ (show(comparePower2 0 0)) ++ "\n" ++
  "Test 4: " ++ (show(comparePower2 0 2)) ++ "\n" ++
  "Test 5: " ++ (show(comparePower2 2 0)) ++ "\n" ++
  "Test 6: " ++ (show(comparePower2 2 2)) ++ "\n")