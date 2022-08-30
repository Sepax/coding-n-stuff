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
power1 :: Integer -> Integer -> Integer
power1 n k
  | k < 0 = error "power: negative argument"
  | k == 0 = 1
power1 n k = product [n | _ <- [1..k]]


-- Part C ----------------------------------------------------------------------

power2 :: Integer -> Integer -> Integer
power2 n k
  | k < 0 = error "power: negative argument"
  | k == 0 = 1
  | even k = power2 (n*n) (div k k)
  | odd k = n * (power2 n (k-1))

-- Part D ----------------------------------------------------------------------

{- 
    Comparison 1: power1 a b == power a b
    Comparison 2: power2 a b == power a b

    For each comparison:
      test 1:
          a == 2 
          b == 3
      test 2:
          a == (-2) 
          b == 3
      test 3:
          a == 0
          b == 3
      test 4:
          a == 2
          b == 0
-}

comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k =
  if power1 n k == power n k then True
  else False

comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k =
  if power2 n k == power n k then True
  else False

testAll = putStr ( "ComparisonPower1:\n     Test 1: " ++ (show (comparePower1 2 3))    ++ "\n" ++
                                       "    Test 2: " ++ (show (comparePower1 (-2) 3)) ++ "\n" ++
                                       "    Test 3: " ++ (show (comparePower1 0 3))    ++ "\n" ++
                                       "    Test 4: " ++ (show (comparePower1 2 0))    ++ "\n" ++
                   "ComparisonPower2:\n     Test 1: " ++ (show (comparePower2 2 3))    ++ "\n" ++
                                       "    Test 2: " ++ (show (comparePower2 (-2) 3)) ++ "\n" ++
                                       "    Test 3: " ++ (show (comparePower2 0 3))    ++ "\n" ++
                                       "    Test 4: " ++ (show (comparePower2 2 0))    ++ "\n" )