-- Exercises week 3

import Data.List (nub)
import Test.QuickCheck
import Test.QuickCheck.Property (failed, reason, succeeded)
--import Data.Maybe
import Prelude hiding (drop, splitAt, take, zip3)

-- | 0. Defining Functions over Lists
-- A.
take :: Int -> [a] -> [a]
take _ [] = []
take n _ | n <= 0 = []
take n xs | n > length xs = xs
take n (x : xs) = x : take (n -1) xs

drop :: Int -> [a] -> [a]
drop _ [] = []
drop n xs | n <= 0 = xs
drop n (x : xs) = drop (n -1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

-- B. How would you define a function zip3 which zips together three lists?

-- Recursive version
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 (x : xs) (y : ys) (z : zs) = (x, y, z) : zip3 xs ys zs
zip3 _ _ _ = []

-- Using zip
zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' xs ys zs = [(x, y, z) | ((x, y), z) <- zip (zip xs ys) zs] -- Interesting!

-- Seems like zip3' ist faster when testing on large lists. Why, IDK.

-- | 1. Permutations
-- isPermutation xs ys checks whether xs is a permutation of ys
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] (y : ys) = False
isPermutation (x : xs) ys = x `elem` ys && isPermutation xs (removeElemOnce x ys)
  where
    removeElemOnce :: Eq a => a -> [a] -> [a]
    removeElemOnce x [] = []
    removeElemOnce x (y : ys)
      | x == y = ys
      | otherwise = y : removeElemOnce x ys

-- | 2. Sorting
sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x : y : xs) = x <= y && sorted (y : xs)

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert' x ys

prop_insert :: Integer -> [Integer] -> Property
prop_insert x xs = sorted xs ==> sorted (insert' x xs)

-- Now use insert' to define...
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert' x (isort xs)

{-
Evaluating isort [3,2,4]:
isort [3:2:4:[]]
insert' 3 (isort (2:4:[]))
insert' 3 (insert' 2 (isort (4:[])))
insert' 3 (insert' 2 (insert' 4 (isort [])))
insert' 3 (insert' 2 (insert' 4 []))
insert' 3 (insert' 2 (4:[]))
insert' 3 (2:4:[])
2: insert' 3 (4:[])
2:3:4:[]
[2,3,4]
-}

-- | 3*. Pascal's Triangle
-- It Works but not the first row...
pascal :: Int -> [Int]
pascal 0 = []
pascal n = [1] ++ sum2 (pascal (n -1)) ++ [1]
  where
    sum2 [] = []
    sum2 [x] = [x]
    sum2 (x : xs) = [sum [x, y] | (x, y) <- zip (x : xs) xs]

{-
Evaluating pascal 3:
pascal 3
[1] ++ sum2 (pascal 2) ++ [1]
[1] ++ sum2 ([1] ++ sum2 (pascal 1) ++ [1]) ++ [1]
[1] ++ sum2 ([1] ++ sum2 ([1] ++ sum2 (pascal 0) ++ [1]) ++ [1]) ++ [1]
[1] ++ sum2 ([1] ++ sum2 ([1] ++ sum2 [] ++ [1]) ++ [1]) ++ [1]
[1] ++ sum2 ([1] ++ sum2 ([1] ++ [] ++ [1]) ++ [1]) ++ [1]
[1] ++ sum2 ([1] ++ sum2 [1,1] ++ [1]) ++ [1]
[1] ++ sum2 ([1] ++ ([sum [x,y] | (x,y) <- zip [1,1] [1]]) ++ [1]) ++ [1]
[1] ++ sum2 ([1] ++ [2] ++ [1]) ++ [1]
[1] ++ sum2 [1,2,1] ++ [1]
[1] ++ ([sum [x,y] | (x,y) <- zip [1,2,1] [2,1]]) ++ [1]
[1] ++ [3,3] ++ [1]
[1,3,3,1]
-}

-- Just for fun!
pascalShow :: [[Int]] -> IO ()
pascalShow [] = return ()
pascalShow (x : xs) = do
  putStrLn (show x)
  pascalShow xs

pascalList n = [pascal x | x <- [1 .. n]]