-- Exercises week 3

import Data.List (nub)
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
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] _ = True
isPermutation (x : xs) (y : ys)
  | x == y = isPermutation xs ys
  | x /= y = isPermutation (x : xs) ys

{-
[1,2,1] [2,1,1]

[]
-}
