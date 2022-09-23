-- Exercises week 3

import Data.List (nub,sort)
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
-- My first attempt failed (kind of) so heres the solution...:

pascal' :: Int -> [Int]
pascal' 1 = [1]
pascal' n = [1] ++ [x + y | (x, y) <- pairs (pascal' (n -1))] ++ [1]
  where
    pairs (x : y : xs) = (x, y) : pairs (y : xs)
    pairs _ = []

--Evaluating pascal' 3:
--pascal'3
{- eval2 =
  [ [1] ++ [x + y | (x, y) <- pairs (pascal' 2)] ++ [1],
    [1] ++ [x + y | (x, y) <- pairs ([1] ++ [x + y | (x, y) <- pairs (pascal' 1)] ++ [1])] ++ [1],
    [1] ++ [x + y | (x, y) <- pairs ([1] ++ [x + y | (x, y) <- pairs [1]] ++ [1])] ++ [1],
    [1] ++ [x + y | (x, y) <- pairs ([1] ++ [x + y | (x, y) <- []] ++ [1])] ++ [1],
    [1] ++ [x + y | (x, y) <- pairs ([1] ++ [x + y | (x, y) <- []] ++ [1])] ++ [1],
    [1] ++ [x + y | (x, y) <- pairs ([1] ++ [] ++ [1])] ++ [1],
    [1] ++ [x + y | (x, y) <- pairs [1, 1]] ++ [1],
    [1] ++ [x + y | (x, y) <- (1, 1) : pairs (1 : [])] ++ [1],
    [1] ++ [x + y | (x, y) <- (1, 1) : []] ++ [1],
    [1] ++ [x + y | (x, y) <- [(1, 1)]] ++ [1],
    [1] ++ [2] ++ [1],
    [1, 2, 1]
  ] -}

-- Just for fun!
pascalShow :: [[Int]] -> IO ()
pascalShow [] = return ()
pascalShow (x : xs) = do
  putStrLn (show x)
  pascalShow xs

pascalList n = [pascal' x | x <- [1 .. n]]

-- | 4*. Erastosthenes' sieve
-- Anctient method of finding prime numbers.

-- Version that usess list comprehension.
-- Adds a number to the list if the remainder is not 0, i.e.
-- x is not a multiple of m.
crossOut :: Int -> [Int] -> [Int]
crossOut n xs = [x | x <- xs, mod x n /= 0]

-- Recursive version!
sieve :: [Int] -> [Int]
sieve [] = []
sieve (1:xs) = sieve xs
sieve (x:xs) = x:sieve (crossOut x xs)

{-
sieve [1,2,3,4,5,6,7,8,9,10]
sieve (1:2:3:4:5:6:7:8:9:10:[])
sieve (2:3:4:5:6:7:8:9:10:[])
2:sieve(crossOut 2 (3:4:5:6:7:8:9:10:[]))
2:sieve[x | x <- [3,4,5,6,7,8,9,10], mod x 2 /= 0]
2:sieve(3:5:7:9[])
2:3:sieve(crossOut 3 (5,7,9:[]))
2:3:sieve[x | x <- [5,7,9], mod x 3 /= 0]
2:3:sieve[5,7]
2:3:5:sieve[crossOut 5 (7:[])]
2:3:5:sieve[x | x <- [7], mod x 5 /= 0]
2:3:5:sieve(7:[])
2:3:5:7:sieve(crossOut 7 [])
2:3:5:7:sieve[x | x <- [], mod x 7 /= 0]
2:3:5:7:sieve[]
2:3:5:7:[]
[2,3,5,7]
-}
primes1To100 = sieve [1..100]

prop_Sieve :: Int -> Bool
prop_Sieve n = and [isPrime n | n <- sieve [1..n]]
  where
    isPrime n = factors n == [1,n]
    factors n = [k | k <- [1..n], mod n k == 0]

-- | 5*. Number 

-- This uses the primes1To100, which uses the sieve function to check
-- whether n belongs to that list of primes. If it doesn't, it's not a prime.
isNPrime :: Int -> Bool
isNPrime n = n `elem` primes1To100

-- This creates a list of tuples, where the elements in each tuple are primes and
-- who's sum is n.
-- If the list is empty, there are no pair of primes which sum is n, and therefore
-- n is a prime.
isNSumOfPrimes :: Int -> Bool
isNSumOfPrimes n = not (null ([(a,b) | a <- primes1To100, b <- primes1To100, n == a+b]))

-- This returns counterExamples. It creates a list of each number from 4 to 100,
-- that is even and that isn't a sum of primes (using the isNSumOfPrimes function).
-- If this list were to be anything but empty, we have an example of when the
-- hypothesis is false. But, when running this, we get the empty list, which means
-- the hypothesis is in fact True.  
counterExamples :: [Int]
counterExamples = [n | n <- [4..100], even n, not(isNSumOfPrimes n)]


-- | 6. Occurences in Lists

-- Using the @elem@ function, we can check whether x occurs in xs
occursIn :: Eq a => a -> [a] -> Bool
occursIn _ [] = False
occursIn x xs = x `elem` xs

-- Using recursion to check every element in the first list to check if they are
-- all occuring in the second list. As soon as one element is not occuring,
-- we stop checking and return False.  
allOccurIn :: Eq a => [a] -> [a] -> Bool
allOccurIn [] ys = True
allOccurIn (x:xs) ys
  | x `notElem` ys = False
  | otherwise = allOccurIn xs ys

-- Using the previously defined function @allOccurIn@ we can check if all elements
-- from xs occur in ys and vice versa. If they do, then it means both lists contains
-- exactly the same elements. (possibly in a different order)
sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = allOccurIn xs ys && allOccurIn ys xs

-- Easy recursion method. Just adds 1 when the element matches with each element
-- in the list.
numOccurrences :: Eq a => a -> [a] -> Int
numOccurrences x [] = 0
numOccurrences x (y:ys)
  | x == y = 1 + numOccurrences x ys
  | otherwise = numOccurrences x ys

-- !!!
-- I dind't read the full task... I was not meant to do recursive functions... 
-- Let's do them all again using list comprehension instead:


-- We create a list of all elements from the list that matches with our control.
-- If the list isn't empty, it means that the element did occur in the list!
occursIn' :: Eq a => a -> [a] -> Bool
occursIn' x xs = not (null([x' | x' <- xs, x == x']))

-- Using the predefined function @coccursIn'@ as a guard, we create a list of
-- all elements that occurs in ys and then checks if the length of xs 
-- is the same "before and after".
allOccurIn' :: Eq a => [a] -> [a] -> Bool
allOccurIn' xs ys = length [x | x <- xs, x `occursIn'` ys] == length xs

-- Using the predefined function @allOccurIn'@, we can check both directions
-- of implication (==>, <==). If we have both, then we know that they contain
-- exactly the same elements.
sameElements' :: Eq a => [a] -> [a] -> Bool
sameElements' xs ys = allOccurIn' xs ys && allOccurIn' ys xs

-- Creates a lists of 1's, one for each element in the list that matches 
-- with the given element. Then using @sum@ to get the number of occurences.
numOccurrences' :: Eq a => a -> [a] -> Int
numOccurrences' x ys = sum [1 | x' <- ys, x == x']

-- Alternative version of the on before. Here we just check the length of the
-- list instead.
numOccurrences'' :: Eq a => a -> [a] -> Int
numOccurrences'' x ys = length [x' | x' <- ys, x == x']


-- BAG function
bag :: Eq a => [a] -> [(a,Int)]
bag xs = nub [(e,amt) | e <- xs, amt <- [amt | amt <- [numOccurrences'' e xs]]]

-- This is unneccessary but fun!
displayBag :: Show a => [(a,Int)] -> IO ()
displayBag [] = return ()
displayBag (x:xs) = do
  putStrLn (show x)
  displayBag xs


-- | 7. Elements and Positions

letters = "abcdef"

positions :: [a] -> [(a,Int)]
positions xs = zip xs [0..]

firstPosition :: Eq a => a -> [(a,Int)] -> Int
firstPosition e (x:xs)
  | e `notElem` map fst (x:xs) = error "error: Element is not in list"
  | e == fst x = snd x
  | otherwise = firstPosition e xs

-- We create a list that adds all elements from the given list, except the element
-- that is on the index that it first occurs in the original list. (Tricky this one)
remove1st :: Eq a => a -> [a] -> [a]
remove1st x xs = [e | (e,i) <- positions xs, i /= firstPosition x (positions xs)]

{-
remove1st 'l' "hello"
[e | (e,i) <- positions "hello", i /= firstPosition 'l' (positions "hello")]
[e | (e,i) <- [('h',0),('e',1),('l',2),('l',3),('o',4)], i /= firstPosition 'l' [('h',0),('e',1),('l',2),('l',3),('o',4)]]
[e | (e,i) <- [('h',0),('e',1),('l',2),('l',3),('o',4)], i /= 2]
['h','e','l','o']
"helo"
-}


-- | 8. List Comprehensions
-- Experiment with the function:
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(x,y) | x <- xs, y <- ys]

-- What does it do?
-- It creates every possible pairs that can be made from the elements
-- in the given lists. First, it takes the first element of xs and pairs is with
-- every element in ys. When ys run out of elements, it moves on to the second
-- element of xs and repeat the process. 


-- All pythagorean triads with a<=b<=c<=100
pytTriads :: [(Int,Int,Int)]
pytTriads = [(x,y,z) | x <- [1..100], y <- [1..100], z <- [1..100], x*x + y*y + z*z <= 100]

-- This is just very neat and candy for the eyes! :D
-- Call the function with pytTriads as it's argument!
showPytTriads :: [(Int,Int,Int)] -> IO ()
showPytTriads [] = return ()
showPytTriads ((a,b,c):xs) = do
  putStr (show (a,b,c))
  putStr " sum: "
  putStrLn (show(a*a + b*b + c*c))
  showPytTriads xs


