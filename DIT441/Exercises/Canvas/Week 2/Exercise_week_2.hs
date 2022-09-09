import Text.XHtml (small)
-- Exercises week 2

-- 1. (*) The Maximum Function

maxi :: Ord a => a -> a -> a 
maxi a b 
    | a > b = a
    | a < b = b
    | otherwise = error "Arguments have the same value"

-- 2. Sum of squares
-- sumsq n returns 1*1 + 2*2 + ... + n*n

sumsq :: Int -> Int
sumsq 0 = 0
sumsq n = n*n + sumsq (n-1)


-- 3 (*) The towers of Hanoi


hanoi :: Integer -> Integer
hanoi 0 = 0
hanoi n = 2 * hanoi(n-1) + 1


-- 4. Fibonacci numbers
-- fib n computes the nth Fibonacci numbers
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- fibList computes a list of all Fibonacci numbers
-- up to the nth number.
fibList :: Int -> [Int]
fibList n = [fib x | x <- [0..n]] 

-- I notice that the computiontime increases for
-- each larger value n.


-- More difficult
-- WHAT EVEN IS A PROPERTY???

-- 5. Factors
smallestFactor :: Int -> Int
smallestFactor = undefined 