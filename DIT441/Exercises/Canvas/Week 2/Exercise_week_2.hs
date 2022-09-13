-- Exercises week 2

-- 1. (*) The Maximum Function

maxi :: Ord a => a -> a -> a 
maxi x y 
    | x >= y = x
    | otherwise = y
    
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
fibList n = [fib x | x <- [0..(n-1)]] 

fibAux :: Integer -> Integer -> Integer -> Integer
fibAux 0 a b = a
fibAux i a b | i>0 = fibAux (i-1) b (a+b)
             | otherwise = error "Negative index"

new_fib :: Integer -> Integer
new_fib n = fibAux n 1 1

{-
Evaluating fibAux 5 1 1
fibAux 5 1 1
fibAux 4 1 2
fibAux 3 2 3
fibAux 2 3 5
fibAux 1 5 8
fibAux 0 8 13
8
-}

-- 5. Factors

smallestFactor :: Int -> Int
smallestFactor n = head [x | x <- [2..n], n `mod` x == 0]

-- This is not recursive... but I don't know what else to do...
nextFactor :: Int -> Int -> Int
nextFactor _ 0 = 0
nextFactor 1 _ = error "error: k = 1 not allowed"
nextFactor k n
    | n `mod` k == 0 = k
    | otherwise = nextFactor (k+1) n

smallestFactor' :: Int -> Int -> Int
smallestFactor' = nextFactor

-- 6.(*) Defining Types ----------------------------------------

-- | Defining the data type "Month"
data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Enum, Show)


-- | Function that checks if given year is a leap year
isItLeapYear :: Int -> Bool
isItLeapYear y = mod y 4 == 0


-- | Defines how many days in each month, given the year
daysInMonth :: Month -> Int -> Int
daysInMonth February y
    | mod y 4 == 0 = 29
    | otherwise = 28
daysInMonth April _ = 30
daysInMonth June _ = 30
daysInMonth September _ = 30
daysInMonth November _ = 30
daysInMonth _ _ = 31

-- | Defining the data type "Date" 
data Date = Date Int Int Int
instance Show Date where
    show (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d


-- | Checks if a given date is valid
validDate :: Date -> Bool
validDate (Date y m d)
    | d `elem` [1..(daysInMonth (toEnum (m-1) :: Month) y)] = True
    | otherwise = False



-- | Function that returns the next date
-- VERY UGLY CODE... Compare with Alex soultions...
tomorrow :: Date -> Date
tomorrow (Date y m d)
    | validDate (Date y m (d+1)) = Date y m (d+1)
    | m < fromEnum December + 1 = Date y (fromEnum (succ (toEnum (m-1) :: Month))+1) 1
    | otherwise = Date (y+1) 1 1 


-- Testdates --

-- Today
today :: Date
today = Date 2022 9 13

-- Unvalid date
unvalidDate :: Date
unvalidDate = Date 2022 9 31

-- Valid leap year date
leapYearValid :: Date
leapYearValid = Date 2020 2 29

-- Invalid leap year date
leapYearInvalid :: Date
leapYearInvalid = Date 2022 2 29



-- 7. Replication

repli :: Integer -> String -> String
repli 0 s = ""
repli n s = s ++ repli (n-1) s



-- 8. Multiplying list elements

multiply :: Num a => [a] -> a
multiply [] = 1
multiply (x:xs) = x * multiply xs


-- 9 (*) Avoiding Duplicates

duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x:xs) = x `elem` xs || duplicates xs


removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x:removeDuplicates xs

prop_duplicatesRemoved :: [Integer] -> Bool
prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))

-- Note: this test allows removeDuplicates to remove too much, e.g.
-- it would pass the test if it always returned an empty list. The test also
-- allows the function to add arbitrary values that were not in the
-- original list, but the polymorphic type of removeDuplicates prevents this.
-- (NB: THIS NOTE WAS COPY-PASTED FROM THE SOLUTIONS)

