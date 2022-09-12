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
    | d `elem` [1..(daysInMonth (toEnum m :: Month) y)] = True
    | otherwise = False



{- -- | Extracts the year of a given date
year :: Date -> Int
year (Date y _ _) = y


-- | Extracts the month of a given date
month :: Date -> Int
month (Date _ m _) = m


-- | Extracts the day of a given date
day :: Date -> Int
day (Date _ _ d) = d -}


-- Testdates --

-- Today
today :: Date
today = Date 2022 9 12

-- Unvalid date
unvalidDate :: Date
unvalidDate = Date 2022 9 31

-- Valid leap year date
leapYearValid :: Date
leapYearValid = Date 2020 2 29

-- Invalid leap year date
leapYearInvalid :: Date
leapYearInvalid = Date 2022 2 29

