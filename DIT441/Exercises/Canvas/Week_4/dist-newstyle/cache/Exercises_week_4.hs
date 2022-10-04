-- Exercises week 4!
import Prelude hiding (lookup)
import System.IO (hFlush, stdout)
import Data.List hiding (lookup)
import Data.Maybe ( isNothing , isJust)
import Test.QuickCheck

-- 0. Basic IO
    -- A.
sumOfNumbers :: IO () -- Tagen från facit
sumOfNumbers =
    do putStrLn "Compute the sum for some numbers."
       putStr "How many numbers? "
       n <- readLn
       let ask n = do putStr "Enter a number: "
                      readLn
       ns <- mapM ask [1..n]
       putStr "The sum of the numbers is "
       print (sum ns)

    -- B.
sortNumbersUntilZero :: IO [Int] -- Tagen från facit
sortNumbersUntilZero = do ns <- readNumbersUntilZero
                          return (sort ns)

readNumbersUntilZero :: IO [Int]
readNumbersUntilZero =
    do putStr "Enter a number (0 to end) "
       n <- readLn
       if n == 0 then return []
                 else do ns <- readNumbersUntilZero
                         return (n:ns)

    -- C.
repeat' :: IO Bool -> IO () -> IO () -- Gjord själv men omskriven m.h.a facit
repeat' test op =
    do op
       b <- test
       if b then return () else repeat' test op

-- 1. Properties of the lookup Function

lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup x [] = Nothing
lookup x ((x',y):xys)
    | x == x' = Just y
    | otherwise = lookup x xys

prop_LookNothing :: Eq a => a -> [(a,b)] -> Bool
prop_LookNothing x xys = isNothing (lookup x xys) && x `notElem` [x | (x,_) <- xys]

prop_LookJust :: Eq a => a -> [(a,b)] -> Bool
prop_LookJust x xys = isJust (lookup x xys) && not (null [x | (x,_) <- xys])

prop_Look :: Eq a => a -> [(a,b)] -> Bool
prop_Look x xys = prop_LookJust x xys && not (prop_LookNothing x xys) 

alphabet = zip (['a'..'z'] ++ ['å','ä','ö']) [1..]

-- 2.

prefixOf :: Eq a => [a] -> [a] -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x:xs) (y:ys) = x == y && prefixOf xs ys

prop_prefixOfSelf :: Int -> String -> Bool
prop_prefixOfSelf n s1 = take n s1 `prefixOf` s1

suffixOf :: Eq a => [a] -> [a] -> Bool
suffixOf [] _ = True
suffixOf _ [] = False
suffixOf xs ys = undefined