-- Exercises week 5

import Prelude hiding (length)
import Test.QuickCheck
import Data.Maybe (isJust, fromJust)
-- 1.

length :: [a] -> Int
length = sum . map (const 1)

iter :: Int -> (a -> a) -> a -> a
iter 0 f = id
iter n f = f . iter (n-1) f

-- What is the type of the foloowing function?
-- \n -> iter n succ
-- Answer: Enum a => Int -> a -> a

sumSq :: Int -> Int
sumSq n = foldr (+) 0 (map (^2) [1..n])

mystery xs = foldr (++) [] (map sing xs)
    where sing x = [x]

-- (id . f) is the same as f because the result of f is fed to id which outputs
-- the sam result.

-- 2. 
one xs = [ x+1 | x <- xs ]
one' = map (+1)

two xs ys = [ x+y | x <- xs, y <-ys ]
two'' xs ys =  concat (map (\x -> map (\y -> x+y) ys) xs) -- Tagen från facit

three xs = [ x+2 | x <- xs, x > 3 ]
three' = map (+2) . filter (>3)

four xys = [ x+3 | (x,_) <- xys ]
four' = map ((+3) . fst)
four'' = map (\(x,_) -> x+3)

five xys = [ x+4 | (x,y) <- xys, x+y < 5 ]
five' = map(+4) . map fst . filter (\(x,y) -> x+y < 5)

six mxs = [ x+5 | Just x <- mxs ]
six' = map (+5) . map fromJust . filter isJust
six'' = map (\(Just x) -> x+5) . filter isJust
