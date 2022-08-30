-- Problem 3
myProduct :: [Int] -> Int
myProduct [] = 0
myProduct (x:xs) = x * (product xs)

-- Problem 4
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller 
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x] 
                