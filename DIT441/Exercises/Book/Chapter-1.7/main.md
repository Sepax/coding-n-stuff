# 1.7 Exercises

## 1. Give another possible calculation for the result of double (double 2)

        double 4

## 2. Show that sum [x] = x for any number x

        sum [x]
        sum x:[]
        x + sum []
        x + 0
        x

## 3. Define a function product that produces the product of a list of numbers, and show using your definition that product [2,3,4] = 24

        myProduct :: [Int] -> Int
        myProduct [] = 0
        myProduct (x:xs) = x * (product xs)

## 4. How should the definition of the function qsort be modified so that it produces a reverse sorted version of a list?

        qsort [] = []
        qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller 
            where
                smaller = [a | a <- xs, a <= x]
                larger = [b | b <- xs, b > x] 

## 5. What would be the effect of replacing <= by < in the original definition of qsort? Hint: consider the example qsort [2,2,3,1,1]

Every instance of `a`, where `a` and  `x` is equal, will not be defined to the values `larger` or `smaller`. The function will therefore not take into account every value in the list.
