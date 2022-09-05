{- 
Q1. What order?
Answer: 1. Parentheses
        2. Multiplication/Division
        3. Addition/subtraction
    E.g. 4*(8-5)+3
    --> 4*3+3
    --> 12+3
    --> 15

Q.2 What happens if the parantheses is removed?
Answer: 4*8-5+3
    --> 32-5+3
    --> 30

Q.3 What order are the operators applied in second expression? 
3.5/(3+4)*4.5
Answer: 
    --> 3.5/7*4.5
    --> 0.5*4.5
    --> 2.25

Q.4 What GHCi commands are there?
Answer: Try :?

----Two simple examples:----
Q.5. How many pounds?
Answer:
1000/12.775 = 78.2625709... 

Q.6 
0°C = 32°F
An increase of 5°C corresponds to increase of 9°F.
How many Farenheit is 28°C?

Solution:
If we calculate 5/9 we get what factor we can
muliply our Celisus value with to get Farenheit.

5/9 = 1.8
0°C + 28°C * 1.8 = 32°F + 50.4°F

Answer: 82.4°F
 -}

--- A programmable pocket calculator ---

pounds :: Double -> Double
pounds kr = kr/12.7775

celsToFar :: Double -> Double
celsToFar c = 32 + c*1.8


--- Comparison and more complex functions ---

price :: Double -> Double
price v
    | v <= 10 = 3.5*v
    | v <= 20 = 5+3*v
    | v > 20 = 15 + 2.5*v

{- 
Main> price (4+8)
41
Try to evaluate this without parentheses and explain the result.

Answer:
Main> price 4+8
22
Why do we get 22?
It's because Haskells priorities. It first calculates price 4 then adds 8.
> price 4+8
> (price 4) + 8
> 14 + 8
> 22
 -}

--- Functions with more than one argument ---

average2 :: Double -> Double -> Double
average2 x y = (x+y)/2

average3 :: Double -> Double -> Double -> Double
average3 x y z = (x+y+z)/3

{- 
--- Integers --- 

Q.1 What is 5 `mod` 0?
You get an error becuases division by zero is undefined

Q.2 Is 107139224 divisible by 11731?
No becauses mod 107139224 11731 returns 1.

Q.3 Define a function that given a year gives the number of days in that year.
 -}
daysInYear :: Integer -> Integer
daysInYear y
    | mod y 4 == 0 = 366
    | otherwise = 365



--- A bigger example ---
{- Think of an whole number greater than one. 
If its even, divide it by two, 
otherwise multiply it by three and add one. 
Stop if the resulting number is one, otherwise repeat the procedure. -}

-- Define the function next
next :: Integer -> Integer
next n
    | mod n 2 == 0 = div n 2
    | otherwise = 3*n+1

-- What is the length of the sequence for n = 6?
-- Answer: 9 (6,3,10,5,16,8,4,2,1)

steps :: Integer -> Integer
steps n
    | n == 1 = 1
    | otherwise = steps(next n)+1

numbers :: Integer -> [Integer]
numbers n
    | n == 1 = [1]
    | otherwise = n : numbers(next n)



-----------------------------------------------------------------------
--- Extra exercises week 1 ---