module Blackjack where

import Cards
import RunGame

-- Test definitions

aCard1 :: Card
aCard1 = Card (Numeric 2) Hearts

aCard2 :: Card
aCard2 = Card Jack Spades

aHand :: Hand
aHand = [aCard1, aCard2]

-- TASK A1

-- Returns a list which contains multiples of the values 2. Proof that the equations in said list returns the same value.
sizeSteps :: [Int]
sizeSteps = [ size aHand
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 1 + 1 + size []
            , 1 + 1 + 0
            , 2
            ]

-- TASK A2

-- Finds and replaces string with another string in a list https://bluebones.net/2007/01/replace-in-haskell/
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)

-- Displays a Card as a String
displayCard :: Card -> String
displayCard c = replace (show (rank c) ++ " of " ++ show (suit c)) "Numeric " ""


-- Displays a Hand as a String
display :: Hand -> String
display [] = []
display (card:cards) = displayCard card ++ "\n" ++ display cards

-- TASK A3

-- Converts Numeric to Int
numToInt :: Rank -> Int
numToInt r = case r of
    Numeric x -> x

-- Calculates the value of a given hand
value :: Hand -> Int
value [] = 0
value (card: cards) = do
    if (rank card) `elem` [Jack, Queen, King, Ace]
        then 10 + value cards
    else numToInt (rank card) + value cards



