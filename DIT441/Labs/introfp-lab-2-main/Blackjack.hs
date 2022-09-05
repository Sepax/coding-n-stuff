module Blackjack where

import Cards
import RunGame

-- Test definitions

cardOf2 :: Card
cardOf2 = Card (Numeric 2) Hearts

cardOf4 :: Card
cardOf4 = Card (Numeric 4) Hearts

cardOf8 :: Card
cardOf8 = Card (Numeric 8) Hearts

cardOfJack :: Card
cardOfJack = Card Jack Spades

aHand :: Hand
aHand = [cardOf2, cardOfJack]

aGoodHand :: Hand
aGoodHand = [cardOf8, cardOf8, cardOf4]

aBadHand :: Hand
aBadHand = [cardOf4, cardOf2, cardOf2]

aBustHand :: Hand
aBustHand = [cardOfJack, cardOf8, cardOf8]

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

-- Calculates the value of a given rank.
valueRank :: Rank -> Int
valueRank (Numeric r) = r
valueRank r 
    | r == Ace = 11
    | otherwise = 10

-- Displays a Card as a String
displayCard :: Card -> String
displayCard c
    | rank c `elem` [Jack, Queen, King, Ace] =  show (rank c) ++ " of " ++ show (suit c)
    | otherwise = show (valueRank (rank c)) ++ " of " ++ show (suit c)


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

-- TASK A4

-- Checks if the player is bust
gameOver :: Hand -> Bool
gameOver h
    | value h > 21 = True
    | value h <= 21 = False

-- Checks if the bank or the player has won the game
winner :: Hand -> Hand -> Player
winner b p 
    | gameOver p == True = Bank
    | value p <= value b = Bank
    | value p > value b = Guest


