module Blackjack where

import Cards
import RunGame

-- Test cards

cardOf2 :: Card
cardOf2 = Card (Numeric 2) Hearts

cardOf4 :: Card
cardOf4 = Card (Numeric 4) Hearts

cardOf8 :: Card
cardOf8 = Card (Numeric 8) Hearts

cardOfJack :: Card
cardOfJack = Card Jack Spades

cardOfAce :: Card
cardOfAce = Card Ace Hearts

-- Test hands

aHand :: Hand
aHand = [cardOf2, cardOfJack]

aGoodHand :: Hand
aGoodHand = [cardOf8, cardOf8, cardOf4]

aBadHand :: Hand
aBadHand = [cardOf4, cardOf2, cardOf2]

aBustHand :: Hand
aBustHand = [cardOfJack, cardOf8, cardOf8]

anAceHand :: Hand
anAceHand = [cardOfAce, cardOf8, cardOf8]

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

-- Calculates the value of a given hand


-- Calculates initial value of hand when Ace has the value 11.
preValue :: Hand -> Int
preValue [] = 0
preValue (card: cards) = valueRank (rank card) + preValue cards

-- Calculates value of hand with Ace case taken into consideration
value :: Hand -> Int
value [] = 0
value (card: cards)
    | ((valueRank (rank card)) + (preValue cards) > 21) && (rank card == Ace) = 1 + value cards --Handles Ace case 
    | otherwise = valueRank (rank card) + value cards

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


