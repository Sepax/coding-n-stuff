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

cardOf10 :: Card
cardOf10 = Card (Numeric 10) Hearts

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
anAceHand = [cardOfAce, cardOf8]

anotherAceHand :: Hand
anotherAceHand = [cardOfAce, cardOfAce, cardOfAce]


-- TASK A1

hand2 = (Card (Numeric 2) Hearts : (Card Jack Spades : []))

sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 1 + (1 + size [])
            , 1 + (1 + 0)
            , 1 + 1
            , 2
            ]


-- TASK A2

-- Displays a Card as a String
displayCard :: Card -> String
displayCard c
    | rank c `elem` [Jack, Queen, King, Ace] =  show (rank c) ++ " of " ++ show (suit c)
    | otherwise = show (valueRank (rank c)) ++ " of " ++ show (suit c)

-- Shows the cards in a given hand.
display :: Hand -> String
display [] = ""
display (c:h) = displayCard c ++ "\n" ++ display h


-- TASK A3

-- Defines a value for given rank.

valueRank :: Rank -> Int
valueRank (Numeric i) = i
valueRank Ace = 11
valueRank _ = 10


-- Defines a value for given card
valueCard :: Card -> Int
valueCard (Card r s) = valueRank r


-- Calulates the number of aces in a hand.
numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (c:h)
    | rank c == Ace = 1 + numberOfAces h
    | otherwise = numberOfAces h


-- Calculates the value of given hand with Aces having the value of 11.
baseValue :: Hand -> Int
baseValue [] = 0
baseValue (c:h) = valueCard c + baseValue h


-- Calulates the value of the given hand, considering Blackjack rules.
value :: Hand -> Int
value h
    | baseValue h > 21 = baseValue h - 10 * numberOfAces h
    | otherwise = baseValue h


-- TASK A4

-- Checks if a given hand is bust.
gameOver :: Hand -> Bool
gameOver h = value h > 21


-- Checks which hand wins, the returns the owner of that hand, i.e the winner.
winner :: Hand -> Hand -> Player
winner gh bh
    | gameOver gh = Bank
    | value gh <= value bh = Bank
    | otherwise = Guest