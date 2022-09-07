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
anAceHand = [cardOfAce, cardOf8, cardOf8]

-- TASK A1

{- size hand2
    = size (Card (Numeric 2) Hearts : (Card Jack Spades : [])
    = 1 + size (Card Jack Spades : [])
    = 1 + (1 + size [])
    = 1 + (1 + 0)
    = 1 + 1
    = 2 -}
 
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


-- /Shows the cards in a given hand.
display :: Hand -> String
display [] = ""
display [c] = displayCard c
display (c:h) = displayCard c ++ display h



-- Displays a given card in a more readable way.
displayCard :: Card -> String
displayCard (Card (Numeric i) Hearts) = " \9829 " ++ show(i) ++ "\n"
displayCard (Card (Numeric i) Spades) = " \9824 " ++ show(i) ++ "\n"
displayCard (Card (Numeric i) Diamonds) = " \9830 " ++ show(i) ++ "\n"
displayCard (Card (Numeric i) Clubs) = " \9827 " ++ show(i) ++ "\n"
displayCard (Card r Hearts) = " \9829 " ++ show(r) ++ "\n"
displayCard (Card r Spades) = " \9824 " ++ show(r) ++ "\n"
displayCard (Card r Diamonds) = " \9830 " ++ show(r) ++ "\n"
displayCard (Card r Clubs) = " \9827 " ++ show(r) ++ "\n"


-- TASK A3

-- Calulates the value of the given hand, considering Blackjack rules.
value :: Hand -> Int
value h
    | valueAcesAsTen h > 21 = valueAcesAsTen h - 9*numberOfAces h
    | otherwise = valueAcesAsTen h



-- Calculates the value of given hand with Aces having the value of 10.
valueAcesAsTen :: Hand -> Int
valueAcesAsTen [] = 0
valueAcesAsTen [c] = valueCard c
valueAcesAsTen (c:h) = valueCard c + valueAcesAsTen h

-- Defines a value for given rank.
valueRank :: Rank -> Int
valueRank (Numeric i) = i
valueRank r = 10

-- Defines a value for given card
valueCard :: Card -> Int
valueCard c = valueRank (rank c)

-- Calulates the number of aces in a hand.
numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces [c]
    | rank c == Ace = 1
    | otherwise = 0
numberOfAces (c:h)
    | rank c == Ace = 1 + numberOfAces h
    | otherwise = numberOfAces h


-- TASK A4

gameOver :: Hand -> Bool
gameOver h = value h > 21

winner :: Hand -> Hand -> Player
winner gh bh
    | (gameOver bh) && not (gameOver gh) = Guest
    | value gh > value bh && not (gameOver gh) = Guest
    | otherwise = Bank
