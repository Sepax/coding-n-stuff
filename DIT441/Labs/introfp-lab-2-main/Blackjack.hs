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

-- Task A1 

sizeSteps :: [Int]
sizeSteps = [ size aHand
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 1 + 1 + size []
            , 1 + 1 + 0
            , 2
            ]


