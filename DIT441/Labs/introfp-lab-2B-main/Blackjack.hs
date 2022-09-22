{-
Authors     : Sebastian Pålsson, Tim Persson, Gustav Dalemo
Lab group   : 9
-}
module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

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
sizeSteps =
  [ size hand2,
    size (Card (Numeric 2) Hearts : (Card Jack Spades : [])),
    1 + size (Card Jack Spades : []),
    1 + (1 + size []),
    1 + (1 + 0),
    1 + 1,
    2
  ]

-- TASK A2

-- Displays a Card as a String
displayCard :: Card -> String
displayCard c
  | rank c `elem` [Jack, Queen, King, Ace] =
    show (rank c) ++ " of "
      ++ show
        (suit c)
  | otherwise = show (valueRank (rank c)) ++ " of " ++ show (suit c)

-- Shows the cards in a given hand.
display :: Hand -> String
display [] = ""
display (c : h) = displayCard c ++ "\n" ++ display h

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
numberOfAces (c : h)
  | rank c == Ace = 1 + numberOfAces h
  | otherwise = numberOfAces h

-- Calculates the value of given hand with Aces having the value of 11.
baseValue :: Hand -> Int
baseValue [] = 0
baseValue (c : h) = valueCard c + baseValue h

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
  | value gh <= value bh && not (gameOver bh) = Bank
  | otherwise = Guest

-- TASK B1

-- All possible ranks
allRanks :: [Rank]
allRanks = [Numeric x | x <- [2 .. 10]] ++ [Jack, Queen, King, Ace]

-- All possible Suits
allSuits :: [Suit]
allSuits = [Hearts, Spades, Diamonds, Clubs]

-- Returns a full deck of 52 cards
fullDeck :: Deck
fullDeck = [Card r s | s <- allSuits, r <- allRanks]

-- Full deck test property
prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

-- TASK B2

-- Given a deck and a hand, draws one card from the deck and puts on the hand.
-- Returns both the deck and the hand (in that order)
draw :: Deck -> Hand -> (Deck, Hand)
draw [] _ = error "draw: The deck is empty."
draw (x : xs) h = (xs, x : h)

playBank :: Deck -> Hand
playBank d = snd (playBank' d [])

playBank' :: Deck -> Hand -> (Deck, Hand)
playBank' d bh
  | value bh < 16 = playBank' d' bh'
  | otherwise = (d, bh)
  where
    (d', bh') = draw d bh

-- TASK B3

-- Shuffles a deck of cards
shuffle :: [Double] -> Deck -> Deck
shuffle _ [] = []
shuffle (x : xs) d = c' : shuffle xs d'
  where
    (d', c') = takeCard (randomIndex (x : xs) d) d

-- Removes the card at index i and returns the modified deck and the card
takeCard :: Int -> Deck -> (Deck, Card)
takeCard _ [] = error "error: Empty deck!"
takeCard i d = (take i d ++ drop (1 + i) d, d !! i)

-- Selects a random index based on the length of the deck
randomIndex :: [Double] -> Deck -> Int
randomIndex (x : xs) d = round (x * fromIntegral (length d - 1))

-- Task B5 & B6

belongsTo :: Card -> Deck -> Bool
c `belongsTo` [] = False
c `belongsTo` (c' : cs) = c == c' || c `belongsTo` cs

prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle c d (Rand randomlist) =
  c `belongsTo` d == c `belongsTo` shuffle randomlist d

prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) d =
  length d == length (shuffle randomlist d)

implementation =
  Interface
    { iFullDeck = fullDeck,
      iValue = value,
      iDisplay = display,
      iGameOver = gameOver,
      iWinner = winner,
      iDraw = draw,
      iPlayBank = playBank,
      iShuffle = shuffle
    }

main :: IO ()
main = runGame implementation
