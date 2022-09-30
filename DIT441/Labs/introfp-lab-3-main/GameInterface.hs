{- |
Module      : GameInterface
Description : Common interface between the game and the various user interfaces modules
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
-}

module GameInterface where

import Shapes (Shape)

-- | Bookkeeping
data Book = Book { score, rowCount :: Int }

updateBook :: Int -> Book -> Book
updateBook rows (Book score rowCount) = Book score' (rowCount + rows)
 where
  score' = score + 100 * rows * rows

-- * User interface

-- | Interface to the game implementation
data Game state = Game 
  { startGame     :: [Double] -> state                      -- ^ Initial state
  , stepGame      :: Action -> state -> Maybe (Int, state)  -- ^ @Nothing@ ends the game, @Just@ continues. The @Int@ is the number of rows that were cleared.
  , drawGame      :: state -> Shape                         -- ^ Visualize the game state
  , gameInfo      :: state -> Book -> [String]              -- ^ Extra info to show to the user
  , tickDelay     :: state -> Book -> Int                   -- ^ Delay between @Tick@ steps
  , gameInvariant :: state -> Bool                          -- ^ for run-time verification
  }

-- | A type for the actions that advances the game (user input and timer ticks)
data Action = Tick | MoveLeft | MoveRight | MoveDown | Rotate
  deriving (Eq, Read, Show, Bounded, Enum)

-- | Extra info that the user interface may display
defaultGameInfo :: (state -> Bool) -> state -> Book -> [String]
defaultGameInfo invariant t b =
  [ show (score b)    ++ " points"
  , show (rowCount b) ++ " rows"
  , if invariant t then replicate (length warning) ' ' else warning
  ]
 where
  warning = "TETRIS INVARIANT VIOLATION"

-- | The delay between ticks (in milliseconds) as a function of current game state
defaultDelay :: state -> Book -> Int
defaultDelay _ b = round (500 * 0.8^(rowCount b `div` 10)) `max` 100
