{- |
Module      : Tetris
Description : The Tetris game (main module)
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
-}

module Main where

import ConsoleGUI
-- import ThreepennyGUI  -- either use ConsoleGUI or ThreepennyGUI

import Shapes
import Test.QuickCheck

--------------------------------------------------------------------------------
-- * The code that puts all the piece together
main :: IO ()
main = runGame tetrisGame

tetrisGame :: Game Tetris
tetrisGame = Game 
  { startGame     = startTetris
  , stepGame      = stepTetris
  , drawGame      = drawTetris
  , gameInfo      = defaultGameInfo prop_Tetris
  , tickDelay     = defaultDelay
  , gameInvariant = prop_Tetris 
  }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation

type Pos   = (Int, Int)

-- | The state of the game consists of three parts:
data Tetris = Tetris 
  { piece  :: (Pos, Shape)  -- ^ The position and shape of the falling piece
  , well   :: Shape         -- ^ The well (the playing field), where the falling pieces pile up
  , shapes :: [Shape]       -- ^ An infinite supply of random shapes
  }

-- | The size of the well
wellWidth, wellHeight :: Int
wellWidth  = 10
wellHeight = 20

wellSize :: (Int, Int)
wellSize   = (wellWidth, wellHeight)

-- | Starting position for falling pieces
startPosition :: Pos
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Pos addition
add :: Pos -> Pos -> Pos
(x1, y1) `add` (x2, y2) = (x1 + x2, y1 + y2)

-- | Move the falling piece into position
place :: (Pos, Shape) -> Shape
place (v, s) = shiftShape v s

-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris t = prop_Shape (snd(piece t)) && shapeSize (well t) == wellSize

-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls s = Shape(wallTopBot s: rows (padShapeWith wallSides s) ++ [wallTopBot s])
 where
  wallTopBot s = replicate (fst(shapeSize s)+2) (Just Black)
  wallSides r = [Just Black] ++ r ++ [Just Black]

padShapeWith :: (Row -> Row) -> Shape -> Shape
padShapeWith f (Shape rows) = Shape (padShapeWith' f rows)
  where
    padShapeWith' f [] = []
    padShapeWith' f (r:rs) = f r : padShapeWith' f rs


-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v, p) w _) = addWalls (combine (shiftShape v p) w) -- incomplete !!!

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, piece) well supply
 where
  well         = emptyShape wellSize
  piece:supply = repeat (allShapes !! 1) -- incomplete !!!

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris Tick = tick -- incomplete !!!

move :: Pos -> Tetris -> Tetris
move pos (Tetris (v,p) w s) = Tetris (add pos v,p)w s

tick :: Tetris -> Maybe (Int, Tetris)
tick t = Just (0,move (0,1) t)