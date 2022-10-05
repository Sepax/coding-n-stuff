{- |
Module      : Tetris
Description : The Tetris game (main module)
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : Sebastian Pålsson, Tim Persson, Gustav Dalemo
Lab group   : 9
-}

module Main where

import ConsoleGUI
-- import ThreepennyGUI  -- either use ConsoleGUI or ThreepennyGUI

import Shapes

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
addWalls = wallThenRotate . wallThenRotate . wallThenRotate . wallThenRotate
  where
    wallThenRotate :: Shape -> Shape
    wallThenRotate s = rotateShape (Shape (replicate (fst $ shapeSize s) (Just Black) : rows s))

-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v, p) w _) = addWalls (combine w (place (v,p)))

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris (x:xs) = Tetris (startPosition, piece) well supply
 where
  well         = emptyShape wellSize
  piece:supply = repeat (allShapes !! round (x * fromIntegral (length allShapes -1)))

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris Tick = tick
stepTetris MoveDown = tick
stepTetris MoveLeft = \t -> Just (0,movePiece (-1) t)
stepTetris MoveRight = \t -> Just (0,movePiece 1 t)
stepTetris Rotate = \t -> Just (0, rotatePiece t)

move :: Pos -> Tetris -> Tetris
move pos (Tetris (v,p) w s) = Tetris (v `add` pos,p) w s

tick :: Tetris -> Maybe (Int, Tetris)
tick t
  | collision $ tickedTetris t = dropNewPiece t
  | otherwise = Just (0,move (0,1) t)
  where
    tickedTetris t = move (0,1) t

collision :: Tetris -> Bool
collision (Tetris ((x,y), p) w _)
  | x < 0 = True
  | x + fst(shapeSize p) > wellWidth = True
  | y + snd(shapeSize p) > wellHeight = True
  | place ((x,y),p) `overlaps` w = True
  | otherwise = False

collision' :: Tetris -> Bool
collision' (Tetris (v, p) w _) = place (v,p) `overlaps` w

movePiece :: Int -> Tetris -> Tetris
movePiece n (Tetris ((x,y), p) w s)
  | collision (move (n,0) (Tetris ((x,y), p) w s)) = Tetris ((x,y), p) w s
  | otherwise = move (n,0) (Tetris ((x,y), p) w s)

rotate :: Tetris -> Tetris
rotate (Tetris ((x,y), p) w s) = Tetris ((x,y), rotateShape p) w s

adjust :: Tetris -> Tetris
adjust t@(Tetris ((x,y), p) w s)
  | collision (rotate t) = move (-(snd (shapeSize p)-1),y) (rotate t)
  | otherwise = rotate t

rotatePiece :: Tetris -> Tetris
rotatePiece t
  | collision (rotate t) =  adjust t
  | otherwise = rotate t

dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece t@(Tetris ((x,y), p) w s)
  | place ((x,y), p) `overlaps` w = error "Game Over"
  | otherwise = Just (0, Tetris (startPosition, head s) (place ((x,y), p) `combine` w) (tail s))


w = addWalls (emptyShape wellSize)
w1 =  combine w (place ((5,18), allShapes !! 1))
w2 = combine w1 (place ((5,15), allShapes !! 1))

