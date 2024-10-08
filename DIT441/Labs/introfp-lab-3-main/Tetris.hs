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
startTetris (d:ds) = Tetris (startPosition, head $ supply (d:ds)) well (supply ds)
  where
    well = emptyShape wellSize
    supply (x:xs) = allShapes !! round (x * fromIntegral (length allShapes -1)) : supply xs

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris a t = case a of 
  MoveRight -> Just (0, movePiece 1 t)
  MoveLeft  -> Just (0, movePiece (-1) t)
  Rotate    -> Just (0, rotatePiece t)
  _         -> tick t

move :: Pos -> Tetris -> Tetris
move pos (Tetris (v,p) w s) = Tetris (v `add` pos,p) w s

tick :: Tetris -> Maybe (Int, Tetris)
tick t
  | collision $ tickedTetris t = dropNewPiece t
  | otherwise = Just (0,move (0,1) t)
  where
    tickedTetris t = move (0,1) t

collision :: Tetris -> Bool
collision (Tetris (v@(x,y), p) w s)
  | x < 0 = True
  | x + fst (shapeSize p) > wellWidth = True
  | y + snd (shapeSize p) > wellHeight = True
  | place (v,p) `overlaps` w = True
  | otherwise = False

movePiece :: Int -> Tetris -> Tetris
movePiece n t
  | collision (move (n,0) t) = t
  | otherwise = move (n,0) t

rotate :: Tetris -> Tetris
rotate (Tetris (v,p) w s) = Tetris (v, rotateShape p) w s

adjust :: Tetris -> Tetris
adjust t
  | collision (adjusted t) = move (-1,0) (adjusted t)
  | collision (rotate t) = adjusted t
  | otherwise = rotate t
  where
    adjusted t = move (-1,-1) (rotate t)

rotatePiece :: Tetris -> Tetris
rotatePiece t
  | collision (rotate t) = adjust t
  | otherwise = rotate t

dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece t@(Tetris ((x,y), p) w (piece:supply))
  | place ((x,y), p) `overlaps` w = Nothing
  | otherwise = Just (n, Tetris (startPosition, piece) s supply)
    where
    (n,s) = clearLines $ w `combine` place ((x,y), p)

clearLines :: Shape -> (Int, Shape)
clearLines (Shape rows) = (rowsCleared, padShape (0,- rowsCleared) (Shape (deleteFullRows rows)))
  where
    rowsCleared = length rows - length (deleteFullRows rows)
    deleteFullRows = filter (not . \r -> Nothing `notElem` r)



