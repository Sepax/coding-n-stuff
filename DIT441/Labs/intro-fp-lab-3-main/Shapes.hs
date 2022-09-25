{- |
Module      : Shapes
Description : Types and functions for shapes. The list of all tetris pieces.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : Sebastian Pålsson, Tim Persson, Gustav Dalemo
Lab group   : 9
-}

module Shapes where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Test.QuickCheck

-- * Shapes

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

type Square = Maybe Colour

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.
type Row   = [Square]
data Shape = Shape { rows :: [Row] } deriving Eq

-- * Showing shapes
showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
 where
  showRow r = [showSquare s | s <- r]
  showSquare Nothing      = '.'
  showSquare (Just Black) = '#' -- can change to '█' on linux/mac
  showSquare (Just Grey)  = 'g' -- can change to '▓'
  showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [Shape (makeSquares s) | s <- shapes]
 where
   makeSquares = map (map colour)
   colour c    = lookup c [ ('I', Red),  ('J', Grey),  ('T', Blue), ('O', Yellow)
                          , ('Z', Cyan), ('L', Green), ('S', Purple) ]
   shapes = [["I",
              "I",
              "I",
              "I"],
             [" J",
              " J",
              "JJ"],
             [" T",
              "TT",
              " T"],
             ["OO",
              "OO"],
             [" Z",
              "ZZ",
              "Z "],
             ["LL",
              " L",
              " L"],
             ["S ",
              "SS",
              " S"]]


-- * Some simple functions

-- ** A1
emptyShape :: (Int, Int) -> Shape
emptyShape (w, h) = Shape (replicate h (replicate w Nothing))

-- ** A2

testShape = Shape[[Nothing, Just Grey]
                 ,[Nothing, Just Grey]
                 ,[Just Grey, Just Grey]]

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int, Int)
shapeSize (Shape []) = (0,0)
shapeSize s = (length (head (rows s)), length (rows s))

-- ** A3

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (Shape []) = 0
blockCount (Shape r) = length [x | x <- concat r, x /= Nothing]

-- * The Shape invariant

-- ** A4
-- | Shape invariant (shapes have at least one row, at least one column, and are rectangular)

prop_Shape :: Shape -> Bool
prop_Shape (Shape rows) = not (null rows) && eqLength rows
  where
    eqLength [] = True
    eqLength (r:rs)
      | length r * length (r:rs) == length (concat (r:rs)) = eqLength rs
      | otherwise = False


-- * Test data generators

-- ** A5
-- | A random generator for colours
genColour :: Gen Colour
genColour = elements [(minBound :: Colour) ..]

instance Arbitrary Colour where
  arbitrary = genColour

-- ** A6
-- | A random generator for shapes
genShape :: Gen Shape
genShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = genShape


-- * Transforming shapes

-- ** A7
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (Shape rows) = Shape (reverse (transpose rows))

-- ** A8
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (x, y) s = moveX x (moveY y s)

-- Moves a shape left or right depending on the value of "i" (Used in A8 & A9)
moveX :: Int -> Shape -> Shape
moveX _ (Shape []) = Shape []
moveX i (Shape (r:rs))
  | i >= 0 = Shape ( (replNothing ++ r) : recurMoveX )
  | otherwise = Shape ( (r ++ replNothing) : recurMoveX )
    where replNothing = replicate (abs i) Nothing
          recurMoveX = rows (moveX i (Shape rs))

{- ** Alternative function for moveX
moveX':: Int -> Shape -> Shape 
moveX' n s =  rotateShape(moveY n (tilt s))
  where
    tilt :: Shape -> Shape
    tilt ns = rotateShape(rotateShape(rotateShape(ns))) 
-}

-- Moves a shape up or down depending on the value of "i" (Used in A8 & A9)
moveY :: Int -> Shape -> Shape
moveY i (Shape r)
  | i > 0 = Shape (nothingRows ++ r)
  | otherwise = Shape (r ++ nothingRows)
    where
      nothingRows = rows (emptyShape(abs i, length (head r)))

-- ** A9
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int, Int) -> Shape -> Shape
padShape (x, y) s = moveX (-x) (moveY (-y) s)

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (x, y) s
  | x < init_x || y < init_y = s
  | otherwise = padShape (subtract init_x x, subtract init_y y) s
    where (init_x, init_y) = shapeSize s

-- * Comparing and combining shapes

-- ** B1

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = error "A11 overlaps undefined"

-- ** B2
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith = error "A12 zipShapeWith undefined"

-- ** B3
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = error "A13 zipShapeWith undefined"
