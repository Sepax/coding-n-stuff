{- |
Module      : ThreepennyGUI
Description : Run a game using the Threepenny GUI
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

- You can open the game in your browser with the followin URL: http://127.0.0.1:8023
- More info about ThreepennyGUI: https://wiki.haskell.org/Threepenny-gui
-}
 
module ThreepennyGUI
 ( module GameInterface
 , runGame
 ) where

import Control.Monad (zipWithM_, when)
import Data.List (sort)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Canvas
import System.Directory (getAppUserDataDirectory)
import System.IO.Error (catchIOError)
import System.Random (randomRs, newStdGen)

import GameInterface
import Shapes (Shape, rows)

data State g = G 
  { game    :: Either g g
  , book    :: Book
  , hiscore :: [Int]
  }

-- | Run a game
runGame :: Game state -> IO ()
runGame g = startGUI defaultConfig (setup g)

setup :: Game g -> Window -> UI ()
setup g window = do 
  pure window # set title "Tetris with ThreepennyGUI"

  world0 <- startGame g . randomRs (0, 1) <$> liftIO newStdGen
  hi0    <- liftIO readHiscores

  let s0 = G (Right world0) (Book 0 0) hi0
        
  canvas <- UI.canvas # set UI.height (24 * k) # set UI.width (24 * k)

  left  <- UI.button #+ [string "←"]
  rot   <- UI.button #+ [string "↻"]
  right <- UI.button #+ [string "→"]
  down  <- UI.button #+ [string "↓"]

  body <- getBody window
  pure body #+ [ column [element canvas]
               , element left, element rot, element right, element down
               ]

  timer <- UI.timer

  let be a b  = const a <$> UI.click (getElement b)
      keys    = filterJust (readAction <$> UI.keydown body)
      actions = foldr1 (unionWith const) [ const Tick <$> UI.tick timer
                                         , be MoveLeft left
                                         , be Rotate rot
                                         , be MoveRight right
                                         , be MoveDown down
                                         , keys
                                         ]

  let setTiming G { game = Right st, book = b } = do 
        pure timer # set UI.interval (tickDelay g st b) >> return ()
      setTiming _ = UI.stop timer

  draw g canvas s0
  setTiming s0
  UI.start timer

  output <- accumE s0 (step g <$> actions)
  onEvent output $ \s -> do 
    draw g canvas s
    liftIO (saveHiscores s)
    setTiming s

  return ()

draw :: Game state -> Element -> State state -> UI ()
draw g c G { game = st, book = book, hiscore = hi } = either gameOver running st
 where
  running    = r ["", ""]
  gameOver   = r ["Game over!", ""]
  r extra st = render c (drawGame g st, gameInfo g st book ++ extra ++ top10)
  top10      = showHiscores hi

step :: Game state -> Action -> State state -> State state
step g a s@G { game = Left _ } = s
step g a s@G { game = Right st, book = b} = maybe end continue (stepGame g a st)
 where
  end = s { game = Left st, hiscore = addToHiscore (hiscore s) (score b) }
  continue (n, t) = s { game = Right t, book = updateBook n b }

saveHiscores :: State g -> IO ()
saveHiscores G { game = Left _, hiscore = hi } = do
  putStrLn ("updateHiscores " ++ show hi)
  updateHiscores hi
saveHiscores _ = return ()

readAction :: (Eq a, Num a) => a -> Maybe Action
readAction key = case key of
  37  -> Just MoveLeft   -- left arrow
  38  -> Just Rotate     -- up arrow
  39  -> Just MoveRight  -- right arrow
  40  -> Just MoveDown   -- down arrow
  32  -> Just MoveDown   -- space
  74  -> Just MoveLeft   -- J
  75  -> Just Rotate     -- K
  76  -> Just MoveRight  -- L
  106 -> Just MoveLeft   -- j
  107 -> Just Rotate     -- k
  108 -> Just MoveRight  -- l
  _   -> Nothing

render :: Canvas -> (Shape, [String]) -> UI ()
render canvas (game, info) = do
  pure canvas # set textFont (show k ++ "px monospace")
  clearCanvas canvas
  renderGame game
  renderInfo info
 where
  renderInfo = zipWithM_ renderMessage [2..]
  renderMessage y = text (14 * k, (k+2) * y)
  text p s = fillText s p canvas

  renderGame  = zipWithM_ renderRow [0..] . rows
  renderRow y = zipWithM_ (renderBlock y) [0..]

  renderBlock y x Nothing  = return ()
  renderBlock y x (Just c) = do
    pure canvas # set fillStyle (solidColor (colors !! i))
    fillRect (scale (x, y)) k k canvas
    strokeRect (scale (x, y)) k k canvas
   where
    i = fromEnum c
    scale (x, y) = (scale1 (x+1), scale1 (y+1))
    scale1 x = k * fromIntegral x

k :: Num a => a
k = 14

colors :: [Color]
colors = [ RGB l l l, RGB h l l, RGB l h l, RGB h h l
         , RGB l l h, RGB h l h, RGB l h h, RGB m m m ]
 where
  l = 0
  m = 192
  h = 240

strokeRect :: (Double, Double) -> Double -> Double -> Canvas -> UI ()
strokeRect (x, y) w h canvas = do
  beginPath canvas
  moveTo (x, y) canvas
  lineTo (x+w, y) canvas
  lineTo (x+w, y+h) canvas
  lineTo (x, y+h) canvas
  closePath canvas
  stroke canvas

-- * Persistent hiscore list
updateHiscores :: [Int] -> IO ()
updateHiscores hi = do
  old <- readHiscores
  when (hi /= old) (writeHiscores hi)

addToHiscore :: Ord a => [a] -> a -> [a]
addToHiscore hs s = take 10 . reverse . sort $ s:hs

showHiscores :: [Int] -> [String]
showHiscores hs =
  "===TOP 10=== " : [ pad 2 i ++ ": " ++ pad 7 s
                    | (i, s) <- zip [1..10] (hs ++ repeat 0) ]
 where
  pad w x = replicate (w-n) ' ' ++ s
   where
    s = show x
    n = length s

hiscorePath :: IO FilePath
hiscorePath = getAppUserDataDirectory "HsTetris"

readHiscores :: IO [Int]
readHiscores = (readIO =<< readFile =<< hiscorePath)
               `catchIOError` const (return [])

writeHiscores :: [Int] -> IO ()
writeHiscores hs = flip writeFile (show hs) =<< hiscorePath
