{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{- |
Module      : ConsoleGUI
Description : Run a game on a text console window with keyboard input and timer ticks
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
-}
 
module ConsoleGUI
  ( module GameInterface
  , runGame, runGameASCII
  ) where

import Control.Concurrent (Chan, newChan, writeChan, readChan, forkIO, killThread, threadDelay, yield)
import Control.Exception (bracket)
import Control.Monad (forever, when)
import Data.List (sort)
import Data.IORef
import System.Console.ANSI
import System.Directory (getAppUserDataDirectory)
import System.IO
import System.IO.Error (catchIOError)
import System.Random (randomRs,newStdGen)

import GameInterface
import Shapes (Shape, rows)

-- | Run a game in the terminal with colors.
-- The terminal is switched to unbuffered (raw) mode, echoing is turned off,
-- the screen is cleared, and the cursor is made invisible.
runGame :: Game state -> IO ()
runGame = runGame' render 
 where
  render (game, info) = 
    mapM_ renderRow (zip (rows game) (info ++ [""] ++ keys ++ repeat ""))

  renderRow (w, i) = mapM_ renderBlock w >> putStrLn (' ':i)

  renderBlock Nothing  = putStr "  "
  renderBlock (Just c) = do 
    setBgColor (fromEnum c)
    putStr "  "
    setNormal

-- | Run a game in the terminal with a simple ASCII character rendering,
-- otherwise like 'runGame'
runGameASCII :: Game state -> IO ()
runGameASCII = runGame' render
 where
  render (game,info) = 
    putStr (unlines (zipWith join ls (info ++ "":keys ++ repeat "")))
   where
    ls = lines (show game)
    join x y = x ++ " " ++ y

-- | runGame with more control over how the output from drawGame is rendered
runGame' :: ((Shape, [String]) -> IO a) -> Game state -> IO ()
runGame' render g = do 
  aChan <- newChan
  let book0 = Book { score = 0, rowCount = 0 }
  start_state <- (startGame g . randomRs (0, 1)) `fmap` newStdGen
  v <- newIORef (start_state, book0)
  bracket (setup aChan v) cleanup $ \ _ -> do 
    homeCursor
    clearScreen
    hi <- readHiscores
    (final_state, book) <- play v aChan False hi start_state book0
    hi' <- updateHiscores hi (score book)
    renderGame final_state book hi'
    putStrLn "Game over!"
 where
  setup aChan v = do 
    b <- hGetBuffering stdin
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    t1 <- forkIO $ readActions aChan
    t2 <- forkIO $ forever $ do 
      (t,b) <- readIORef v
      threadDelay (1000*tickDelay g t b)
      writeChan aChan (Right Tick)
    hideCursor
    return (b,t1,t2)

  cleanup (b,t1,t2) = do 
    showCursor
    killThread t1
    killThread t2
    hSetBuffering stdin b
    hSetEcho stdin True

  renderGame state book hi = do 
    homeCursor
    render (drawGame g state,gameInfo g state book++showHiscores hi)
    
  play v aChan paused hi state book = do 
    writeIORef v (state,book)
    renderGame state book hi
    action <- readChan aChan
    case action of
      Right a | not paused -> maybe (return (state, book)) continue (stepGame g a state)
       where continue (rows, state) = play v aChan paused hi state (updateBook rows book)
      Left Pause -> play v aChan (not paused) hi state book
      Left Quit  -> return (state,book)
      _          -> play v aChan paused hi state book

readActions :: Chan (Either OtherAction Action) -> IO ()
readActions aChan = do 
  a <- readAction
  writeChan aChan a
  when (a /= Left Quit) $ readActions aChan

readAction :: IO (Either OtherAction Action)
readAction = do 
  c <- getCh
  case c of
    '\n'-> return (Right MoveDown)
    ' ' -> return (Right MoveDown)
    'j' -> return (Right MoveLeft)
    'l' -> return (Right MoveRight)
    'k' -> return (Right Rotate)
    'p' -> return (Left Pause)
    'q' -> return (Left Quit)
    _   -> readAction

keys :: [String]
keys = [ "J = Left", "K = Rotate", "L = Right"
       , "P = Pause", "sp = Down", "Q = Quit" ]

data OtherAction = Pause | Quit deriving Eq

-- * Persistent hiscore list

updateHiscores :: [Int] -> Int -> IO [Int]
updateHiscores hs s = let hs' = take 10 (reverse (sort (s:hs))) in do 
  writeHiscores hs'
  return hs'

showHiscores :: [Int] -> [String]
showHiscores hs =
    "===TOP 10=== " : [ pad 2 i ++ ": " ++ pad 7 s 
                      | (i, s) <- zip [1..10] (hs ++ repeat 0) ]
 where
  pad w x = replicate (w - n) ' ' ++ s
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

-- * Cursor/Color Control functions

homeCursor :: IO ()
homeCursor  = setCursorPosition 0 0

setBgColor :: Int -> IO ()
setBgColor n = setSGR [SetColor Background vivid (toEnum (n `mod` 8))]
 where 
  vivid = if n < 8 then Vivid else Dull

setNormal :: IO ()
setNormal  = setSGR [Reset]

-- * Unbuffered console keyboard input for Win32

getCh :: IO Char
#ifdef mingw32_HOST_OS
-- A quick hack to get unbuffered keyboard input on Windows
foreign import ccall "conio.h _getch" getch :: IO Int
getCh = do 
  i <- getch
  if i `elem` [0,224]
    then do getch; yield; getCh -- skip function/arrow keys
    else return (toEnum i) 
#else
getCh = getChar
#endif
