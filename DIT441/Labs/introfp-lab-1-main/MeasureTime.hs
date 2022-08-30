module MeasureTime
  ( measureTime
  , measureTime2
  ) where

import Data.Time
import Numeric

-- | Show a time difference using @n@ significant figures
showSignificant :: Int -> NominalDiffTime -> String
showSignificant n a = showFFloat Nothing b "s"
 where
   ae = showEFloat (Just (n-1)) (fromRational (toRational a)) ""
   b  = read ae :: Double

-- | Force the computation of a value
force :: Show a => a -> IO ()
force a = maximum (show a) `seq` return ()

-- | Measure and print the time it takes for the function to return
measureTime :: Show res => (arg -> res) -> arg -> IO ()
measureTime f arg = do
  putStrLn "Start evaluation"
  t1 <- getCurrentTime
  force (f arg)
  t2 <- getCurrentTime
  putStrLn $ "Done after " ++ showSignificant 2 (diffUTCTime t2 t1)

-- | Like @measureTime@ but for functions of two arguments
measureTime2 :: Show res => (a -> b -> res) -> a -> b -> IO ()
measureTime2 f a b = measureTime (uncurry f) (a, b)

-- Use as follows:
--
--     *Main> measureTime2 power 10 100000
--     Start evaluation
--     Done after 2.9s
