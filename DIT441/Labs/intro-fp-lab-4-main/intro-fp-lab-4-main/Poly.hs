{-# LANGUAGE CPP #-}
{- |
Module      : Poly
Description : Abstract type of polynomials (over Int)
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
-}

module Poly 
  ( -- * Type 
    Poly
    -- * Evaluating a polynomial
  , evalPoly 
    -- * Convert to and from a list of Int
  , toList, fromList
  ) where

import Data.Maybe
import Test.QuickCheck

-- | A data type for polynomials modelled as a list of ints, with the least
-- significant powers first. The data type has instances for 'Eq', 'Show', 
-- 'Arbitrary' and 'Num'.
data Poly = Poly [Int] deriving Eq

instance Show Poly where
  show (Poly ns) = showParts $ reverse [f | f <- zip ns powers, fst f /= 0]
   where 
     showParts []            = show 0
     showParts ((n, p):rest) = showNum n p ++ concatMap showRest rest

     showNum n ""   = show n
     showNum (-1) p = "-" ++ p -- only used for showing first one
     showNum 1 p    = p
     showNum n p    = show n ++ p

     showRest (n,p) | n < 0     = " - " ++ showNum (negate n) p
                    | otherwise = " + " ++ showNum n          p

#ifdef mingw32_HOST_OS
     powers = "" : "x" : ["x^" ++ show y    | y <- [2..]] 
#else
     powers = "" : "x" : ['x' : showSuper y | y <- [2..]]
  
showSuper :: Int -> String
showSuper = fromJust . mapM (flip lookup superScripts) . show
 where
  superScripts = zip ['0'..'9'] "⁰¹²³⁴⁵⁶⁷⁸⁹"
#endif

instance Arbitrary Poly where
  arbitrary = do
    l <- choose (1, 9) -- stick the nice looking powers
    ns <- vectorOf l arbitrary
    return $ fromList ns

-- | Convert a polynomial to a list representation @x² + 2x + 3@ would be 
-- represented by @[1,2,3]@
toList :: Poly -> [Int]
toList (Poly ns) = reverse ns

-- |Convert a list to a polynomial
fromList :: [Int] -> Poly
fromList = poly . reverse

-- A smart constructor for that removes leading zeros from little-endian list
poly :: [Int] -> Poly
poly = Poly . reverse . dropWhile (== 0) . reverse

-- Round trip property:
---
-- prop> fromList (toList p) == p
prop_Poly :: Poly -> Bool
prop_Poly p = fromList (toList p) == p

instance Num Poly where
  (+)    = addPoly
  (*)    = mulPoly
  negate = fromList . map negate . toList
  abs    = undefined
  signum = undefined
  fromInteger n = fromList [fromInteger n]

-- | Evaluate a polynomial at the given point
evalPoly :: Int -> Poly -> Int
evalPoly x (Poly cs) = sum $ zipWith (*) cs powersOfx 
 where 
  powersOfx = map (x^) [0..]          

-- Addition for polynomials
addPoly :: Poly -> Poly -> Poly
addPoly (Poly p1) (Poly p2) = poly $ zipWith (+) (pad l1 p1) (pad l2 p2)
 where 
  pad lp p = p ++ replicate (maxlen - lp) 0
  (l1,l2, maxlen) = (length p1, length p2, max l1 l2)

-- Multiplication for polynomials
mulPoly :: Poly -> Poly -> Poly
mulPoly (Poly p1) (Poly p2) = poly $ mul p1 p2 
 where
  mul [] p     = []
  mul (n:ns) p =  (n `times` p) `plus` timesX (ns `mul` p) 
  timesX  p    = 0:p
  times n p    = map (n*) p
  plus p1 p2   = reverse . toList $ Poly p1 + Poly p2

-- * Tests ---------------------------------------------------------------------

-- Check that eval is a homomorphism (and maintains the invariant)
prop_PolyOps :: Poly -> Poly -> Int -> Bool
prop_PolyOps p1 p2 x = evalHom (*) mulPoly && evalHom (+) addPoly
 where 
  evalHom f g = let p' = p1 `g` p2 in 
    evalPoly x p1 `f` evalPoly x p2 == evalPoly x p' && prop_Poly p'
                       
ex1    = fromList [1,2,3]
x      = fromList [1,0]
xPlus1 = fromList [1,1]