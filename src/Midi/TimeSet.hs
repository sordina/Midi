{-# Language GADTs, NoMonomorphismRestriction #-}

module Midi.TimeSet ( normalize ) where

import Data.Set (fromAscList, toAscList, unions)
import Control.Arrow ((>>>))

normalize :: (Ord b, Ord a, Num a) => [[(a, b)]] -> [(a, b)]
normalize = map absolute >>> norm >>> relative

norm :: (Eq x, Ord x, Eq y, Ord y) => [[(x,y)]] -> [(x,y)]
norm = toAscList . unions . map fromAscList

absolute = scanl1 f where f (a,_) (b,y) = (a+b, y)

relative :: Num a => [(a,b)] -> [(a,b)]
relative [] = []
relative l@(x:xs) = x : zipWith g l xs
  where
    g (a,_) (c,d) = (c-a, d)
