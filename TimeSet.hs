{-# Language GADTs, NoMonomorphismRestriction #-}

module TimeSet (

  normalize

  ) where


import Data.Set   (fromAscList, toAscList, unions)
import Data.Maybe
import Control.Arrow ((>>>))


normalize :: (Ord b, Ord a, Num a) => [[(a, b)]] -> [(a, b)]
normalize = map absolute >>> norm >>> relative

norm :: (Eq x, Ord x, Eq y, Ord y) => [[(x,y)]] -> [(x,y)]
norm = toAscList . unions . map fromAscList

absolute = catMaybes . a
  where
    a :: Num a => [(a,b)] -> [Maybe (a,b)]
    a = scanl f Nothing

f Nothing       x    = Just x
f (Just (a,_)) (b,y) = Just (a+b, y)

relative :: Num a => [(a,b)] -> [(a,b)]
relative [] = []
relative l@(x:xs) = x : zipWith g l xs
  where
    g (a,_) (c,d) = (c-a, d)
