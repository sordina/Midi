module Midi.Util (

  every,
  drops

) where

import Data.List.Split

every :: Int -> [a] -> [a]
every n = map head . splitEvery n

drops :: [Int] -> [a] -> [a]
drops [] xs = xs
drops _ []  = []
drops (n:ns) xs@(x:_) = x : drops (ns) (drop n xs)
