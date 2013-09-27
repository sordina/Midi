module Midi.Util (

  every,
  drops

) where

every :: Int -> [a] -> [a]
every n | n > 0 = go
        | otherwise = error ("every called with invalid spacing: " ++ show n)
        where
  go []       = []
  go xs@(x:_) = x : go (drop n xs)

-- Passed:
--
-- prop_equiv :: Int -> [Int] -> Bool
-- prop_equiv n xs = every2 n' xs == every n' xs where n' = abs n + 1
--
-- import Data.List.Split
--
-- every2 :: Int -> [a] -> [a]
-- every2 n = map head . splitEvery n

drops :: [Int] -> [a] -> [a]
drops [] xs = xs
drops _ []  = []
drops (n:ns) xs@(x:_) = x : drops (ns) (drop n xs)
