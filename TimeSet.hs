{-# Language GADTs #-}

module TimeSet (

  normalize

  ) where


import Data.Set hiding (map)


normalize :: (Eq x, Ord x, Eq y, Ord y) => [[(x,y)]] -> [(x,y)]
normalize = toAscList . unions . map fromList
