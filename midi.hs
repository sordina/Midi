{-# Language OverloadedStrings #-}

import Data.ByteString.Char8 ()
import Data.ByteString       (ByteString, pack, unpack)
import GHC.Word              (Word8)
import Numeric

showBin :: Int -> String
showBin = flip showsBin ""

showsBin :: Int -> ShowS
showsBin = showIntAtBase 2 bins

lead s = ls ++ s
 where
   ls = replicate (8-l) '0'
   l  = length s

showBins :: ByteString -> String
showBins = concatMap (lead . showBin) . map fromIntegral . unpack

bins 1 = '1'
bins 0 = '0'

bs :: Integer -> ByteString
bs = pack . reverse . fix . b 0x80

b :: Integer -> Integer -> [Integer]
b l n
 | d > 0     =  m : b l d
 | otherwise = [m]
 where (d,m) = divMod n l

fix :: [Integer] -> [Word8]
fix = map fromIntegral . zipWith (+) (0 : repeat 0x80)
